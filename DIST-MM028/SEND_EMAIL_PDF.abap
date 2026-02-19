*----------------------------------------------------------------------*
***INCLUDE ZSAPZWE3.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form send_email_pdf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM send_email_pdf USING ps_docparams TYPE sfpdocparams
                          ps_mblnr     TYPE mblnr
                          ps_details   TYPE tty_we03_details
                          lv_fm_name   TYPE rs38l_fnam.

  DATA: lo_cx_bcx         TYPE REF TO cx_bcs.
  DATA: lt_binary_content TYPE solix_tab,
        lt_text           TYPE bcsy_text.
  DATA: ls_formoutput     TYPE fpformoutput.
  DATA: lv_subject  TYPE so_obj_des,
        lv_isubject TYPE string,
        lv_att_sbj  TYPE sood-objdes.

*    IF sy-uname EQ 'MQA_ABAP3' OR sy-uname EQ 'GMOLINA'.

  READ TABLE ps_details INTO DATA(ls_details) INDEX 1.
  IF sy-subrc EQ 0.
    DATA(lv_bsart) = ls_details-ekko-bsart.
  ENDIF.

  CASE lv_bsart.
    WHEN 'ZCAD' OR 'ZPUB' OR 'ZSER' OR 'ZCCO'.

*       LLamar formulario para obtener OTF
      CALL FUNCTION lv_fm_name
        EXPORTING
          /1bcdwb/docparams  = ps_docparams
          mat_nr             = ps_mblnr
          we03_details       = ps_details
        IMPORTING
          /1bcdwb/formoutput = ls_formoutput
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      TRY.
*           Buscamos sujeto para correo
          SELECT SINGLE * FROM t001 INTO @DATA(ls_t001)
            WHERE bukrs EQ @ls_details-ekko-bukrs.

          SELECT SINGLE * FROM lfa1 INTO @DATA(ls_lfa1)
            WHERE lifnr EQ @ls_details-ekko-lifnr.

*           Mapeamos el nombre del sujeto
          lv_subject = lv_isubject = |{ ls_t001-butxt }, Entrada de Mercancía { ps_mblnr }, { ls_lfa1-name1 }|. "| { lv_butxt } | && 'Entrada de Mercancía:' && | { lv_ebeln } | && | { lv_name1 } |.

*           Crear cuerpo del correo
          APPEND '<b>Estimado proveedor:</b><br><br>' TO lt_text.
          APPEND 'Adjunto encontrará copia de la entrada de mercancías, favor revisarla, ya que debe coincidir con los productos entregados o servicios brindados para su trámite de pago.<br><br>' TO lt_text.
          APPEND 'Atentamente,<br><br>' TO lt_text.
          APPEND '<b>Grupo Distelsa</b>' TO lt_text.


*           Creamos el objeto documento (Cuerpo)
          DATA(lo_document) = cl_document_bcs=>create_document(
                                i_type    = 'HTM' " O 'HTM' si usas HTML
                                i_text    = lt_text
                                i_subject = lv_subject ).

*           Mapeamos el nombre del adjunto
          lv_att_sbj = |Entrada de Mercancía { ps_mblnr }|. "lv_subject.

*           Convertimos XSTRING a tabla binaria (solix_tab)
          lt_binary_content = cl_bcs_convert=>xstring_to_solix( iv_xstring = ls_formoutput-pdf ).

*           Agregamos el adjunto al objeto lo_document
          lo_document->add_attachment(
            i_attachment_type    = 'PDF'
            i_attachment_subject = lv_att_sbj " Nombre del archivo
            i_att_content_hex    = lt_binary_content ).

          DATA(lo_send_request) = cl_bcs=>create_persistent( ).

*           IMPORTANTE: Seteamos el documento (que ya incluye el cuerpo y el adjunto)
          lo_send_request->set_document( lo_document ).

          lo_send_request->set_message_subject( ip_subject = lv_isubject ).

*           Buscamos destinario COMPRAS
          SELECT * FROM adr6 INTO TABLE @DATA(lt_adr6)
            WHERE addrnumber EQ @ls_lfa1-adrnr.
          IF sy-subrc EQ 0.
            SELECT * FROM adrt INTO TABLE @DATA(lt_adrt)
              FOR ALL ENTRIES IN @lt_adr6
              WHERE addrnumber EQ @lt_adr6-addrnumber
                AND consnumber EQ @lt_adr6-consnumber
                AND remark EQ 'COMPRAS'.
          ENDIF.

*        Agregar destinatarios
          IF lv_bsart NE 'ZCCO'.
            LOOP AT lt_adr6 ASSIGNING FIELD-SYMBOL(<fs_adr6>).
              READ TABLE lt_adrt TRANSPORTING NO FIELDS WITH KEY addrnumber = <fs_adr6>-addrnumber consnumber = <fs_adr6>-consnumber.
              IF sy-subrc EQ 0.
                TRY.
*                 Creamos el objeto destinatario para cada correo encontrado
                    DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( <fs_adr6>-smtp_addr ).

*                 Agregamos el destinatario a la solicitud de envío
                    lo_send_request->add_recipient(
                      i_recipient = lo_recipient
                      i_express   = 'X').

                    DATA(lv_destinatario_ok) = abap_true.

                  CATCH cx_address_bcs.
*                  Manejo de error si el formato del correo es inválido
                    CONTINUE.
                ENDTRY.
              ENDIF.
            ENDLOOP.

*         Busqueda de destinatrio si no encuentra
            IF lv_destinatario_ok = abap_false.
              REFRESH: lt_adr6.
              UNASSIGN <fs_adr6>.
              SELECT * FROM adr6 INTO TABLE lt_adr6
                WHERE addrnumber EQ ls_lfa1-adrnr.
              IF sy-subrc EQ 0.
                LOOP AT lt_adr6 ASSIGNING <fs_adr6>.
                  TRY.
*                 Creamos el objeto destinatario para cada correo encontrado
                      lo_recipient = cl_cam_address_bcs=>create_internet_address( <fs_adr6>-smtp_addr ).

*                 Agregamos el destinatario a la solicitud de envío
                      lo_send_request->add_recipient(
                        i_recipient = lo_recipient
                        i_express   = 'X').

                    CATCH cx_address_bcs.
*                  Manejo de error si el formato del correo es inválido
                      CONTINUE.
                  ENDTRY.
                ENDLOOP.
              ENDIF.
            ENDIF.

*         Después de crear lo_send_request
            DATA(lo_sender) = cl_sapuser_bcs=>create( sy-uname ). "El usuario que contabiliza
            lo_send_request->set_sender( lo_sender ).

*         Enviar inmediatamente
            lo_send_request->set_send_immediately( 'X' ).

*         Enviar
            lo_send_request->send( i_with_error_screen = 'X' ).

          ELSE.
*         Busca correo proveedor solo para clase ZCCO
            SELECT * FROM zmm_email_zcco INTO TABLE @DATA(lt_email)
              WHERE supplier EQ @ls_details-ekko-lifnr
                AND ekgrp    EQ @ls_details-ekko-ekgrp.

            LOOP AT lt_email INTO DATA(ls_mail).
              TRY.
*               Creamos el objeto destinatario para cada correo encontrado
                  lo_recipient = cl_cam_address_bcs=>create_internet_address( ls_mail-smtp_addr ).

*               Agregamos el destinatario a la solicitud de envío
                  lo_send_request->add_recipient(
                    i_recipient = lo_recipient
                    i_express   = 'X').

                  lv_destinatario_ok = abap_true.

                CATCH cx_address_bcs.
*                Manejo de error si el formato del correo es inválido
                  CONTINUE.
              ENDTRY.
            ENDLOOP.

*         Busqueda de destinatrio si no encuentra en tabla ZMM_EMAIL_ZCCO
            IF lv_destinatario_ok = abap_false.
              REFRESH: lt_adr6.
              UNASSIGN <fs_adr6>.
              SELECT * FROM adr6 INTO TABLE lt_adr6
                WHERE addrnumber EQ ls_lfa1-adrnr.
              IF sy-subrc EQ 0.
                LOOP AT lt_adr6 ASSIGNING <fs_adr6>.
                  TRY.
*                 Creamos el objeto destinatario para cada correo encontrado
                      lo_recipient = cl_cam_address_bcs=>create_internet_address( <fs_adr6>-smtp_addr ).

*                 Agregamos el destinatario a la solicitud de envío
                      lo_send_request->add_recipient(
                        i_recipient = lo_recipient
                        i_express   = 'X').

                    CATCH cx_address_bcs.
*                  Manejo de error si el formato del correo es inválido
                      CONTINUE.
                  ENDTRY.
                ENDLOOP.
              ENDIF.
            ENDIF.

*         Después de crear lo_send_request
            lo_sender = cl_sapuser_bcs=>create( sy-uname ). "El usuario que contabiliza
            lo_send_request->set_sender( lo_sender ).

*         Enviar inmediatamente
            lo_send_request->set_send_immediately( 'X' ).

*         Enviar
            lo_send_request->send( i_with_error_screen = 'X' ).
          ENDIF.

        CATCH cx_bcs INTO lo_cx_bcx.
*              Appropriate Exception Handling
*              WRITE: 'Exception:', lo_cx_bcx->error_type.
      ENDTRY.
*      ENDCASE.
  ENDCASE.
*    ENDIF.

ENDFORM.
