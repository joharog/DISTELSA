  METHOD sd_salesdocument_create.

*input-pedido-ZCOD_SELLER     "Falta en estructura input - se utiliza de zsdt_pedidos_3p
*input-pedido-ZFECHAI         "Falta en estructura input - se utiliza de zsdt_pedidos_3p
*input-pedido-ZFECHAF         "Falta en estructura input - se utiliza de zsdt_pedidos_3p
*input-pedido-ZREF_COD_MAT    "Falta en estructura input - se utiliza de zsdt_material_3p
*input-pedido-ZMONTO          "Falta en estructura input - se utiliza de items-conditions
*input-pedido-ztipo_doc.      "Existe

    CONSTANTS: lc_objtype TYPE bapiusw01-objtype VALUE 'BUS2094'.

    DATA: lv_codseller TYPE zsde_cod_seller,
          lv_fecha1    TYPE char10,
          lv_fecha2    TYPE char10,
          lv_fini      TYPE datum,
          lv_ffin      TYPE datum,
          lv_posnr     TYPE posnr_va,
          lv_salesdoc  TYPE bapivbeln-vbeln,
          lv_billdoc   TYPE bill_doc.

    DATA: ls_header        TYPE bapisdhd1,
          ls_headerx       TYPE bapisdhd1x,
          lt_partners      TYPE TABLE OF bapiparnr,
          lt_items         TYPE TABLE OF bapisditm,
          lt_itemsx        TYPE TABLE OF bapisditmx,
          lt_conditions    TYPE TABLE OF bapicond,
          lt_conditionsx   TYPE TABLE OF bapicondx,
          lt_return        TYPE TABLE OF bapiret2,
          lw_pedidos       TYPE zsdt_pedidos_3p,
          lt_billingdatain TYPE TABLE OF bapivbrk,
          lt_success_bill  TYPE TABLE OF bapivbrksuccess,
          lt_return_bill   TYPE TABLE OF bapiret1.


    SPLIT input-pedido-referencia AT ';' INTO lv_codseller
                                              lv_fecha1
                                              lv_fecha2.

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_fecha1
      IMPORTING
        date_internal            = lv_fini
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_fecha2
      IMPORTING
        date_internal            = lv_ffin
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.

    SELECT SINGLE * FROM zsdt_pedidos_3p INTO @DATA(ls_pedidos)
      WHERE zcod_seller = @lv_codseller
        AND zperiodoi = @lv_fini
        AND zperiodof = @lv_ffin
        AND zestatus IN ( 'P', 'F' ).
    IF sy-subrc NE 0.
*     Periodo de facturacion no procesado
      SELECT SINGLE * FROM zsdt_general_3p INTO @DATA(ls_data_general)
        WHERE zcod_seller = @lv_codseller.

*     Tomar ultimo registros para doc.referencia
      SELECT * FROM vbrk INTO @DATA(ls_vbrk)
        WHERE fkart EQ @ls_data_general-auart
          AND vbtyp EQ 'M'
          AND vkorg EQ @ls_data_general-vkorg
          AND vtweg EQ @ls_data_general-vtweg
          AND spart EQ @ls_data_general-spart
          AND kunrg EQ @ls_data_general-kunrg
        ORDER BY vbeln ASCENDING. "DESCENDING.
*        UP TO 1 ROWS.
      ENDSELECT.

*        IF sy-subrc EQ 0.
      SELECT SINGLE * FROM zdistelsa_mon_cds_01 INTO @DATA(ls_distsd)
        WHERE vbeln EQ @ls_vbrk-vbeln
          AND stlog EQ 'F'. "Finalizado  con exito.

      ls_header = VALUE #( doc_type   = ls_data_general-zclase_nc
                           sales_org  = ls_data_general-vkorg
                           distr_chan = ls_data_general-vtweg
                           division   = ls_data_general-spart
                           ref_doc    = ls_vbrk-vbeln
                           purch_no_c = ls_header-ref_doc
                           purch_no_s = ls_distsd-yuserstr03
                           ref_1_s    = ls_distsd-yuserstr02
*                           ref_doc_l  = ls_distsd-yuserstr03
*                           ass_number = ls_distsd-yuserstr02
                           refdoc_cat = 'M').

      REFRESH: lt_partners, lt_items, lt_itemsx, lt_conditions, lt_conditionsx.
      LOOP AT input-pedido-items ASSIGNING FIELD-SYMBOL(<fs_items>).

        lv_posnr += '10'.

        SELECT SINGLE * FROM zsdt_material_3p INTO @DATA(ls_material)
          WHERE zref_cod_mat = @<fs_items>-material.


*            LOOP AT <fs_items>-partner ASSIGNING FIELD-SYMBOL(<fs_partner>).
        APPEND VALUE #( partn_role = ls_data_general-parvw "<fs_partner>-interlocutor
                        partn_numb = ls_data_general-kunrg ) "<fs_partner>-cliente )
                        TO lt_partners.
*            ENDLOOP.

        APPEND VALUE #( itm_number = lv_posnr
                        material   = ls_material-matnr
                        target_qty = '1'
                        ref_doc    = ls_header-ref_doc
                        ref_doc_ca = 'M')
                        TO lt_items.

        APPEND VALUE #( itm_number = lv_posnr
                        material   = abap_true
                        target_qty = abap_true
                        ref_doc    = abap_true
                        ref_doc_ca = abap_true )
                        TO lt_itemsx.

        LOOP AT <fs_items>-conditions ASSIGNING FIELD-SYMBOL(<fs_conditions>) WHERE posicion EQ lv_posnr.
          APPEND VALUE #( itm_number = lv_posnr
                          cond_type  = ls_data_general-kschl "<fs_conditions>-cond_precio
                          cond_value = <fs_conditions>-monto
                          currency   = 'GTQ')
                          TO lt_conditions.

          APPEND VALUE #( itm_number = lv_posnr
                          cond_type  = abap_true
                          cond_value = abap_true
                          currency   = abap_true )
                          TO lt_conditionsx.
        ENDLOOP.

*            UNASSIGN: <fs_partner>, <fs_conditions>.

      ENDLOOP.

      CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
        EXPORTING
          sales_header_in      = ls_header
          sales_header_inx     = ls_headerx
          business_object      = lc_objtype
*         TESTRUN              =
        IMPORTING
          salesdocument_ex     = lv_salesdoc
        TABLES
          return               = lt_return
          sales_items_in       = lt_items
          sales_items_inx      = lt_itemsx
          sales_partners       = lt_partners
          sales_conditions_in  = lt_conditions
          sales_conditions_inx = lt_conditionsx.

      IF lt_return[] IS NOT INITIAL.
        APPEND INITIAL LINE TO output-pedido_response-table ASSIGNING FIELD-SYMBOL(<fs_output>).
        <fs_output>-doc_number = lv_salesdoc.

        LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
          APPEND INITIAL LINE TO <fs_output>-messages ASSIGNING FIELD-SYMBOL(<fs_messages>).
          <fs_messages>-type    = <fs_return>-type.
          <fs_messages>-message = <fs_return>-message.
        ENDLOOP.
      ENDIF.

      IF lv_salesdoc IS NOT INITIAL AND ls_vbrk-vbeln IS NOT INITIAL.
        lw_pedidos = VALUE #( zcod_seller    = lv_codseller
                              kunrg          = ls_data_general-kunrg
                              zperiodoi      = lv_fini
                              zperiodof      = lv_ffin
                              vbeln          = lv_salesdoc
                              zestatus       = 'P'
                              fecha_creacion = sy-datum
                              hora_creacion  = sy-uzeit
                              usuario        = sy-uname ).

        INSERT zsdt_pedidos_3p FROM lw_pedidos.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

*       Validar existencia de doc referencia y con status F
        IF ls_vbrk-vbeln IS NOT INITIAL AND ls_distsd IS NOT INITIAL.

          APPEND VALUE #( ref_doc     = lv_salesdoc
                          ref_doc_ca  = 'K' )
                          TO lt_billingdatain.

          CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
            TABLES
              billingdatain = lt_billingdatain
              return        = lt_return_bill
              success       = lt_success_bill.

          READ TABLE lt_success_bill ASSIGNING FIELD-SYMBOL(<fs_success_bill>) INDEX 1.
          IF sy-subrc = 0.
            lv_billdoc = <fs_success_bill>-bill_doc.

            lw_pedidos-nro_fact = lv_billdoc.
            lw_pedidos-zestatus = 'F'.
            MODIFY zsdt_pedidos_3p FROM lw_pedidos.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.

          IF lt_return_bill[] IS NOT INITIAL.
            APPEND INITIAL LINE TO output-pedido_response-table ASSIGNING <fs_output>.
            <fs_output>-doc_number = lv_billdoc.
            LOOP AT lt_return_bill ASSIGNING FIELD-SYMBOL(<fs_return_bill>).
              APPEND INITIAL LINE TO <fs_output>-messages ASSIGNING <fs_messages>.
              <fs_messages>-type = <fs_return_bill>-type.
              <fs_messages>-message = <fs_return_bill>-message.
            ENDLOOP.

          ENDIF.

        ELSE.
          APPEND INITIAL LINE TO output-pedido_response-table ASSIGNING <fs_output>.
          <fs_output>-doc_number = lv_salesdoc.
          APPEND INITIAL LINE TO <fs_output>-messages ASSIGNING <fs_messages>.
          <fs_messages>-type = 'E'.
          <fs_messages>-message = 'Factura Referencia no encontrada'.
        ENDIF.

      ENDIF.

    ELSE.
*       Periodo de facturacion ya procesado
      APPEND INITIAL LINE TO output-pedido_response-table ASSIGNING <fs_output>.
      <fs_output>-doc_number = ls_pedidos-nro_fact.
      APPEND INITIAL LINE TO <fs_output>-messages ASSIGNING <fs_messages>.
      <fs_messages>-type = 'E'.
      <fs_messages>-message = 'Periodo de facturación ya ejecutado procesado'.
    ENDIF.
  ENDMETHOD.
