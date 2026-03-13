*&---------------------------------------------------------------------*
*& Include          ZMMR046_F1
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CARGAR_ARCHIVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM cargar_archivo.

*--- declaraciones locales
  DATA: lv_filename      TYPE string,
        lt_records       TYPE solix_tab,
        lv_headerxstring TYPE xstring,
        lv_filelength    TYPE i.

  lv_filename = p_file.

*--- se cargan los datos de ruta en binarios
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_filelength
      header                  = lv_headerxstring
    TABLES
      data_tab                = lt_records
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

*--- se convierte datos binarios a xstring
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_filelength
    IMPORTING
      buffer       = lv_headerxstring
    TABLES
      binary_tab   = lt_records
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    "Implement suitable error handling here
  ENDIF.

*--- se crea el objeto referenciado a la clase para
*--- crear el documento excel
  DATA: lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet.

*--- se llama al metodo para crear el documento excel
*--- con ruta y archivo y la trama de datos en xstring
  TRY.
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
        document_name = lv_filename
        xdocument     = lv_headerxstring ).
    CATCH cx_fdt_excel_core.
      "Implement suitable error handling here
  ENDTRY .

*--- obtengo la lista de las hojas del excel o Worksheets
  lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheets) ).

  IF NOT lt_worksheets IS INITIAL.
    READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

*--- con el nombre de la hoja del documento excel se leen
*--- los datos en tabla
    DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
      lv_woksheetname ).

*--- se asignan los datos del worksheet/hoja en la tabla interna dinamica
    ASSIGN lo_data_ref->* TO <gt_data>.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SOLPED_CRT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_solped_crt.

  PERFORM f_init.
  LOOP AT <gt_table> INTO <fs_wa>.

*--- se llenan las posiciones
*--- posición
    ASSIGN COMPONENT 'BNFPO' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-preq_item = <fs_fld>.
    lv_pos = ls_pritem-preq_item.

*--- se toma en cuenta el salto de pedido en pedido por la posiciones
    IF sy-tabix NE 1 AND lv_pos EQ '1'.
      IF gt_prio_urg[] IS NOT INITIAL.
        EXPORT gt_prio_urg TO MEMORY ID 'ZGT_PRIO_URG_DT'.
      ENDIF.
      PERFORM f_pr_crt.
      PERFORM f_init.

      ls_pritem-preq_item = lv_pos.

    ENDIF.

*--- paso los registros que voy procesando a la tabla con la estructura de la salida
*--- que luego si pasa bien se actualice el documento de la solped
    MOVE-CORRESPONDING <fs_wa> TO gs_table.
    APPEND gs_table TO gt_table_prcd.

*--- bsart | Cabecera PRHEADER
    ASSIGN COMPONENT 'BSART' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_prheader-pr_type = <fs_fld>.

    ls_prheaderx = VALUE #( BASE ls_prheaderx
      pr_type = COND #( WHEN ls_prheader-pr_type IS NOT INITIAL THEN abap_true ) ).


    CASE ls_prheader-pr_type.
      WHEN 'ZSER'. "Servicio

*--- grupo tipo de producto
        ls_pritem-producttype = '2'.

*---fecha de inicio para el período de prestación
        ls_pritem-startdate = sy-datum.

*---Fecha de fin para el período de prestación
        ls_pritem-enddate = sy-datum + 30.


      WHEN 'ZSBA'. "Material

*--- grupo tipo de producto
        ls_pritem-producttype = ''.

*--- fecha de entrega
        ASSIGN COMPONENT 'LFDAT' OF STRUCTURE <fs_wa> TO <fs_fld>.
        ls_pritem-deliv_date = <fs_fld>.

    ENDCASE.

*--- Indicador de entrada de mercancías
    ls_pritem-gr_ind = abap_true.

*--- Indicador de recepción de factura
    ls_pritem-ir_ind = abap_true.

*--- grupo de compras
    ASSIGN COMPONENT 'EKGRP' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-pur_group = <fs_fld>.

*--- nombre del solicitante
    ASSIGN COMPONENT 'AFNAM' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-preq_name = <fs_fld>.

*--- material
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-material = |{ <fs_fld> ALPHA = IN }|.

*--- centro logístico
    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-plant = |{ <fs_fld> ALPHA = IN }|.

*--- almacen
    ASSIGN COMPONENT 'LGORT' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-store_loc = <fs_fld>.

*--- Nro de la necesidad
    ASSIGN COMPONENT 'BEDNR' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-trackingno = <fs_fld>.

*--- centro suministrador
    ASSIGN COMPONENT 'RESWK' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-suppl_plnt = <fs_fld>.

*--- cantidad
    ASSIGN COMPONENT 'MENGE' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-quantity = <fs_fld>.

*--- fecha solicitud
    ASSIGN COMPONENT 'BADAT' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-preq_date = COND #( WHEN <fs_fld> IS NOT INITIAL THEN <fs_fld> ELSE sy-datum ).

*--- fecha de liberación de la solicitud de pedido
    ls_pritem-rel_date = sy-datum.

*--- tipo posición
    ASSIGN COMPONENT 'PSTYP' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-item_cat = <fs_fld>.

*--- tipo imputación
    ASSIGN COMPONENT 'KNTTP' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-acctasscat = <fs_fld>.

*--- proveedor deseado
    ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-des_vendor = |{ <fs_fld> ALPHA = IN }|.

*--- proveedor fijo
    ASSIGN COMPONENT 'FLIEF' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-fixed_vend = |{ <fs_fld> ALPHA = IN }|.

*--- organización de compras
    ASSIGN COMPONENT 'EKORG' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-purch_org = <fs_fld>.

*--- moneda
    ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-currency = <fs_fld>.

*--- precio valor
    ASSIGN COMPONENT 'PREIS' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-preq_price = <fs_fld>.

*--- Transferir precio SolPed en pedido
    ASSIGN COMPONENT 'BPUEB' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-po_price = <fs_fld>.

*--- contrato marco
    ASSIGN COMPONENT 'KONNR' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-agreement = <fs_fld>.

*--- posición del contrato marco
    ASSIGN COMPONENT 'KTPNR' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-agmt_item = <fs_fld>.

*--- registro info
    ASSIGN COMPONENT 'INFNR' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_pritem-info_rec = <fs_fld>.

*--- mapeo estructura de cambio ITEMS
    ls_pritemx = VALUE #( BASE ls_pritemx
      preq_item   = COND #( WHEN ls_pritem-preq_item   IS NOT INITIAL THEN ls_pritem-preq_item )
      preq_itemx  = COND #( WHEN ls_pritem-preq_item   IS NOT INITIAL THEN abap_true )
      pur_group   = COND #( WHEN ls_pritem-pur_group   IS NOT INITIAL THEN abap_true )
      preq_name   = COND #( WHEN ls_pritem-preq_name   IS NOT INITIAL THEN abap_true )
      material    = COND #( WHEN ls_pritem-material    IS NOT INITIAL THEN abap_true )
      plant       = COND #( WHEN ls_pritem-plant       IS NOT INITIAL THEN abap_true )
      store_loc   = COND #( WHEN ls_pritem-store_loc   IS NOT INITIAL THEN abap_true )
      trackingno  = COND #( WHEN ls_pritem-trackingno  IS NOT INITIAL THEN abap_true )
      suppl_plnt  = COND #( WHEN ls_pritem-suppl_plnt  IS NOT INITIAL THEN abap_true )
      quantity    = COND #( WHEN ls_pritem-quantity    IS NOT INITIAL THEN abap_true )
      preq_date   = COND #( WHEN ls_pritem-preq_date   IS NOT INITIAL THEN abap_true )
      deliv_date  = COND #( WHEN ls_pritem-deliv_date  IS NOT INITIAL THEN abap_true )
      rel_date    = COND #( WHEN ls_pritem-rel_date    IS NOT INITIAL THEN abap_true )
      preq_price  = COND #( WHEN ls_pritem-preq_price  IS NOT INITIAL THEN abap_true )
      acctasscat  = COND #( WHEN ls_pritem-acctasscat  IS NOT INITIAL THEN abap_true )
      gr_ind      = COND #( WHEN ls_pritem-gr_ind      IS NOT INITIAL THEN abap_true )
      ir_ind      = COND #( WHEN ls_pritem-ir_ind      IS NOT INITIAL THEN abap_true )
      des_vendor  = COND #( WHEN ls_pritem-des_vendor  IS NOT INITIAL THEN abap_true )
      fixed_vend  = COND #( WHEN ls_pritem-fixed_vend  IS NOT INITIAL THEN abap_true )
      purch_org   = COND #( WHEN ls_pritem-purch_org   IS NOT INITIAL THEN abap_true )
      agreement   = COND #( WHEN ls_pritem-agreement   IS NOT INITIAL THEN abap_true )
      agmt_item   = COND #( WHEN ls_pritem-agmt_item   IS NOT INITIAL THEN abap_true )
      info_rec    = COND #( WHEN ls_pritem-info_rec    IS NOT INITIAL THEN abap_true )
      po_price    = COND #( WHEN ls_pritem-po_price    IS NOT INITIAL THEN abap_true )
      currency    = COND #( WHEN ls_pritem-currency    IS NOT INITIAL THEN abap_true )
      producttype = COND #( WHEN ls_pritem-producttype IS NOT INITIAL THEN abap_true )
      startdate   = COND #( WHEN ls_pritem-startdate   IS NOT INITIAL THEN abap_true )
      enddate     = COND #( WHEN ls_pritem-enddate     IS NOT INITIAL THEN abap_true )
      ).

    APPEND ls_pritem TO lt_pritem.
    APPEND ls_pritemx TO lt_pritemx.



*--- urgencia de necesidad
    ASSIGN COMPONENT 'PRIO_URG' OF STRUCTURE <fs_wa> TO <fs_fld>.
    gs_prio_urg-bnfpo = lv_pos.
    gs_prio_urg-prio_urg = <fs_fld>.

    APPEND gs_prio_urg TO gt_prio_urg.



*--- imputación
*--- posicion en imputacion
    ls_praccount-preq_item = lv_pos.

*--- número actual de la imputación
    ls_praccount-serial_no = '01'.

*--- cantidad
    ASSIGN COMPONENT 'MENGE' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_praccount-quantity = <fs_fld>.

*--- centro de coste
    ASSIGN COMPONENT 'KOSTL' OF STRUCTURE <fs_wa> TO <fs_fld>.

*--- se toma en cuenta que el CECO puede estar siendo recibido
*--- no el valor sino la descripción
    SELECT SINGLE kostl
      INTO @DATA(lv_kostl)
      FROM cskt
      WHERE spras = @sy-langu AND
            ktext = @<fs_fld>.
    IF sy-subrc = 0.
      ls_praccount-costcenter = lv_kostl.
    ELSE.
      SELECT SINGLE kostl
        INTO @DATA(lv_kstl)
        FROM csks
        WHERE kostl = @<fs_fld>.
      IF sy-subrc = 0.
        ls_praccount-costcenter = lv_kstl.
      ENDIF.
    ENDIF.

*--- orden
    ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_praccount-orderid = |{ <fs_fld> ALPHA = IN }|.

*--- activo fijo
    ASSIGN COMPONENT 'ANLN1' OF STRUCTURE <fs_wa> TO <fs_fld>.
    ls_praccount-asset_no = <fs_fld>.

    ls_praccount-co_area = '1000'.

*--- mapeo estructura de cambio IMPUTACION
    ls_praccountx = VALUE #( BASE ls_praccountx
      preq_item  = COND #( WHEN ls_praccount-preq_item  IS NOT INITIAL THEN ls_praccount-preq_item )
      serial_no  = COND #( WHEN ls_praccount-serial_no  IS NOT INITIAL THEN ls_praccount-serial_no )
      preq_itemx = COND #( WHEN ls_praccount-preq_item  IS NOT INITIAL THEN abap_true )
      serial_nox = COND #( WHEN ls_praccount-serial_no  IS NOT INITIAL THEN abap_true )
      quantity   = COND #( WHEN ls_praccount-quantity   IS NOT INITIAL THEN abap_true )
      costcenter = COND #( WHEN ls_praccount-costcenter IS NOT INITIAL THEN abap_true )
      orderid    = COND #( WHEN ls_praccount-orderid    IS NOT INITIAL THEN abap_true )
      asset_no   = COND #( WHEN ls_praccount-asset_no   IS NOT INITIAL THEN abap_true )
      co_area    = COND #( WHEN ls_praccount-co_area    IS NOT INITIAL THEN abap_true )
    ).

    APPEND ls_praccount TO lt_praccount.
    APPEND ls_praccountx TO lt_praccountx.



    AT LAST.
      IF gt_prio_urg[] IS NOT INITIAL.
        EXPORT gt_prio_urg TO MEMORY ID 'ZGT_PRIO_URG_DT'.
      ENDIF.
      PERFORM f_pr_crt.
      PERFORM f_init.
    ENDAT.

  ENDLOOP.


*HEADER TEXT can be created by using Fm CREATE_TEXT . pass the
*parameters like
*FID # Text ID
*FLANGUAGE Language
*FNAME PR number
*FOBJECT table name.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SHOW_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_show_log.

  DATA: lt_bal_msg TYPE /scmtms/t_bal_s_msg,
        ls_bal_msg TYPE bal_s_msg.

  DATA: text TYPE string.

  REFRESH: lt_bal_msg.

  LOOP AT lt_log_er INTO ls_log_er WHERE type NE 'S'.

    ls_bal_msg-msgty = ls_log_er-type.
    ls_bal_msg-msgid = 'YMQAF01_WSEXT'.
    ls_bal_msg-msgno = '400'.
    ls_bal_msg-msgv1 = ls_log_er-message(50).
    ls_bal_msg-msgv2 = ls_log_er-message+50(50).
    ls_bal_msg-msgv3 = ls_log_er-message+100(50).
    APPEND ls_bal_msg TO lt_bal_msg.

    CLEAR: ls_bal_msg.
  ENDLOOP.

  CALL METHOD /scmtms/cl_batch_helper_80=>show_application_log_in_popup
    EXPORTING
      it_bal_msg = lt_bal_msg.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PR_CRT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pr_crt.

*--- se le pasa el texto de cabecera a la SOLPED
  DATA: lv_fid         TYPE thead-tdid, "
        lt_flines	     TYPE STANDARD TABLE OF tline, "
        lv_flanguage   TYPE thead-tdspras, "
        lv_fname       TYPE thead-tdname, "
        lv_fobject     TYPE thead-tdobject, "
        ls_save_direct TYPE thead, "   'X'
        lv_fformat     TYPE tline-tdformat, "   '*'
        ls_flines      LIKE LINE OF lt_flines,
        lv_ps(5)       TYPE n.

*--- se llama la bapi para la creación de la SOLPED
  BREAK mqa_abap3.
  CALL FUNCTION 'BAPI_PR_CREATE'
    EXPORTING
      prheader    = ls_prheader
      prheaderx   = ls_prheaderx
    IMPORTING
      number      = lv_number
      prheaderexp = ls_prheaderexp
    TABLES
      return      = lt_return
      pritem      = lt_pritem
      pritemx     = lt_pritemx
      praccount   = lt_praccount
      praccountx  = lt_praccountx.

*  CALL FUNCTION 'BAPI_REQUISITION_CREATE'  "Create Purchase Requisition
*    EXPORTING
*      skip_items_with_error          = lv_skip_items_with_error
*      automatic_source               = lv_automatic_source
*    IMPORTING
*      number                         = lv_number
*    TABLES
*      requisition_items              = lt_requisition_items
*      requisition_addrdelivery       = lt_requisition_addrdelivery
*      extensionin                    = lt_extensionin
*      requisition_account_assignment = lt_req_acct_assgmnt
*      requisition_item_text          = lt_requisition_item_text
*      requisition_limits             = lt_requisition_limits
*      requisition_contract_limits    = lt_requisition_contract_limits
*      requisition_services           = lt_requisition_services
*      requisition_srv_accass_values  = lt_req_srv_accass_vls
*      return                         = lt_return
*      requisition_services_text      = lt_requisition_services_text. " BAPI_REQUISITION_CREATE

  READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT lt_return INTO ls_return WHERE type = 'E'.
      APPEND ls_return TO lt_log_er.
    ENDLOOP.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*--- actualizo el nro del pedido en el reporte alv
    CLEAR: gs_table, lt_flines[], lv_fformat, lv_fobject, lv_fname, lv_flanguage, lv_fid, lv_ps.
    LOOP AT gt_table_prcd INTO gs_table.
      gs_table-banfn = lv_number.
      MODIFY gt_table FROM gs_table TRANSPORTING banfn
      WHERE item = gs_table-item AND bnfpo = gs_table-bnfpo.

*--- lleno el texto posicion
      lv_fid = 'B01'.
      lv_flanguage = sy-langu.
      lv_ps = gs_table-bnfpo.

***** Función que saca los ceros a la izquierda de una variable
****      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
****        EXPORTING
****          INPUT  = LV_PS
****        IMPORTING
****          OUTPUT = LV_PS.

      CONCATENATE lv_number lv_ps INTO lv_fname.
      lv_fobject = 'EBAN'.

      CLEAR: ls_flines, lt_flines[].
      ls_flines-tdformat = '*'.
      ls_flines-tdline = gs_table-textpos.
      APPEND ls_flines TO lt_flines.

      CALL FUNCTION 'CREATE_TEXT'  "NOTRANSL: SAPscript-Textbausteine direkt anlegen
        EXPORTING
          fid         = lv_fid
          flanguage   = lv_flanguage
          fname       = lv_fname
          fobject     = lv_fobject
          save_direct = 'X'
          fformat     = lv_fformat
        TABLES
          flines      = lt_flines
        EXCEPTIONS
          no_init     = 1
          no_save     = 2. " CREATE_TEXT

    ENDLOOP.

*--- lleno las notas de posicion
    CLEAR: gs_table, lt_flines[], lv_fformat, lv_fobject, lv_fname, lv_flanguage, lv_fid, lv_ps.
    LOOP AT gt_table_prcd INTO gs_table.
      lv_fid = 'B02'.
      lv_flanguage = sy-langu.
      lv_ps = gs_table-bnfpo.

* Función que saca los ceros a la izquierda de una variable
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_ps
        IMPORTING
          output = lv_ps.

      CONCATENATE lv_number lv_ps INTO lv_fname.
      lv_fobject = 'EBAN'.

      CLEAR: ls_flines, lt_flines[].
      ls_flines-tdformat = '*'.
      ls_flines-tdline = gs_table-notapos.
      APPEND ls_flines TO lt_flines.

      CALL FUNCTION 'CREATE_TEXT'  "NOTRANSL: SAPscript-Textbausteine direkt anlegen
        EXPORTING
          fid         = lv_fid
          flanguage   = lv_flanguage
          fname       = lv_fname
          fobject     = lv_fobject
          save_direct = 'X'
          fformat     = lv_fformat
        TABLES
          flines      = lt_flines
        EXCEPTIONS
          no_init     = 1
          no_save     = 2. " CREATE_TEXT

    ENDLOOP.

*--- lleno el texto de la cabecera a la solped recien creada
    lv_fid = 'B01'.
    lv_flanguage = sy-langu.
    lv_fname = lv_number.
    lv_fobject = 'EBANH'.

    CLEAR: ls_flines, lt_flines[].
    ls_flines-tdformat = '*'.
    ls_flines-tdline = gs_table-textcab.
    APPEND ls_flines TO lt_flines.

    CALL FUNCTION 'CREATE_TEXT'  "NOTRANSL: SAPscript-Textbausteine direkt anlegen
      EXPORTING
        fid         = lv_fid
        flanguage   = lv_flanguage
        fname       = lv_fname
        fobject     = lv_fobject
        save_direct = 'X'
        fformat     = lv_fformat
      TABLES
        flines      = lt_flines
      EXCEPTIONS
        no_init     = 1
        no_save     = 2. " CREATE_TEXT

    CLEAR gt_table_prcd[].
  ENDIF.

*HEADER TEXT can be created by using Fm CREATE_TEXT . pass the
*parameters like
*FID # Text ID
*FLANGUAGE Language
*FNAME PR number
*FOBJECT table name.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init.

  CLEAR: lt_return[],
         lv_number,
         ls_req_acct_assgmnt,
         lt_req_acct_assgmnt[],
         ls_requisition_items,
         lt_requisition_items[],
         lt_requisition_addrdelivery[],
         lt_extensionin[],
         lt_req_acct_assgmnt[],
         lt_requisition_item_text[],
         lt_requisition_limits[],
         lt_requisition_contract_limits[],
         lt_requisition_services[],
         lt_req_srv_accass_vls[],
         lt_requisition_services_text[],
         gt_table_prcd[],
         gs_prio_urg,
         gt_prio_urg[].


  CLEAR: ls_prheader,
         ls_prheaderx,
         lv_number,
         ls_prheaderexp,
         lt_return[],
         lt_pritem[],
         lt_pritemx[],
         lt_praccount[],
         lt_praccountx[],
         lt_servicelines[],
         lt_servicelinesx[].

  FREE MEMORY ID 'ZGT_PRIO_URG_DT'.

ENDFORM.
