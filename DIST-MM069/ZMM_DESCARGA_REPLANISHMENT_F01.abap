*&---------------------------------------------------------------------*
*& Include          ZMM_DESCARGA_REPLANISHMENT_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.

  DATA: lr_locnr     TYPE RANGE OF wrf3-locnr,
        lt_return    TYPE TABLE OF bapireturn,
        ls_return    TYPE bapireturn,
        lv_material  TYPE matnr18,
        lv_plant     TYPE bapimatvp-werks,

        lt_wmdvsx    TYPE TABLE OF bapiwmdvs,
        lt_wmdvex    TYPE TABLE OF bapiwmdve,

        lt_items     TYPE TABLE OF bapiekpoc,

        lv_date      TYPE scal-date,
        lv_week      TYPE scal-week,

        lw_mver      TYPE mver,              " Work area for the loop
        lv_count     TYPE n LENGTH 2,        " Counter for the month number (01 to 52)
        lv_fieldname TYPE fieldname,     " To hold the dynamic field name
        lv_total1    TYPE mver-gsv01,
        lv_total8    TYPE mver-gsv08,
        lv_total52   TYPE mver-gsv13.        " Variable to store the running total (use a suitable type)

  FIELD-SYMBOLS: <lfs_gsv> TYPE any.     " Field symbol to dynamically assign fields

  BREAK mqa_abap3.
  IF s_matnr IS NOT INITIAL.
    SELECT * FROM mara INTO TABLE @DATA(lt_mara)
      WHERE matnr IN @s_matnr
        AND matkl IN @s_matkl.
  ELSE.
    SELECT * FROM mara INTO TABLE lt_mara
      WHERE matkl IN s_matkl.
  ENDIF.

  IF sy-subrc EQ 0.
    SELECT * FROM makt INTO TABLE @DATA(lt_makt)
      FOR ALL ENTRIES IN @lt_mara
      WHERE matnr EQ @lt_mara-matnr
        AND spras EQ @sy-langu.

    SELECT * FROM marc INTO TABLE @DATA(lt_marc)
      FOR ALL ENTRIES IN @lt_mara
      WHERE matnr EQ @lt_mara-matnr
        AND werks IN @s_werks
        AND dispo IN @s_dispo
        AND mmsta EQ ''.

    IF sy-subrc EQ 0.
      SELECT * FROM t001w INTO TABLE @DATA(lt_t001w)
        FOR ALL ENTRIES IN @lt_marc
        WHERE werks EQ @lt_marc-werks.

      lr_locnr = VALUE #( FOR wa IN lt_marc (
          sign   = 'I'
          option = 'EQ'
          low    = |{ wa-werks }| ) ).

      IF lr_locnr IS NOT INITIAL.
        SELECT * FROM wrf3 INTO TABLE @DATA(lt_wrf3) "Tomar PRIORITAET = 01 CUANDO EXISTAN VARIOS REG
          WHERE locnr IN @lr_locnr.
      ENDIF.

      SELECT * FROM mard INTO TABLE @DATA(lt_mard)
        FOR ALL ENTRIES IN @lt_wrf3
        WHERE matnr IN @s_matnr
          AND werks EQ @lt_wrf3-loclb
          AND lgort EQ '1001'.  "Pasar el campo a STVARV (hacerlo configurable)
    ENDIF.

  ENDIF.






  LOOP AT lt_marc INTO DATA(ls_marc).

    gs_data-werks = ls_marc-werks.
    gs_data-articulo = ls_marc-matnr.
    gs_data-store_transit = ls_marc-trame.
    gs_data-planning_feature = ls_marc-dismm.

    READ TABLE lt_t001w INTO DATA(ls_t001w) WITH KEY werks = ls_marc-werks.
    IF sy-subrc EQ 0.
      gs_data-cod_b1   = ls_t001w-name1.
      gs_data-werks_s4 = ls_t001w-name2.
    ENDIF.

    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_marc-matnr.
    IF sy-subrc EQ 0.

      gs_data-marca = ls_mara-brand_id.
      gs_data-grupo_articulo = ls_mara-matkl.

      SELECT SINGLE vtext FROM t179t INTO gs_data-departamento
        WHERE prodh EQ ls_mara-matkl(3).

      SELECT SINGLE vtext FROM t179t INTO gs_data-tipo_producto
        WHERE prodh EQ ls_mara-matkl(5).

      SELECT SINGLE vtext FROM t179t INTO gs_data-familia
        WHERE prodh EQ ls_mara-matkl(7).

      gs_data-subfamilia = 'NO APLICA'.

      READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_mara-matnr.
      IF sy-subrc EQ 0.
        gs_data-art_descrip = ls_makt-maktx.
      ENDIF.

    ENDIF.

    READ TABLE lt_wrf3 INTO DATA(ls_wrf3) WITH KEY locnr = ls_marc-werks.
    IF sy-subrc EQ 0.
      IF ls_wrf3-prioritaet EQ '01'.

        READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY werks = ls_wrf3-loclb
                                                       matnr = ls_marc-matnr.
        IF sy-subrc EQ 0.
          gs_data-libre_util_cd    = ls_mard-labst.
          gs_data-libre_util_store = ls_mard-labst.
        ENDIF.
      ENDIF.
    ENDIF.

    lv_material  = |{ ls_marc-matnr ALPHA = OUT }|.
    lv_material  = |{ lv_material ALPHA = IN }|.
    lv_plant = ls_wrf3-loclb.

    REFRESH: lt_wmdvsx, lt_wmdvex.
    CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
      EXPORTING
        plant      = lv_plant       "Centro de WRF3
        material   = lv_material
        unit       = 'UN'
        check_rule = 'B'
      IMPORTING
        return     = ls_return
      TABLES
        wmdvsx     = lt_wmdvsx
        wmdvex     = lt_wmdvex.

    READ TABLE lt_wmdvex INTO DATA(ls_wmdvex) INDEX 1.
    IF sy-subrc EQ 0.
      gs_data-ventas_cd = ls_wmdvex-com_qty.
    ENDIF.

    READ TABLE lt_wmdvsx INTO DATA(ls_wmdvsx) INDEX 1.
    IF sy-subrc EQ 0.
      gs_data-stock_cd =  ls_wmdvsx-req_qty.
    ENDIF.

    REFRESH: lt_wmdvsx, lt_wmdvex.
    CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
      EXPORTING
        plant      = ls_marc-werks  "Centro de Marc
        material   = lv_material
        unit       = 'UN'
        check_rule = 'B'
      IMPORTING
        return     = ls_return
      TABLES
        wmdvsx     = lt_wmdvsx
        wmdvex     = lt_wmdvex.

    READ TABLE lt_wmdvex INTO ls_wmdvex INDEX 1.
    IF sy-subrc EQ 0.
      gs_data-sale_store = ls_wmdvex-com_qty.
    ENDIF.

    READ TABLE lt_wmdvsx INTO ls_wmdvsx INDEX 1.
    IF sy-subrc EQ 0.
      gs_data-stock_comp =  ls_wmdvsx-req_qty.
    ENDIF.

    CALL FUNCTION 'BAPI_PO_GETITEMS'
      EXPORTING
        material = lv_material "ls_marc-matnr
        plant    = lv_plant    "ls_wrf3-locnr
      TABLES
*       PO_HEADERS =
        po_items = lt_items
        return   = lt_return.

    LOOP AT lt_items INTO DATA(ls_items).
*      gs_data-stock_cd += ls_items-disp_quan. "Validar logica con funcional ( punto 7-G )

*    gs_data-stock_store "Validar logica con funcional (punto 8)
*    gs_data-stock_store
    ENDLOOP.




*  Validar API con funcional
*  bill_comp
*  history_plan

* Pendiente recepción disponible para venta
    gs_data-pending_sale_store = gs_data-plan_history - gs_data-store_transit.

* Ingresos
    gs_data-income = gs_data-bill_comp + gs_data-sale_store.

* Engresos
    gs_data-expenses = gs_data-bill_comp + gs_data-plan_history.


* Periódo ultimo pronostico (semana anterior)
    lv_date = sy-datum - 7.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = lv_date
      IMPORTING
        week         = lv_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    gs_data-forecast_lwp = lv_week.


* Valor del Pronostico Semana anterior
    SELECT SINGLE * FROM mapr INTO @DATA(ls_mapr)
      WHERE matnr EQ @ls_marc-matnr
        AND werks EQ @ls_marc-werks.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM prop INTO @DATA(ls_prop)
        WHERE pnum1 EQ @ls_mapr-pnum1
          AND hsnum EQ '00'.
      IF sy-subrc EQ 0.

        CLEAR: lv_date.

        CALL FUNCTION 'WEEK_GET_FIRST_DAY'
          EXPORTING
            week         = gs_data-forecast_lwp
          IMPORTING
            date         = lv_date
          EXCEPTIONS
            week_invalid = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        SELECT SINGLE * FROM prow INTO  @DATA(ls_prow)
           WHERE pnum2 EQ @ls_prop-pnum2
             AND ertag EQ @lv_date.
        IF sy-subrc EQ 0.
          gs_data-value_lwp = ls_prow-prwrt.
        ENDIF.

      ENDIF.

    ENDIF.

* Periódo pronostico (semana actual)
    CLEAR: lv_date, lv_week.
    lv_date = sy-datum.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = lv_date
      IMPORTING
        week         = lv_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    gs_data-forecast_awp = lv_week.


* Valor del Pronostico Semana actual
    CLEAR: lv_date.

    CALL FUNCTION 'WEEK_GET_FIRST_DAY'
      EXPORTING
        week         = gs_data-forecast_awp
      IMPORTING
        date         = lv_date
      EXCEPTIONS
        week_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT SINGLE * FROM prow INTO ls_prow
       WHERE pnum2 EQ ls_prop-pnum2
         AND ertag EQ lv_date.
    IF sy-subrc EQ 0.
      gs_data-value_awp = ls_prow-prwrt.
    ENDIF.

* Periódo pronostico (proxima semana)
    CLEAR: lv_date, lv_week.
    lv_date = sy-datum.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = lv_date
      IMPORTING
        week         = lv_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    gs_data-forecast_nwp = lv_week.


* Valor del Pronostico Semana proxima
    CLEAR: lv_date.

    CALL FUNCTION 'WEEK_GET_FIRST_DAY'
      EXPORTING
        week         = gs_data-forecast_nwp
      IMPORTING
        date         = lv_date
      EXCEPTIONS
        week_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT SINGLE * FROM prow INTO ls_prow
       WHERE pnum2 EQ ls_prop-pnum2
         AND ertag EQ lv_date.
    IF sy-subrc EQ 0.
      gs_data-value_nwp = ls_prow-prwrt.
    ENDIF.


* Historial Acumulado consumo ultimas 52 semanas
    SELECT SINGLE * FROM mver INTO @DATA(ls_mver)
      WHERE werks EQ @ls_marc-werks
        AND matnr EQ @ls_marc-matnr
        AND gjahr EQ @sy-datum(4).
    IF sy-subrc EQ 0.

      " Use a DO loop to sum the monthly fields (GSV01 to GSV52)
      DO 52 TIMES.
        " Format the field name dynamically (e.g., 'GSV01', 'GSV02')
        lv_count = sy-index. " sy-index goes from 1 to DO times

        CONCATENATE 'GSV' lv_count INTO lv_fieldname.
        CONDENSE lv_fieldname NO-GAPS.

        " Assign the dynamic field to the field symbol
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE lw_mver TO <lfs_gsv>.

        " Check if assignment was successful and field is numeric (MVER-GSVxx are numeric)
        IF sy-subrc = 0 AND <lfs_gsv> IS ASSIGNED.
          lv_total52 = lv_total52 + <lfs_gsv>.

          IF lv_count LE '08'.
            lv_total8 = lv_total8 + <lfs_gsv>.
          ENDIF.

          IF lv_count EQ '01'.
            lv_total1 = <lfs_gsv>.
          ENDIF.


        ENDIF.
      ENDDO.

      IF lv_total52 IS NOT INITIAL.
        gs_data-accu_history_52 = lv_total52 / 52.  "Validar semanas con el funcional solo llega GSV13
      ENDIF.

      IF lv_total8 IS NOT INITIAL.
        gs_data-accu_history_8 = lv_total8 / 8.
      ENDIF.

      IF lv_total1 IS NOT INITIAL.
        gs_data-forecast_daily = lv_total1 / 7.
      ENDIF.

    ENDIF.


    SELECT SINGLE * FROM wrpl INTO @DATA(ls_wrpl)
      WHERE matnr EQ @ls_marc-matnr
        AND kunnr EQ @ls_marc-werks.  "Validar si aplica con SRP_WERKS.
    IF sy-subrc EQ 0.
      gs_data-target_stock = ls_wrpl-sobst. "Stock Objetivo

      gs_data-target_stock_min  = ls_wrpl-prwug. "Stock Objetivo mínimo

      gs_data-target_stock_max = ls_wrpl-prwog. "Stock Objetivo máximo

      gs_data-target_coverage_days = ls_wrpl-trcov. "Cobertura en días

    ENDIF.

    gs_data-stock_security = ls_marc-eisbe. "Stock de seguridad
    gs_data-delivery_frequency_id = ls_marc-mrppp. "ID Frecuencia Entrega.
    gs_data-planning_cycle_id = ls_marc-lfrhy. "ID Ciclo de planificación


*  Necesidad Reaprov.
    CASE ls_marc-dismm.
      WHEN 'RP'.
        gs_data-need_reapproval = gs_data-sale_store - gs_data-target_stock.
      WHEN 'RF'.
        gs_data-need_reapproval = gs_data-forecast_daily * gs_data-target_coverage_days.
    ENDCASE.


*  reception_id = Denominación FE
*  description = Denominación CP





    APPEND gs_data TO gt_data.
    CLEAR: gs_data.

  ENDLOOP.

  IF gt_data IS INITIAL.
    MESSAGE 'No se encontraron datos' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.



***&---------------------------------------------------------------------*
***& Form GET_DATA
***&---------------------------------------------------------------------*
*FORM GET_DATA.
*
*  DATA:
*    LT_SELSCREEN TYPE TABLE OF RSPARAMS,
*    LS_SELSCREEN TYPE RSPARAMS,
*    LR_DATA      TYPE REF TO DATA.
*
*
*  IF S_VEND IS NOT INITIAL.
*    APPEND_SELSCREEN: 'VEND_OWNER'  'S' 'I' 'BT' S_VEND-LOW S_VEND-HIGH.    "Proveedor
*  ENDIF.
*
*  IF S_NUM IS NOT INITIAL.
*    APPEND_SELSCREEN: 'NUM' 'S' 'I' 'BT' S_NUM-LOW S_NUM-HIGH.    "Contrato de condiciones
*  ENDIF.
*
*  IF S_BUKRS IS NOT INITIAL.
*    APPEND_SELSCREEN: 'BUKRS' 'S' 'I' 'BT' S_BUKRS-LOW S_BUKRS-HIGH.    "Sociedad
*  ENDIF.
*
*  IF S_EKORG IS NOT INITIAL.
*    APPEND_SELSCREEN: 'EKORG' 'S' 'I' 'BT' S_EKORG-LOW S_EKORG-HIGH.    "Organización compras
*  ENDIF.
*
*  IF S_EKGRP IS NOT INITIAL.
*    APPEND_SELSCREEN: 'EKGRP' 'S' 'I' 'BT' S_EKGRP-LOW S_EKGRP-HIGH.    "Organización ventas
*  ENDIF.
*
*  IF S_DATE IS NOT INITIAL.
*    APPEND_SELSCREEN: 'SETTL_DATE' 'S' 'I' 'BT' S_DATE-LOW S_DATE-HIGH.    "Fecha de liquidación
*  ENDIF.
*
*
**  BREAK-POINT.
*  CL_SALV_BS_RUNTIME_INFO=>SET( EXPORTING DISPLAY  = ABAP_FALSE
*                                          METADATA = ABAP_FALSE
*                                          DATA     = ABAP_TRUE ).
*
*  SUBMIT RWB2R_BUSINESS_VOLUME WITH SELECTION-TABLE LT_SELSCREEN AND RETURN EXPORTING LIST TO MEMORY.
**  SUBMIT rwb2r_business_volume AND RETURN EXPORTING LIST TO MEMORY.
*
*  WAIT UP TO 1 SECONDS.
*
*  IF <FS_CONTRACT_DATA> IS ASSIGNED.
*    UNASSIGN <FS_CONTRACT_DATA>.
*  ENDIF.
*
*  TRY.
*      CL_SALV_BS_RUNTIME_INFO=>GET_DATA_REF( IMPORTING R_DATA = LR_DATA ).
*
*      ASSIGN LR_DATA->* TO <FS_CONTRACT_DATA>.
*
*      IF <FS_CONTRACT_DATA> IS ASSIGNED.
*        MOVE-CORRESPONDING <FS_CONTRACT_DATA> TO GT_DATA.
*      ENDIF.
*
*    CATCH CX_SALV_BS_SC_RUNTIME_INFO.
*  ENDTRY.
*
*  CL_SALV_BS_RUNTIME_INFO=>CLEAR_ALL( ).
*
*ENDFORM.
*
*
**&---------------------------------------------------------------------*
**& Form GET_IQ09
**&---------------------------------------------------------------------*
*FORM GET_DATA.
*  BREAK-POINT.
*  DATA: LR_BSTKD      TYPE RANGE OF VBKD-BSTKD,
*        LR_SALESORDER TYPE RANGE OF I_SALESORDER-SALESORDER.
*
*  DATA LT_SD_FLOW TYPE TABLE OF I_SDDOCUMENTMULTILEVELPROCFLOW.
*
*  SELECT * FROM WCOCOH INTO TABLE @DATA(LT_WCOCOH)
*    FOR ALL ENTRIES IN @GT_DATA
*    WHERE NUM EQ @GT_DATA-NUM.
*  IF SY-SUBRC EQ 0.
*
*    LR_BSTKD = VALUE #( FOR WA IN LT_WCOCOH (
*        SIGN   = 'I'
*        OPTION = 'EQ'
*        LOW    = |{ WA-NUM ALPHA = OUT }| ) ).
*
*    IF LR_BSTKD IS NOT INITIAL.
*      SELECT * FROM VBKD INTO TABLE @DATA(LT_VBKD)
*          WHERE BSTKD IN @LR_BSTKD
*            AND POSNR NE '000000'.
*      IF SY-SUBRC EQ 0.
*        SELECT * FROM I_SALESORDER INTO TABLE @DATA(LT_SALESORDER)
*          FOR ALL ENTRIES IN @LT_VBKD
*          WHERE SALESORDER EQ @LT_VBKD-VBELN.
*        IF SY-SUBRC EQ 0.
*
*          LR_SALESORDER = VALUE #( FOR WA2 IN LT_SALESORDER (
*              SIGN   = 'I'
*              OPTION = 'EQ'
*              LOW    = |{ WA2-SALESORDER ALPHA = IN }| ) ).
*
*          SELECT SINGLE *
*            FROM I_SDDOCUMENTMULTILEVELPROCFLOW
*            WHERE PRECEDINGDOCUMENT EQ '0025000187'
*            INTO @DATA(LS_SD_FLOW).
*
*          "BUSCAR ALRTERNATIVA EN vbfa
*
*          SELECT * FROM I_SDDOCUMENTMULTILEVELPROCFLOW INTO TABLE @DATA(LT_MULTILEVELFLOW).
**            FOR ALL ENTRIES IN @lt_salesorder
**            WHERE precedingdocument in @lr_salesorder "@lt_salesorder-salesorder.
**              where precedingdocumentcategory EQ 'C'.
*          IF SY-SUBRC EQ 0.
*            SELECT * FROM I_BILLINGDOCUMENT INTO TABLE @DATA(LT_BILLINGDOCUMENT)
*              FOR ALL ENTRIES IN @LT_MULTILEVELFLOW
*              WHERE BILLINGDOCUMENT EQ @LT_MULTILEVELFLOW-SUBSEQUENTDOCUMENT.
*            IF SY-SUBRC EQ 0.
*              SELECT * FROM P_RO_SAFTBSADBELNR INTO TABLE @DATA(LT_PROSAFTBSADBELNR)
*                FOR ALL ENTRIES IN @LT_BILLINGDOCUMENT
*                WHERE ACCOUNTINGDOCUMENT EQ @LT_BILLINGDOCUMENT-BILLINGDOCUMENT.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.
*
*
*
*  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<FS_DATA>).
*
*    READ TABLE LT_WCOCOH INTO DATA(LS_WCOCOH) WITH KEY NUM = <FS_DATA>-NUM.
*    IF SY-SUBRC EQ 0.
*
*      MOVE-CORRESPONDING LS_WCOCOH TO <FS_DATA>.
*
*      READ TABLE LT_VBKD INTO DATA(LS_VBKD) WITH KEY BSTKD = LS_WCOCOH-NUM.
*      IF SY-SUBRC EQ 0.
*
*        READ TABLE LT_SALESORDER INTO DATA(LS_SALESORDER) WITH KEY SALESORDER = LS_VBKD-VBELN.
*        IF SY-SUBRC EQ 0.
*          MOVE-CORRESPONDING LS_SALESORDER TO <FS_DATA>.
*
*          READ TABLE LT_MULTILEVELFLOW INTO DATA(LS_MULTILEVELFLOW) WITH KEY PRECEDINGDOCUMENT = LS_SALESORDER-SALESORDER.
*          IF SY-SUBRC EQ 0.
*            MOVE: LS_MULTILEVELFLOW-SUBSEQUENTDOCUMENT TO <FS_DATA>-SUBSEQUENTDOCUMENT.
*
*            READ TABLE LT_BILLINGDOCUMENT INTO DATA(LS_BILLINGDOCUMENT) WITH KEY BILLINGDOCUMENT = LS_MULTILEVELFLOW-SUBSEQUENTDOCUMENT.
*            IF SY-SUBRC EQ 0.
*              MOVE-CORRESPONDING LS_BILLINGDOCUMENT TO <FS_DATA>.
*
*              READ TABLE LT_PROSAFTBSADBELNR INTO DATA(LS_PROSAFTBSADBELNR) WITH KEY ACCOUNTINGDOCUMENT = LS_BILLINGDOCUMENT-BILLINGDOCUMENT.
*              IF SY-SUBRC EQ 0.
*                <FS_DATA>-COMP_STATUS = 'Compensada'.
*              ELSE.
*                <FS_DATA>-COMP_STATUS = 'No Compensada'.
*              ENDIF.
*
*            ENDIF.
*
*          ENDIF.
*        ENDIF.
*
*      ENDIF.
*
*
*    ENDIF.
*
*
*
*  ENDLOOP.
*
*
*ENDFORM.
*
*
**&---------------------------------------------------------------------*
**& Form UPDATE
**&---------------------------------------------------------------------*
*FORM GET_UPDATE.
**
**  DATA lt_comp_mat TYPE TABLE OF ymqasm01_tb00001.
**
**  SELECT * FROM ymqasm01_tb00001 INTO TABLE lt_comp_mat
**    FOR ALL ENTRIES IN gt_data
**    WHERE matnr EQ gt_data-matnr
**      AND sernr EQ gt_data-sernr.
**  IF sy-subrc EQ 0.
**
**    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
**      READ TABLE lt_comp_mat INTO DATA(ls_comp_mat) WITH KEY matnr = <fs_data>-matnr sernr = <fs_data>-sernr.
**      IF sy-subrc EQ 0.
**        MOVE: ls_comp_mat-smotor  TO <fs_data>-smotor,
**              ls_comp_mat-spoliza TO <fs_data>-spoliza,
**              ls_comp_mat-fpoliza TO <fs_data>-fpoliza.
**      ENDIF.
**    ENDLOOP.
**
**  ENDIF.
*
*ENDFORM.
*
*
*&---------------------------------------------------------------------*
*& Form SHOW ALV
*&---------------------------------------------------------------------*
FORM show_alv.
  PERFORM alv_group.
  PERFORM alv_fieldcat.
  PERFORM alv_layout.
  PERFORM alv_display.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form ALV_GROUP
*&---------------------------------------------------------------------*
FORM alv_group.
  APPEND VALUE #( sp_group = 'A'
                  text     = TEXT-001 )
                  TO gt_slis_group.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form ALV_FIELDCAT
*&---------------------------------------------------------------------*
FORM alv_fieldcat.
  REFRESH gt_slis_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = 'ZSTR_REPLANISHMENT' " Pass the local or DDIC structure name
      i_inclname             = sy-repid             " Must be the program/include where structure is defined
    CHANGING
      ct_fieldcat            = gt_slis_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    " Handle errors
  ENDIF.


*  APPEND VALUE #( FIELDNAME = 'LIGHT_IND'
*                  SELTEXT_L = 'Semáforo')
*                  TO GT_SLIS_FIELDCAT.
*
*  APPEND VALUE #( FIELDNAME = 'NUM'
*                  SELTEXT_L = 'Contrato de condición' )
*                  TO GT_SLIS_FIELDCAT.



ENDFORM.


*&---------------------------------------------------------------------*
*& Form ALV_LAYOUT
*&---------------------------------------------------------------------*
FORM alv_layout.
  CLEAR gs_slis_layout.
  gs_slis_layout-colwidth_optimize   = 'X'.
  gs_slis_layout-zebra               = 'X'.
*  gs_slis_layout-box_fieldname       = 'CHECK'.
*  gs_slis_layout-get_selinfos        = 'X'.
*  gs_slis_layout-f2code              = 'BEAN' .
*  gs_slis_layout-confirmation_prompt = 'X'.
*  gs_slis_layout-key_hotspot         = 'X'.
*  gs_slis_layout-info_fieldname      = 'COL'.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM alv_display.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_top_of_page   = 'TOP_OF_PAGE'
*     i_callback_pf_status_set = 'PF_STATUS'
*     i_callback_user_command  = 'USER_COMMAND'
      is_layout          = gs_slis_layout
      it_fieldcat        = gt_slis_fieldcat
      it_special_groups  = gt_slis_group
      i_save             = 'X'
    TABLES
      t_outtab           = gt_data.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form PF_STATUS
*&---------------------------------------------------------------------*
*FORM pf_status USING extab TYPE slis_t_extab.
*  SET PF-STATUS 'STANDARD'.
*ENDFORM.
