*&---------------------------------------------------------------------*
*& Report ZMMR046
*&---------------------------------------------------------------------*
*& Descripción: Reporte para carga masiva de SOLPED
*& Consultor:   Javier Otaiza MQA
*& Fecha:       11/07/2025
*&---------------------------------------------------------------------*
*& Modificado Por | Fecha Modificación | Descripción Modificación
*&---------------------------------------------------------------------*
*& Johan Rodriguez| Marzo 2026         | Cambio de BAPI
*&---------------------------------------------------------------------*
REPORT zmmr046.

INCLUDE: zmmr046_tp,
         zmmr046_sl,
         zmmr046_cl,
         zmmr046_f1,
         zmmr046_f2.

*--- abrir el dialogo para el parámetro de la ruta y archivo
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title = 'Seleccionar un archivo'
    CHANGING
      file_table   = gt_files
      rc           = gc_rc
    EXCEPTIONS
      OTHERS       = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING error_with_gui.
  ENDIF.
  READ TABLE gt_files INDEX 1 INTO p_file.

*--- inicio del proceso
START-OF-SELECTION.

*--- armo el catalogo con la estructura de la tabla de los datos
  gv_str_name = 'ZMM_ST_046'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = gv_str_name
      i_client_never_display = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = gtb_field_cat_dyn
*     EXCEPTIONS
*     INCONSISTENT_INTERFACE = 1
*     PROGRAM_ERROR          = 2
*     OTHERS                 = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*--- luego creo la tabla interna dinámica y se asigna al Field Symbol
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gtb_field_cat_dyn
    IMPORTING
      ep_table        = dyn_table.

  ASSIGN dyn_table->* TO <gt_table>.
  CREATE DATA dyn_line LIKE LINE OF <gt_table>.
  ASSIGN dyn_line->* TO <fs_wa>.

*--- se carga el archivo excel en la tabla <GT_DATA>
  PERFORM cargar_archivo.

*--- borro el archivo resultante los registros de "formato"
  DO 4 TIMES.
    DELETE <gt_data> INDEX 1.
  ENDDO.

*--- busco nro columnas de tabla def
  DATA(lv_ncols) = lines( gtb_field_cat_dyn ).

*--- se mueven los datos de la tabla de la carga de excel a la tabla dinamica
*--- primero busco el index del campo fecha desde y fecha hasta
*--- fecha de solicitud
  READ TABLE gtb_field_cat_dyn INTO gs_field_cat WITH KEY fieldname = 'BADAT'.
  IF sy-subrc = 0.
    DATA(lv_inx_badat) = sy-tabix.
  ENDIF.
*--- fecha de entrega de posicion
  READ TABLE gtb_field_cat_dyn INTO gs_field_cat WITH KEY fieldname = 'LFDAT'.
  IF sy-subrc = 0.
    DATA(lv_inx_lfdat) = sy-tabix.
  ENDIF.

  READ TABLE gtb_field_cat_dyn INTO gs_field_cat WITH KEY fieldname = 'STARTDATE'.
  IF sy-subrc = 0.
    DATA(lv_inx_startdate) = sy-tabix.
  ENDIF.

  READ TABLE gtb_field_cat_dyn INTO gs_field_cat WITH KEY fieldname = 'ENDDATE'.
  IF sy-subrc = 0.
    DATA(lv_inx_enddate) = sy-tabix.
  ENDIF.

  gv_i = 1.
  LOOP AT <gt_data> ASSIGNING <ls_line>.

*--- se asignan el resto de los campos de la tabla dinamica
    DO lv_ncols TIMES.
      ASSIGN COMPONENT gv_i OF STRUCTURE <ls_line> TO <fs_1>.
      ASSIGN COMPONENT gv_i OF STRUCTURE <fs_wa> TO <fs_2>.
      IF <fs_1> IS INITIAL.
        gv_i = gv_i + 1.
        CONTINUE.
      ELSE.

*--- adapto los campos de fecha al formato interno y quito los guiones
        IF gv_i EQ lv_inx_badat OR gv_i EQ lv_inx_lfdat OR
           gv_i EQ lv_inx_startdate OR gv_i EQ lv_inx_enddate.
*--- convierto al formato interno
          REPLACE ALL OCCURRENCES OF '-' IN <fs_1> WITH ''.
          CONDENSE <fs_1> NO-GAPS.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external            = <fs_1>
*             ACCEPT_INITIAL_DATE      =
            IMPORTING
              date_internal            = <fs_1>
            EXCEPTIONS
              date_external_is_invalid = 1
              OTHERS                   = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
        ENDIF.

        <fs_2> = <fs_1>.
      ENDIF.
      gv_i = gv_i + 1.
    ENDDO.

*--- se guarda en la tabla transparente
    MOVE-CORRESPONDING <fs_wa> TO gs_table.
    APPEND <fs_wa> TO <gt_table>.
    APPEND gs_table TO gt_table.
    CLEAR <fs_wa>.
    CLEAR gs_table.
    gv_i = 1.
  ENDLOOP.

*--- se imprime el reporte alv
  IF gt_table[] IS NOT INITIAL.
    PERFORM f_display_rpt.
  ENDIF.
