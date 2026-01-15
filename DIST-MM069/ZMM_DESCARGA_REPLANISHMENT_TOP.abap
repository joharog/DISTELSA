*&---------------------------------------------------------------------*
*& Include          ZMM_DESCARGA_REPLANISHMENT_TOP
*&---------------------------------------------------------------------*

TABLES: mara, marc.

*&---------------------------------------------------------------------*
*&           T Y P E  -  P O O L S
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.


*&---------------------------------------------------------------------*
*&           G L O B A L  -  S T R U C T U R E S / T A B L E S
*&---------------------------------------------------------------------*
DATA: gt_data TYPE TABLE OF zstr_replanishment,
      gs_data TYPE zstr_replanishment.

*&---------------------------------------------------------------------*
*&           F I E L D  S Y M B O L S
*&---------------------------------------------------------------------*
FIELD-SYMBOLS:
  <gt_data_iq09>     TYPE ANY TABLE,
  <fs_contract_data> TYPE ANY TABLE.

*&---------------------------------------------------------------------*
*&           A L V  -  D E F I N I T I O N
*&---------------------------------------------------------------------*
DATA:
  gt_slis_group    TYPE slis_t_sp_group_alv,
  gt_slis_fieldcat TYPE slis_t_fieldcat_alv,
  gs_slis_layout   TYPE slis_layout_alv,
  gt_slis_header   TYPE slis_t_listheader,

  ls_refalv        TYPE REF TO cl_gui_alv_grid.


*&---------------------------------------------------------------------*
*&           D E F I N E
*&---------------------------------------------------------------------*
DEFINE append_selscreen.
  CLEAR:
    ls_selscreen.

  ls_selscreen-selname = &1.
  ls_selscreen-kind    = &2.
  ls_selscreen-sign    = &3.
  ls_selscreen-option  = &4.
  ls_selscreen-low     = &5.
  ls_selscreen-high    = &6.

  APPEND ls_selscreen TO lt_selscreen.
END-OF-DEFINITION.


*&---------------------------------------------------------------------*
*&           S T A R T  -  O F  -  S E L E C T I O N
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_werks FOR marc-werks OBLIGATORY,  "Centro
                  s_dispo FOR marc-dispo OBLIGATORY,  "Planificador de necesidades
                  s_matkl FOR mara-matkl OBLIGATORY,  "Grupo artículos
                  s_matnr FOR marc-matnr.             "Material
SELECTION-SCREEN: END OF BLOCK b1.

*SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
*  PARAMETERS: r1 RADIOBUTTON GROUP rad1 DEFAULT 'X',
*              r2 RADIOBUTTON GROUP rad1.
*SELECTION-SCREEN: END OF BLOCK b2.
