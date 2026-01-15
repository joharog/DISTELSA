*&---------------------------------------------------------------------*
*& Include          ZMM_IQ09_TOP
*&---------------------------------------------------------------------*

TABLES: equi, eqbs.

*&---------------------------------------------------------------------*
*&           T Y P E  -  P O O L S
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

TYPES: BEGIN OF ty_data,
         werk    TYPE equi-werk,
         matnr   TYPE equi-matnr,
         sernr   TYPE equi-sernr,
         lager   TYPE equi-lager,
         sobkz   TYPE eqbs-sobkz,
         smotor  TYPE ymqasm01de_motor,
         spoliza TYPE ymqasm01de_poliza,
         fpoliza TYPE ymqasm01de_fpoliza,
       END OF ty_data.


*&---------------------------------------------------------------------*
*&           G L O B A L  -  S T R U C T U R E S / T A B L E S
*&---------------------------------------------------------------------*
DATA: gt_data TYPE TABLE OF ty_data.


*&---------------------------------------------------------------------*
*&           F I E L D  S Y M B O L S
*&---------------------------------------------------------------------*
FIELD-SYMBOLS:
  <gt_data_iq09>  TYPE ANY TABLE.


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
  SELECT-OPTIONS: s_werks FOR equi-werk,
                  s_matnr FOR equi-matnr,
                  s_sernr FOR equi-sernr,
                  s_lgort FOR equi-lager,
                  s_sobkz FOR eqbs-sobkz.
SELECTION-SCREEN: END OF BLOCK b1.
