*&---------------------------------------------------------------------*
*& Report ZMM_DESCARGA_REPLANISHMENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_descarga_replanishment.

INCLUDE zmm_descarga_replanishment_top.
INCLUDE zmm_descarga_replanishment_f01.


*---------------------------------------------------------------------
*           S T A R T  -  O F  -  S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM GET_DATA.
*  PERFORM GET_UPDATE.
  PERFORM SHOW_ALV.

END-OF-SELECTION.
