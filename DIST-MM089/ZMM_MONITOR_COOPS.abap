*&---------------------------------------------------------------------*
*& Report  ZMM_IQ09
*&
*&---------------------------------------------------------------------*
REPORT zmm_monitor_coops.

INCLUDE zmm_monitor_coops_top.
INCLUDE zmm_monitor_coops_f01.


*---------------------------------------------------------------------
*           S T A R T  -  O F  -  S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM get_wb2r_busvol. "TX: WB2R_BUSVOL
  PERFORM get_data.
  PERFORM get_update.
  PERFORM show_alv.

END-OF-SELECTION.
