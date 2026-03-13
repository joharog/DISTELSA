*&---------------------------------------------------------------------*
*& Include          ZMMR046_TP
*&---------------------------------------------------------------------*

*--- tipos
TYPES: BEGIN OF ty_prio_urg,
         bnfpo    TYPE eban-bnfpo,
         prio_urg TYPE eban-prio_urg,
       END OF ty_prio_urg.

*--- declaraciones
*--- tablas internas
DATA: gt_files          TYPE filetable,
      gt_table          TYPE STANDARD TABLE OF zmm_st_alv_046,
      gt_table_prcd     TYPE STANDARD TABLE OF zmm_st_alv_046,
      gs_table          LIKE LINE OF gt_table,
      gtb_field_cat_dyn TYPE lvc_t_fcat,
      dyn_table         TYPE REF TO data,
      dyn_line          TYPE REF TO data,
      gt_prio_urg       TYPE STANDARD TABLE OF ty_prio_urg.

*--- estructuras
DATA: gs_field_cat TYPE lvc_s_fcat,
      gs_prio_urg  LIKE LINE OF gt_prio_urg.

*--- variables
DATA: gv_str_name(30) TYPE c,
      gv_i            TYPE i.

*--- constantes
DATA: gc_rc TYPE i.


*-------------------------- field symbols -----------------------------*
FIELD-SYMBOLS: <gt_table> TYPE STANDARD TABLE,
               <gt_data>  TYPE STANDARD TABLE,
               <fs_wa>    TYPE any,
               <ls_line>  TYPE any,
               <fs_1>     TYPE any,
               <fs_2>     TYPE any.

*--- se mueven las declaraciones locales a globales por este include
*--- declaraciones locales para la bapi de creacion de solicitudes
*--- de pedidos
*--- tablas
DATA: lt_requisition_items           TYPE STANDARD TABLE OF bapiebanc, "
      lt_requisition_addrdelivery	   TYPE STANDARD TABLE OF bapimerqaddrdelivery, "
      lt_extensionin                 TYPE STANDARD TABLE OF bapiparex, "
      lt_req_acct_assgmnt	           TYPE STANDARD TABLE OF bapiebkn, "
      lt_requisition_item_text       TYPE STANDARD TABLE OF bapiebantx, "
      lt_requisition_limits	         TYPE STANDARD TABLE OF bapiesuhc, "
      lt_requisition_contract_limits TYPE STANDARD TABLE OF bapiesucc, "
      lt_requisition_services	       TYPE STANDARD TABLE OF bapiesllc, "
      lt_req_srv_accass_vls          TYPE STANDARD TABLE OF bapiesklc, "
*      lt_return                       TYPE STANDARD TABLE OF bapireturn, "
      lt_requisition_services_text   TYPE STANDARD TABLE OF bapieslltx, "
      lt_log_er	                     TYPE STANDARD TABLE OF bapireturn.

*--- estructuras
DATA: ls_requisition_items LIKE LINE OF lt_requisition_items,
      ls_req_acct_assgmnt  LIKE LINE OF lt_req_acct_assgmnt,
*      ls_return            LIKE LINE OF lt_return,
      ls_log_er            LIKE LINE OF lt_log_er,
      ls_extensionin       TYPE bapiparex.

*--- variables
DATA: lv_number	               TYPE bapiebanc-preq_no, "
      lv_skip_items_with_error TYPE bapimmpara-selection, "
      lv_automatic_source      TYPE bapimmpara-selection VALUE 'X', "   'X'
      lv_pos                   TYPE i.

FIELD-SYMBOLS: <fs_fld> TYPE any.

*--- Nueva BAPI | BAPI_PR_CREATE
*--- Import
DATA: ls_prheader  LIKE  bapimereqheader,
      ls_prheaderx LIKE  bapimereqheaderx,
      ls_testrun   LIKE  bapiflag-bapiflag.

*--- Export
DATA: ls_number      LIKE  bapimereqheader-preq_no,
      ls_prheaderexp TYPE  bapimereqheader.

*--- Tables
DATA: lt_return        TYPE TABLE OF bapiret2,
      lt_pritem        TYPE TABLE OF bapimereqitemimp,
      lt_pritemx       TYPE TABLE OF  bapimereqitemx,
      lt_praccount     TYPE TABLE OF bapimereqaccount,
      lt_praccountx    TYPE TABLE OF bapimereqaccountx,
      lt_servicelines  TYPE TABLE OF bapi_srv_service_line,
      lt_servicelinesx TYPE TABLE OF bapi_srv_service_linex.

DATA: ls_return        TYPE bapiret2,
      ls_pritem        TYPE bapimereqitemimp,
      ls_pritemx       TYPE bapimereqitemx,
      ls_praccount     TYPE bapimereqaccount,
      ls_praccountx    TYPE bapimereqaccountx,
      ls_servicelines  TYPE bapi_srv_service_line,
      ls_servicelinesx TYPE bapi_srv_service_linex.
