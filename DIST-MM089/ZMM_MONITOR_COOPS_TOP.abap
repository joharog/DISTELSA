*&---------------------------------------------------------------------*
*& Include          ZMM_MONITOR_COOPS_TOP
*&---------------------------------------------------------------------*

TABLES: wcocoh, equi, eqbs.

*&---------------------------------------------------------------------*
*&           T Y P E  -  P O O L S
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

TYPES: BEGIN OF ty_data,
*        Part 1
         light_ind            TYPE char100,                   "Semaforo
         num                  TYPE wcb_coco_num,              "Contrato de condiciones
         knumh                TYPE knumh,                     "Nº reg.cond.
         krech                TYPE krech,                     "Regla cálculo
         kbetr                TYPE kbetr_kond,                "Importe
         kpein                TYPE kpein,                     "UM de precio
         mto_fond_coop        TYPE kbetr_kond,                "Monto fondo COOP
         cc_curr              TYPE wcb_cc_curr,               "Moneda WCOCOH
         vend_owner           TYPE wcb_vend_owner,            "Cod. Proveedor
         vend_name            TYPE char80,                    "Nom. Proveedor
         bukrs                TYPE bukrs,                     "Sociedad
         butxt                TYPE butxt,                     "Nombre soc.
         ekorg                TYPE ekorg,                     "Organización de compras
         ekotx                TYPE ekotx,                     "Denominación Org.C
         ekgrp                TYPE ekgrp,                     "Grupo de compras
         eknam                TYPE eknam,                     "Denominación Grp.C
         contract_type        TYPE wcb_contract_type,         "Tipo contrato condición
         created_on           TYPE wcb_created_on,            "Fecha de creación
         date_from            TYPE wcb_date_from,             "Inicio de validez
         date_to              TYPE  wcb_date_to ,             "Fin de validez
         settl_date_type      TYPE val_text, "wb2_settlement_date_type,  "Tipo fecha de liquidación
         settl_date           TYPE wb2_settlement_date,       "Fecha de liquidación
         menge                TYPE wfimg,                     "Cantidad de liquidación
         monto_rest_liq       TYPE kwert,                     "Monto restante liquidado
         wfkme                TYPE wfkme,                     "Unidad liquidacion
         waerl                TYPE waerl,                     "Moneda de documentento liquidacion
         kschl_rebv           TYPE kwert,                     "REBV Vol.negocios rappel

*        Part 2
         materialgroupname    TYPE wgbez,                                             "Denominación del grupo de artículos
         materialgrouptext    TYPE wgbez60,                                           "Descripción de grupo de artículos
         salesordertype       TYPE i_salesorder-salesordertype,                       "Clase de documento
         posnr                TYPE vbkd-posnr,                                        "Posicion pedido
         salesorder           TYPE i_salesorder-salesorder,                           "Número de pedido
         creationdate         TYPE i_salesorder-creationdate,                         "Fecha de creación
         salesorganization    TYPE i_salesorder-salesorganization,                    "Organización de ventas
         distributionchannel  TYPE i_salesorder-distributionchannel,                  "Canal de ventas
         sddocumentreason     TYPE i_salesorder-sddocumentreason,                     "Motivo pedido
         sddocumentreason_t   TYPE bezei,                                             "Descripcion Motivo Pedido
         subsequentdocument   TYPE i_sddocumentmultilevelprocflow-subsequentdocument, "Numero de factura
         billingdocumentdate  TYPE i_billingdocument-billingdocumentdate,             "Fecha de factura
         transactioncurrency  TYPE i_billingdocument-transactioncurrency,             "Moneda de factura
         totalnetamount       TYPE i_billingdocument-totalnetamount,                  "Monto neto factura
         totaltaxamount       TYPE i_billingdocument-totaltaxamount,                  " Monto Impuesto
         comp_status          TYPE char30,                                            "Estatus de compensación

*        Extra fields from standard submit
         cust_owner           TYPE wcb_cust_owner,            "Cliente
         owner_name           TYPE wcb_owner_name,            "Nombre del propietario
         process_variant      TYPE wcb_process_variant,       "Variante
         process_variant_text TYPE wcb_process_variant_text,  "Texto variante proceso contrato condicion
         kschl_res3           TYPE kwert,                     "RES3 Reembolso descuento
         kschl_buvo           TYPE kwert,                     "BUVO LF: Volumen negocios
       END OF ty_data.


*&---------------------------------------------------------------------*
*&           G L O B A L  -  S T R U C T U R E S / T A B L E S
*&---------------------------------------------------------------------*
DATA: gt_data   TYPE TABLE OF ty_data,
      gt_data_n TYPE TABLE OF ty_data.

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
  SELECT-OPTIONS: s_vend  FOR wcocoh-vend_owner,
                  s_num   FOR wcocoh-num,
                  s_bukrs FOR wcocoh-bukrs,
                  s_ekorg FOR wcocoh-ekorg,
                  s_ekgrp FOR wcocoh-ekgrp,
                  s_date  FOR wcocoh-created_on.
SELECTION-SCREEN: END OF BLOCK b1.

*SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
*  PARAMETERS: r1 RADIOBUTTON GROUP rad1 DEFAULT 'X',
*              r2 RADIOBUTTON GROUP rad1.
*SELECTION-SCREEN: END OF BLOCK b2.
