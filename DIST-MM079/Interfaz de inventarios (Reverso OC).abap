***FV50XFLK_LIKP_BEARBEITEN
***INCLUDE FV50XFLK_LIKP_BEARBEITEN .
*---------------------------------------------------------------------*
*       FORM LIKP_BEARBEITEN                                          *
*---------------------------------------------------------------------*
*       Aufgrund der Bildschirmeingabe wird durch die                 *
*       FIELD- Anweisung im Dynpro die Workarea LIKP gepflegt.        *
*---------------------------------------------------------------------*
form likp_bearbeiten.
*{   INSERT         DS4K905522                                        1
IF sy-tcode EQ 'VL32N'.
  DATA: lv_vbeln TYPE likp-vbeln.
  MOVE likp-vbeln TO lv_vbeln.
  EXPORT lv_vbeln TO MEMORY ID 'ENT'.
*  BREAK MQA_ABAP3.
ENDIF.
*}   INSERT

  if slikp-tabix = 0.
    perform likp_anlegen.
  else.
    perform likp_aendern.
  endif.

endform.                    "LIKP_BEARBEITEN


***MV50AFZ1
*---------------------------------------------------------------------*
*       FORM USEREXIT_DELETE_DOCUMENT                                 *
*---------------------------------------------------------------------*
*       This userexit can be used to delete data in additional tables *
*       when a delivery is deleted.                                   *
*                                                                     *
*      This form is called in dialog at the end of form BELEG_LOESCHEN*
*      just before form BELEG_SICHERN is performed to delete the      *
*      datas on the database.                                         *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_DELETE_DOCUMENT.
*{   INSERT         DS4K905522                                        1

IF sy-tcode EQ 'VL32N'.
  BREAK MQA_ABAP3.

  DATA: lo_entrega       TYPE REF TO zws_co_consulta_entrega, "Generated proxy class
        lo_sys_exception TYPE REF TO cx_ai_system_fault,      "Catch structure
        ls_request       TYPE zws_consulta_entrega1,          "Request structure
        ls_response      TYPE zws_consulta_entrega_response1, "Response structure
        lv_msg           TYPE char255.

* Create an instance of the proxy
  TRY.
      CREATE OBJECT lo_entrega.

* Populate the request structure with data
      ls_request-consulta_entrega-numero_entrega = likp-vbeln.

* Call the service operation
      TRY.
          CALL METHOD lo_entrega->consulta_entrega
            EXPORTING
              input  = ls_request
            IMPORTING
              output = ls_response.

* Process the response
          IF ls_response-consulta_entrega_response-status = 'En Proceso'.
            lv_msg = | La entrega entrante tiene estatus en proceso { likp-vbeln }|.
            MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE TO SCREEN 1000 ."1202
          ELSEIF ls_response-consulta_entrega_response-status = 'No iniciado'.
            " Handle error in response
          ELSE.
            " Handle error in response
          ENDIF.

        CATCH cx_ai_system_fault INTO lo_sys_exception.
          lo_sys_exception->if_message~get_text( ).
*          LEAVE TO SCREEN 1000 ."1202
      ENDTRY.

    CATCH cx_ai_system_fault INTO lo_sys_exception.
      lo_sys_exception->if_message~get_text( ).
  ENDTRY.

  COMMIT WORK AND WAIT.

ENDIF.
*}   INSERT

ENDFORM.

***MV50AFZ1
*---------------------------------------------------------------------*
*       FORM USEREXIT_DELETE_DOCUMENT                                 *
*---------------------------------------------------------------------*
*       This userexit can be used to delete data in additional tables *
*       when a delivery is deleted.                                   *
*                                                                     *
*      This form is called in dialog at the end of form BELEG_LOESCHEN*
*      just before form BELEG_SICHERN is performed to delete the      *
*      datas on the database.                                         *
*                                                                     *
*---------------------------------------------------------------------*
FORM USEREXIT_DELETE_DOCUMENT.
*{   INSERT         DS4K905522                                        1

IF sy-tcode EQ 'VL32N'.
  BREAK MQA_ABAP3.

  DATA: lo_entrega       TYPE REF TO zws_co_consulta_entrega, "Generated proxy class
        lo_sys_exception TYPE REF TO cx_ai_system_fault,      "Catch structure
        ls_request       TYPE zws_consulta_entrega1,          "Request structure
        ls_response      TYPE zws_consulta_entrega_response1, "Response structure
        lv_msg           TYPE char255.

* Create an instance of the proxy
  TRY.
      CREATE OBJECT lo_entrega.

* Populate the request structure with data
      ls_request-consulta_entrega-numero_entrega = likp-vbeln.

* Call the service operation
      TRY.
          CALL METHOD lo_entrega->consulta_entrega
            EXPORTING
              input  = ls_request
            IMPORTING
              output = ls_response.

* Process the response
          IF ls_response-consulta_entrega_response-status = 'En Proceso'.
            lv_msg = | La entrega entrante tiene estatus en proceso { likp-vbeln }|.
            MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE TO SCREEN 1000 ."1202
          ELSEIF ls_response-consulta_entrega_response-status = 'No iniciado'.
            " Handle error in response
          ELSE.
            " Handle error in response
          ENDIF.

        CATCH cx_ai_system_fault INTO lo_sys_exception.
          lo_sys_exception->if_message~get_text( ).
*          LEAVE TO SCREEN 1000 ."1202
      ENDTRY.

    CATCH cx_ai_system_fault INTO lo_sys_exception.
      lo_sys_exception->if_message~get_text( ).
  ENDTRY.

  COMMIT WORK AND WAIT.

ENDIF.
*}   INSERT

ENDFORM.
