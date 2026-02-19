  METHOD if_ex_le_shp_delivery_proc~save_and_publish_document.

    " 1. Declaración de variables para el Proxy
    DATA: lo_proxy         TYPE REF TO zws_co_notificar_entrega,
          lo_sys_exception TYPE REF TO cx_ai_system_fault,
          ls_request       TYPE zws_notificar_entrega1,
          ls_response      TYPE zws_notificar_entrega_respons1,
          lo_sys_fault     TYPE REF TO cx_ai_system_fault,
          ls_noti_entrega  TYPE zws_notificar_entrega.

    DATA: ls_entrega TYPE zmm_noti_entrega.

    " 2. Filtro de seguridad: Solo Entregas Borradas (VL32N)
    IMPORT ls_noti_entrega = ls_noti_entrega FROM MEMORY ID 'NOTI_DEL'.
*    " 2. Filtro de seguridad: Solo Entregas Entrantes (VL31N)
*    CHECK VALUE #( it_xlikp[ 1 ]-vbtyp OPTIONAL ) = '7'.

    BREAK mqa_abap3.
    IF ls_noti_entrega IS NOT INITIAL.

      TRY.
          " 3. Instanciar el Proxy
          CREATE OBJECT lo_proxy.

          " 4. Mapear los datos de la entrega al Input del Proxy
          ls_request-notificar_entrega-numero_entrega = ls_noti_entrega-numero_entrega.
          ls_request-notificar_entrega-estado_entrega = ls_noti_entrega-estado_entrega.

          " 5. Llamada al método del Proxy
          CALL METHOD lo_proxy->notificar_entrega
            EXPORTING
              input  = ls_request
            IMPORTING
              output = ls_response.

          IF ls_response IS NOT INITIAL.

            ls_entrega = VALUE #( numero_entrega = ls_request-notificar_entrega-numero_entrega
                                  code             = ls_response-notificar_entrega_response-code
                                  status           = ls_response-notificar_entrega_response-status
                                  message          = ls_response-notificar_entrega_response-message

                                  document         = ls_response-notificar_entrega_response-data-document
                                  document_status  = ls_response-notificar_entrega_response-data-document_status
                                  type             = ls_response-notificar_entrega_response-data-type
                                  created_at       = ls_response-notificar_entrega_response-data-created_at
                                  error_detail     = ls_response-notificar_entrega_response-data-error_detail

                                  usuario_registro = sy-uname
                                  fecha_registro   = sy-datum
                                  hora_registro    = sy-uzeit ).

            " 6. Insertar o Actualizar en la tabla Z
            MODIFY zmm_noti_entrega FROM ls_entrega.

          ENDIF.

*      CATCH cx_ai_system_fault INTO lo_sys_exception.
*        lo_sys_exception->if_message~get_text( ).

        CATCH cx_ai_system_fault INTO lo_sys_fault.
          " Si falla el sistema, guardamos el error técnico en la tabla Z
          ls_entrega-numero_entrega = VALUE #( it_xlikp[ 1 ]-vbeln OPTIONAL ).
          ls_entrega-status = 'ERROR_SISTEMA'.
          ls_entrega-message = lo_sys_fault->get_text( ).
          MODIFY zmm_noti_entrega FROM ls_entrega.
      ENDTRY.

    ENDIF.

  ENDMETHOD.
