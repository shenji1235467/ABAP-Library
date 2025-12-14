CLASS zcl_co_version DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING versi         TYPE coversi
      RETURNING VALUE(result) TYPE REF TO zcl_co_version
      RAISING   zcx_co_versi.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_map,
             versi TYPE coversi,
             obj   TYPE REF TO zcl_co_version,
           END OF multiton_map,

           multiton_set TYPE HASHED TABLE OF multiton_map
           WITH UNIQUE KEY primary_key COMPONENTS versi.

    CLASS-DATA multitons TYPE multiton_set.

    DATA versi TYPE coversi.

    METHODS constructor IMPORTING versi TYPE coversi
                        RAISING   zcx_co_versi.
ENDCLASS.


CLASS zcl_co_version IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN multitons[ KEY primary_key
                      versi = versi ] TO FIELD-SYMBOL(<mt>).
    IF sy-subrc <> 0.
      INSERT VALUE #( versi = versi
                      obj   = NEW #( versi ) ) INTO TABLE multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD constructor.
    IF versi IS INITIAL.
      RAISE EXCEPTION NEW zcx_co_versi( textid = zcx_co_versi=>empty_version ).
    ENDIF.

    SELECT SINGLE FROM tkvs
           FIELDS @abap_true
           WHERE versi = @versi
           INTO @DATA(versi_exists).

    IF versi_exists = abap_false.
      RAISE EXCEPTION NEW zcx_co_versi( textid = zcx_co_versi=>invalid_version ).
    ENDIF.

    me->versi = versi.
  ENDMETHOD.
ENDCLASS.