CLASS zcl_bc_duration DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF hm_map,
             hrs  TYPE i,
             mins TYPE i,
           END OF hm_map.

    TYPES hm_with_short_units_type TYPE text20.

    CLASS-METHODS get_instance_by_minutes
      IMPORTING minutes       TYPE i
      RETURNING VALUE(result) TYPE REF TO zcl_bc_duration.

    METHODS get_hm                  RETURNING VALUE(result) TYPE hm_map.

    METHODS get_hm_with_short_units RETURNING VALUE(result) TYPE hm_with_short_units_type.

  PRIVATE SECTION.
    TYPES: BEGIN OF min_multiton_map,
             minutes TYPE i,
             obj     TYPE REF TO zcl_bc_duration,
           END OF min_multiton_map,

           min_multiton_set TYPE HASHED TABLE OF min_multiton_map
                            WITH UNIQUE KEY primary_key COMPONENTS minutes.

    CLASS-DATA min_multitons TYPE min_multiton_set.

    DATA: minutes                    TYPE i,
          hm_cache                   TYPE hm_map,
          hm_cached                  TYPE abap_bool,
          hm_with_short_units_cache  TYPE hm_with_short_units_type,
          hm_with_short_units_cached TYPE abap_bool.

    METHODS constructor                IMPORTING minutes       TYPE i.

    METHODS obtain_hm                  RETURNING VALUE(result) TYPE REF TO hm_map.

    METHODS obtain_hm_with_short_units RETURNING VALUE(result) TYPE REF TO hm_with_short_units_type.
ENDCLASS.


CLASS zcl_bc_duration IMPLEMENTATION.
  METHOD get_instance_by_minutes.
    ASSIGN min_multitons[ KEY primary_key
                          minutes = minutes ] TO FIELD-SYMBOL(<mt>).
    IF sy-subrc <> 0.
      INSERT VALUE #( minutes = minutes
                      obj     = NEW #( minutes ) ) INTO TABLE min_multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD get_hm.
    result = obtain_hm( )->*.
  ENDMETHOD.

  METHOD get_hm_with_short_units.
    result = obtain_hm_with_short_units( )->*.
  ENDMETHOD.

  METHOD constructor.
    me->minutes = minutes.
  ENDMETHOD.

  METHOD obtain_hm.
    result = REF #( me->hm_cache ).
    CHECK me->hm_cached = abap_false.

    result->* = VALUE #( hrs  = me->minutes DIV 60
                         mins = me->minutes MOD 60 ).

    me->hm_cached = abap_true.
  ENDMETHOD.

  METHOD obtain_hm_with_short_units.
    result = REF #( me->hm_with_short_units_cache ).
    CHECK me->hm_with_short_units_cached = abap_false.

    DATA(hm) = obtain_hm( ).

    result->* = |{ COND #( WHEN hm->hrs > 0 THEN |{ hm->hrs } { TEXT-hsh } | ) }| &&
                |{ COND #( WHEN hm->mins > 0 THEN |{ hm->mins } { TEXT-msh }| ) }|.

    me->hm_with_short_units_cached = abap_true.
  ENDMETHOD.
ENDCLASS.