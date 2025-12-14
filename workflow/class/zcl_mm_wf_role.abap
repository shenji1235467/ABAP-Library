CLASS zcl_mm_wf_role DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING otype         TYPE otype
                objid         TYPE actorid
      RETURNING VALUE(result) TYPE REF TO zcl_mm_wf_role.

    METHODS get_position_long_text RETURNING VALUE(result) TYPE string.

    METHODS get_approver_full_name RETURNING VALUE(result) TYPE string.

    METHODS get_approver_objec     RETURNING VALUE(result) TYPE objec.

    METHODS get_approver_bname     RETURNING VALUE(result) TYPE xubname.

    METHODS get_approver_objecs    RETURNING VALUE(result) TYPE hap_t_objec.

    METHODS get_position_objec     RETURNING VALUE(result) TYPE objec.

  PRIVATE SECTION.
    TYPES: BEGIN OF mt_dict, " Multiton
             otype TYPE otype,
             objid TYPE actorid,
             obj   TYPE REF TO zcl_mm_wf_role,
           END OF mt_dict,

           mt_set TYPE HASHED TABLE OF mt_dict
                  WITH UNIQUE KEY primary_key COMPONENTS otype objid.

    CONSTANTS: BEGIN OF wegid,
                 us TYPE wegid VALUE 'US_C_US',
               END OF wegid.

    CONSTANTS: BEGIN OF plvar,
                 us TYPE plvar VALUE '01',
               END OF plvar.

    CONSTANTS: BEGIN OF otype_enum,
                 user     TYPE otype VALUE 'US',
                 position TYPE otype VALUE 'S',
               END OF otype_enum.

    CLASS-DATA mts TYPE mt_set.

    DATA: otype        TYPE otype,
          objid        TYPE actorid,
          us_rh_read   TYPE abap_bool,
          us_rh_objecs TYPE hap_t_objec.

    METHODS constructor
      IMPORTING otype TYPE otype
                objid TYPE actorid.

    METHODS get_us_rh_struc EXPORTING objecs TYPE REF TO hap_t_objec.

    METHODS get_us_rh_obj_txt_sum
      IMPORTING otype         TYPE otype
      RETURNING VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_mm_wf_role IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( mts[ KEY primary_key
                               otype = otype
                               objid = objid ] ).

      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( otype = otype
                        objid = objid
                        obj   = NEW #( otype = otype
                                       objid = objid ) )
               INTO TABLE mts REFERENCE INTO mt.
    ENDTRY.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_approver_full_name.
    result = get_us_rh_obj_txt_sum( me->otype_enum-user ).
  ENDMETHOD.

  METHOD get_position_long_text.
    result = get_us_rh_obj_txt_sum( me->otype_enum-position ).
  ENDMETHOD.

  METHOD get_approver_objec.
    get_us_rh_struc( IMPORTING objecs = DATA(objecs) ).

    TRY.
        result = objecs->*[ otype = me->otype_enum-user ].
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD get_approver_bname.
    result = get_approver_objec( )-realo.
  ENDMETHOD.

  METHOD get_approver_objecs.
    get_us_rh_struc( IMPORTING objecs = DATA(objecs) ).

    result = VALUE #( FOR _obj IN objecs->*
                      WHERE ( otype = me->otype_enum-user )
                      ( _obj ) ).
  ENDMETHOD.

  METHOD get_position_objec.
    get_us_rh_struc( IMPORTING objecs = DATA(objecs) ).

    TRY.
        result = objecs->*[ otype = me->otype_enum-position ].
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    me->otype = otype.
    me->objid = objid.
  ENDMETHOD.

  METHOD get_us_rh_struc.
    objecs = REF #( me->us_rh_objecs ).
    CHECK me->us_rh_read = abap_false.

    ##FM_SUBRC_OK
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING  act_otype      = me->otype
                 act_objid      = me->objid
                 act_wegid      = me->wegid-us
                 act_plvar      = me->plvar-us
      TABLES     result_objec   = objecs->*
      EXCEPTIONS no_plvar_found = 1
                 no_entry_found = 2
                 OTHERS         = 3.

    me->us_rh_read = abap_true.
  ENDMETHOD.

  METHOD get_us_rh_obj_txt_sum.
    get_us_rh_struc( IMPORTING objecs = DATA(objecs) ).
    result = VALUE #( objecs->*[ otype = otype ]-stext+0(27) OPTIONAL ).
  ENDMETHOD.
ENDCLASS.