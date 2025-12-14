CLASS zcl_mm_material_quantity DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA: matnr TYPE matnr READ-ONLY,
          menge TYPE mengv READ-ONLY,
          meins TYPE meins READ-ONLY.

    CLASS-METHODS convert_mat_quantity
      IMPORTING matnr         TYPE matnr
                menge         TYPE mengv
                from_uom      TYPE meins
                to_uom        TYPE meins
      RETURNING VALUE(result) TYPE mengv
      RAISING   zcx_mm_material_unit_conv.

    METHODS constructor
      IMPORTING matnr TYPE matnr
                meins TYPE meins
                menge TYPE mengv OPTIONAL
      RAISING   zcx_mm_material
                zcx_bc_uom.

    METHODS add IMPORTING menge TYPE mengv
                          meins TYPE meins
                RAISING   zcx_mm_material_unit_conv.

    METHODS convert_to_base_uom RETURNING VALUE(result) TYPE REF TO zcl_mm_material_quantity
                                RAISING   zcx_mm_material_unit_conv.

  PRIVATE SECTION.
    TYPES: BEGIN OF matnr_vld_map,
             matnr TYPE matnr,
             error TYPE REF TO zcx_mm_material,
           END OF matnr_vld_map,

           matnr_vld_set TYPE HASHED TABLE OF matnr_vld_map
                         WITH UNIQUE KEY primary_key COMPONENTS matnr.

    TYPES: BEGIN OF meins_vld_map,
             meins TYPE meins,
             error TYPE REF TO zcx_bc_uom,
           END OF meins_vld_map,

           meins_vld_set TYPE HASHED TABLE OF meins_vld_map
                         WITH UNIQUE KEY primary_key COMPONENTS meins.

    TYPES: BEGIN OF material_map,
             matnr TYPE mara-matnr,
             meins TYPE mara-meins,
           END OF material_map,

           material_set TYPE HASHED TABLE OF material_map WITH UNIQUE KEY matnr.

    CLASS-DATA: matnr_vlds_cache TYPE matnr_vld_set,
                meins_vlds_cache TYPE meins_vld_set,
                materials_cache  TYPE material_set.

    CLASS-METHODS validate_matnr
      IMPORTING matnr TYPE matnr
      RAISING   zcx_mm_material.

    CLASS-METHODS validate_meins
      IMPORTING meins TYPE meins
      RAISING   zcx_bc_uom.

    CLASS-METHODS obtain_material
      IMPORTING matnr         TYPE matnr
      RETURNING VALUE(result) TYPE REF TO material_map
      RAISING   zcx_mm_material.

ENDCLASS.


CLASS zcl_mm_material_quantity IMPLEMENTATION.
  METHOD convert_mat_quantity.
    CHECK menge IS NOT INITIAL.

    IF from_uom = to_uom.
      result = menge.
      RETURN.
    ENDIF.

    TRY.
        validate_matnr( matnr ).
        validate_meins( from_uom ).
        validate_meins( to_uom ).

      CATCH cx_root INTO DATA(prep_error).
        RAISE EXCEPTION NEW zcx_mm_material_unit_conv( matnr    = matnr
                                                       src_uom  = from_uom
                                                       tar_uom  = to_uom
                                                       previous = prep_error ).
    ENDTRY.

    DATA(i_menge) = CONV bstmg( abs( menge ) ).
    DATA(e_menge) = CONV bstmg( 0 ).

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING  i_matnr              = matnr
                 i_in_me              = from_uom
                 i_out_me             = to_uom
                 i_menge              = i_menge
      IMPORTING  e_menge              = e_menge
      EXCEPTIONS error_in_application = 1
                 error                = 2
                 OTHERS               = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mm_material_unit_conv( matnr   = matnr
                                                     src_uom = from_uom
                                                     tar_uom = to_uom ).
    ENDIF.

    result = COND #( WHEN menge < 0
                     THEN e_menge * -1
                     ELSE e_menge ).
  ENDMETHOD.

  METHOD constructor.
    validate_matnr( matnr ).
    validate_meins( meins ).

    me->matnr = matnr.
    me->menge = menge.
    me->meins = meins.
  ENDMETHOD.

  METHOD add.
    CHECK menge IS NOT INITIAL.

    DATA(converted_menge) = convert_mat_quantity( matnr    = me->matnr
                                                  menge    = menge
                                                  from_uom = meins
                                                  to_uom   = me->meins ).

    me->menge += converted_menge.
  ENDMETHOD.

  METHOD convert_to_base_uom.
    TRY.
        DATA(material) = obtain_material( me->matnr ).

        IF material->meins = me->meins.
          result = me.
          RETURN.
        ENDIF.

        DATA(converted_quan) = convert_mat_quantity( matnr    = me->matnr
                                                     menge    = me->menge
                                                     from_uom = me->meins
                                                     to_uom   = material->meins ).

        result = NEW #( matnr = me->matnr
                        menge = converted_quan
                        meins = material->meins ).

      CATCH zcx_mm_material_unit_conv INTO DATA(conv_error).
        RAISE EXCEPTION conv_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid   = zcx_mm_material_unit_conv=>unexpected_problem
                                                       previous = diaper
                                                       matnr    = me->matnr ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_matnr.
    IF matnr IS INITIAL.
      RAISE EXCEPTION NEW zcx_mm_material( textid = zcx_mm_material=>matnr_empty ).
    ENDIF.

    ASSIGN matnr_vlds_cache[ KEY primary_key
                             matnr = matnr ] TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      DATA(new_cache) = VALUE matnr_vld_map( matnr = matnr ).

      SELECT SINGLE FROM mara
             FIELDS @abap_true
             WHERE matnr = @new_cache-matnr
             INTO @DATA(material_exists).

      IF material_exists = abap_false.
        new_cache-error = NEW #( textid = zcx_mm_material=>undefined
                                 matnr  = new_cache-matnr ).
      ENDIF.

      INSERT new_cache INTO TABLE matnr_vlds_cache ASSIGNING <cache>.
    ENDIF.

    CHECK <cache>-error IS NOT INITIAL.
    RAISE EXCEPTION <cache>-error.
  ENDMETHOD.

  METHOD validate_meins.
    IF meins IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_uom( textid = zcx_bc_uom=>meins_empty ).
    ENDIF.

    ASSIGN meins_vlds_cache[ KEY primary_key
                             meins = meins ] TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      DATA(new_cache) = VALUE meins_vld_map( meins = meins ).

      SELECT SINGLE FROM t006
             FIELDS @abap_true
             WHERE msehi = @new_cache-meins
             INTO @DATA(uom_exists).

      IF uom_exists = abap_false.
        new_cache-error = NEW #( textid = zcx_bc_uom=>undefined
                                 meins  = new_cache-meins ).
      ENDIF.

      INSERT new_cache INTO TABLE meins_vlds_cache ASSIGNING <cache>.
    ENDIF.

    CHECK <cache>-error IS NOT INITIAL.
    RAISE EXCEPTION <cache>-error.
  ENDMETHOD.

  METHOD obtain_material.
    IF matnr IS INITIAL.
      RAISE EXCEPTION NEW zcx_mm_material( textid = zcx_mm_material=>matnr_empty ).
    ENDIF.

    ASSIGN materials_cache[ KEY primary_key
                            matnr = matnr ] TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      DATA(new_cache) = VALUE material_map( matnr = matnr ).

      SELECT SINGLE FROM mara
             FIELDS meins
             WHERE matnr = @new_cache-matnr
             INTO @new_cache-meins.

      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_mm_material( textid = zcx_mm_material=>undefined
                                             matnr  = new_cache-matnr ).
      ENDIF.

      INSERT new_cache INTO TABLE materials_cache ASSIGNING <cache>.
    ENDIF.

    result = REF #( <cache> ).
  ENDMETHOD.
ENDCLASS.