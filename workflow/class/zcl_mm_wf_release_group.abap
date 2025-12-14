CLASS zcl_mm_wf_release_group DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING frggr         TYPE frggr
      RETURNING VALUE(result) TYPE REF TO zcl_mm_wf_release_group
      RAISING   zcx_mm_wf_release_group.

    METHODS build_rel_cd_tx
      IMPORTING frgco         TYPE frgco
                werks         TYPE werks_d OPTIONAL
      RETURNING VALUE(result) TYPE zmmd_rel_cd_tx.

    METHODS get_wf_role
      IMPORTING frgco         TYPE frgco
                werks         TYPE werks_d OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_mm_wf_role
      RAISING   zcx_mm_wf_role.

  PRIVATE SECTION.
    TYPES: BEGIN OF mt_dict, " Multiton
             frggr TYPE frggr,
             obj   TYPE REF TO zcl_mm_wf_release_group,
           END OF mt_dict,

           mt_set TYPE HASHED TABLE OF mt_dict WITH UNIQUE KEY primary_key COMPONENTS frggr.

    TYPES: BEGIN OF rel_cd_tx_cache_dict,
             frgco     TYPE frgco,
             werks     TYPE werks_d,
             rel_cd_tx TYPE zmmd_rel_cd_tx,
           END OF rel_cd_tx_cache_dict,

           rel_cd_tx_cache_set TYPE HASHED TABLE OF rel_cd_tx_cache_dict
                                WITH UNIQUE KEY primary_key COMPONENTS frgco werks.

    TYPES: BEGIN OF wf_role_cache_dict,
             frgco TYPE frgco,
             werks TYPE werks_d,
             obj   TYPE REF TO zcl_mm_wf_role,
           END OF wf_role_cache_dict,

           wf_role_cache_set TYPE HASHED TABLE OF wf_role_cache_dict
                             WITH UNIQUE KEY primary_key COMPONENTS frgco werks.

    CLASS-DATA mts TYPE mt_set.

    DATA: frggr           TYPE frggr,
          rel_cd_tx_cache TYPE rel_cd_tx_cache_set,
          wf_role_cache   TYPE wf_role_cache_set.

    METHODS constructor
      IMPORTING frggr TYPE frggr
      RAISING   zcx_mm_wf_release_group.
ENDCLASS.


CLASS zcl_mm_wf_release_group IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( mts[ KEY primary_key
                               frggr = frggr ] ).

      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( frggr = frggr
                        obj   = NEW #( frggr ) ) INTO TABLE mts REFERENCE INTO mt.
    ENDTRY.

    result = mt->obj.
  ENDMETHOD.

  METHOD build_rel_cd_tx.
    TRY.
        DATA(cache) = REF #( me->rel_cd_tx_cache[ KEY primary_key
                                                  frgco = frgco
                                                  werks = werks ] ).
      CATCH cx_sy_itab_line_not_found.
        DATA(new_cache) = VALUE rel_cd_tx_cache_dict( frgco = frgco
                                                      werks = werks ).

        DO 1 TIMES.
          TRY.
              DATA(wf_role) = get_wf_role( frgco = frgco
                                           werks = werks ).

            CATCH zcx_mm_wf_role.
              EXIT.
          ENDTRY.

          DATA(position_name) = wf_role->get_position_long_text( ).
          DATA(approver_name) = wf_role->get_approver_full_name( ).
          new_cache-rel_cd_tx = |{ position_name } / { approver_name }|.
        ENDDO.

        INSERT new_cache INTO TABLE me->rel_cd_tx_cache REFERENCE INTO cache.
    ENDTRY.

    result = cache->rel_cd_tx.
  ENDMETHOD.

  METHOD get_wf_role.
    TRY.
        DATA(cache) = REF #( me->wf_role_cache[ KEY primary_key
                                                frgco = frgco
                                                werks = werks ] ).

      CATCH cx_sy_itab_line_not_found.
        SELECT SINGLE FROM t16fw
               FIELDS otype, objid
               WHERE frggr = @me->frggr
                 AND frgco = @frgco
                 AND werks = @werks
               INTO @DATA(t16fw).

        IF NOT ( sy-subrc = 0 AND t16fw-otype IS NOT INITIAL AND t16fw-objid IS NOT INITIAL ).
          RAISE EXCEPTION NEW zcx_mm_wf_role( textid = zcx_mm_wf_role=>rel_grp_doesnt_contain_role
                                              frggr  = me->frggr
                                              frgco  = frgco
                                              werks  = werks ).
        ENDIF.

        INSERT VALUE #( frgco = frgco
                        werks = werks
                        obj   = CAST #( zcl_mm_wf_role=>get_instance( otype = t16fw-otype
                                                                      objid = t16fw-objid ) ) )
               INTO TABLE me->wf_role_cache REFERENCE INTO cache.
    ENDTRY.

    result = cache->obj.
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE FROM t16fg
           FIELDS @abap_true
           WHERE frggr = @frggr
           INTO @DATA(rel_grp_exists).

    IF rel_grp_exists = abap_false.
      RAISE EXCEPTION NEW zcx_mm_wf_release_group( textid = zcx_mm_wf_release_group=>undefined
                                                   frggr  = frggr ).
    ENDIF.

    me->frggr = frggr.
  ENDMETHOD.
ENDCLASS.