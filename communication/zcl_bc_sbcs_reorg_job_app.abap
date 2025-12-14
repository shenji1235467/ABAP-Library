CLASS zcl_bc_sbcs_reorg_job_app DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF reorg_input_map,
             begda TYPE begda,
             endda TYPE endda,
           END OF reorg_input_map.

    METHODS run IMPORTING reorg_input TYPE reorg_input_map.

  PRIVATE SECTION.
    TYPES crdat_range TYPE RANGE OF sood-crdat.

    METHODS submit_sbcs_reorg IMPORTING crdat TYPE sood-crdat.

ENDCLASS.


CLASS zcl_bc_sbcs_reorg_job_app IMPLEMENTATION.
  METHOD run.
    DATA(date_cursor) = reorg_input-endda.

    WHILE date_cursor >= reorg_input-begda.
      submit_sbcs_reorg( date_cursor ).
      date_cursor -= 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD submit_sbcs_reorg.
    DATA(crdat_rng) = VALUE crdat_range( ( sign   = ycl_addict_toolkit=>sign-include
                                           option = ycl_addict_toolkit=>option-eq
                                           low    = crdat ) ).

    SUBMIT rsbcs_reorg "#EC CI_SUBMIT
           WITH srdel = abap_true
           WITH docdel = abap_true
           WITH absolute = abap_true
           WITH relative = abap_false
           WITH created IN crdat_rng
           WITH showlog = abap_false
           WITH testmode = abap_false
           AND RETURN.
  ENDMETHOD.
ENDCLASS.