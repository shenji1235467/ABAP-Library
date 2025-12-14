CLASS zcl_bc_date DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF date_enum,
                 evermore TYPE sydatum VALUE '00010101',
                 forever  TYPE sydatum VALUE '99991231',
               END OF date_enum.

    DATA date TYPE dats READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !date         TYPE dats
      RETURNING VALUE(result) TYPE REF TO zcl_bc_date.

    METHODS get_month_end           RETURNING VALUE(result) TYPE sydatum.

    METHODS get_prev_month_last_day RETURNING VALUE(result) TYPE sydatum.

    METHODS add_months_correctively
      IMPORTING months        TYPE i
      RETURNING VALUE(result) TYPE sydatum.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_map,
             date TYPE dats,
             obj  TYPE REF TO zcl_bc_date,
           END OF multiton_map,

           multiton_set TYPE HASHED TABLE OF multiton_map
                        WITH UNIQUE KEY primary_key COMPONENTS date.

    TYPES: BEGIN OF amc_result_map,
             months TYPE i,
             result TYPE sydatum,
           END OF amc_result_map,

           amc_result_set TYPE HASHED TABLE OF amc_result_map
                          WITH UNIQUE KEY primary_key COMPONENTS months.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: month_end_cache           TYPE sydatum,
          prev_month_last_day_cache TYPE sydatum,
          amc_results_cache         TYPE amc_result_set.

    METHODS constructor IMPORTING !date TYPE dats.
ENDCLASS.


CLASS zcl_bc_date IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN zcl_bc_date=>multitons[ KEY primary_key COMPONENTS date = date ] TO FIELD-SYMBOL(<mt>).
    IF sy-subrc <> 0.
      INSERT VALUE #( date = date
                      obj  = NEW #( date ) )
             INTO TABLE zcl_bc_date=>multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD get_month_end.
    IF me->month_end_cache IS INITIAL.
      cl_reca_date=>get_date_info( EXPORTING id_date           = me->date
                                   IMPORTING ed_date_month_end = me->month_end_cache ).
    ENDIF.

    result = me->month_end_cache.
  ENDMETHOD.

  METHOD get_prev_month_last_day.
    IF me->prev_month_last_day_cache IS INITIAL.
      me->prev_month_last_day_cache       = me->date.
      me->prev_month_last_day_cache+6(2)  = '01'.
      me->prev_month_last_day_cache      -= 1.
    ENDIF.

    result = me->prev_month_last_day_cache.
  ENDMETHOD.

  METHOD add_months_correctively.
    ASSIGN me->amc_results_cache[ KEY primary_key
                                  months = months ] TO FIELD-SYMBOL(<cache>).
    IF sy-subrc <> 0.
      INSERT VALUE #( months = months ) INTO TABLE me->amc_results_cache ASSIGNING <cache>.

      DO 1 TIMES.
        IF months IS INITIAL.
          <cache>-result = me->date.
          EXIT.
        ENDIF.

        DATA(year)  = me->date+0(4).
        DATA(month) = CONV i( me->date+4(2) ).
        DATA(day)   = me->date+6(2).

        month += <cache>-months.

        WHILE month > 12.
          year += 1.
          month -= 12.
        ENDWHILE.

        DATA(year_mod_4) = CONV i( year ) MOD 4.

        day = SWITCH #( month
                        WHEN 2                 THEN COND #( WHEN year_mod_4 IS INITIAL AND day > 29     THEN 29
                                                            WHEN year_mod_4 IS NOT INITIAL AND day > 28 THEN 28
                                                            ELSE                                             day )

                        WHEN 4 OR 6 OR 9 OR 11 THEN COND #( WHEN day > 30 THEN 30 ELSE day )

                        ELSE                        day ).

        <cache>-result = |{ year }{ CONV numc2( month ) }{ day }|.

      ENDDO.
    ENDIF.

    result = <cache>-result.
  ENDMETHOD.

  METHOD constructor.
    me->date = date.
  ENDMETHOD.
ENDCLASS.