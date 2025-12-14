CLASS zcl_fi_acc_doc_install_plan DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: amount_type      TYPE wrbtr,
           amount_long_type TYPE bapidoccur_31.

    TYPES:
      BEGIN OF installment_map,
        installment  TYPE i,a
        payment_date TYPE datum,
        amount       TYPE amount_type,
        amount_long  TYPE amount_long_type,
      END OF installment_map,

      installment_array TYPE SORTED TABLE OF installment_map WITH UNIQUE KEY installment.

    TYPES: BEGIN OF param_map,
             total_amount       TYPE amount_type,
             total_amount_long  TYPE amount_long_type,
             installment_count  TYPE i,
             first_payment_date TYPE datum,
           END OF param_map.

    DATA rows TYPE installment_array READ-ONLY.

    METHODS constructor IMPORTING param TYPE param_map.

  PRIVATE SECTION.
    DATA param TYPE param_map.

ENDCLASS.


CLASS zcl_fi_acc_doc_install_plan IMPLEMENTATION.
  METHOD constructor.
    me->param = param.

    DATA(doc_date) = zcl_bc_date=>get_instance( param-first_payment_date ).

    DATA(usable_installment_count) = COND i( WHEN param-installment_count > 1
                                             THEN param-installment_count
                                             ELSE 1 ).

    DATA(installment) = 1.

    DATA(amount_per_installment) = CONV amount_type( param-total_amount / usable_installment_count ).
    DATA(remaining_amount) = param-total_amount.

    DATA(amount_long_per_installlment) = CONV amount_long_type( param-total_amount_long / usable_installment_count ).
    DATA(remaining_amount_long) = param-total_amount_long.

    WHILE installment <= usable_installment_count.
      DATA(installment_amount) = COND amount_type(
        WHEN installment = param-installment_count
        THEN remaining_amount " Küsürat kalmasın
        ELSE amount_per_installment ).

      DATA(installment_amount_long) = COND amount_long_type(
        WHEN installment = param-installment_count
        THEN remaining_amount_long " Küsürat kalmasın
        ELSE amount_long_per_installlment ).

      DATA(bline_date) = doc_date->add_months_correctively( installment - 1 ).

      INSERT VALUE #( installment  = installment
                      payment_date = bline_date
                      amount       = installment_amount
                      amount_long  = installment_amount_long )
             INTO TABLE me->rows.

      installment           += 1.
      remaining_amount      -= installment_amount.
      remaining_amount_long -= installment_amount_long.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.