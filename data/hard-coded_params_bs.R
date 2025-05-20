fixed_assets_params <- list(
  growth_rates = list(
    intangible_assets = list(
      period_1 = 0.6761,
      period_2 = 0.6761,
      period_3 = 0.6761,
      period_4 = 0.025
    ),
    pp_and_a_owned = list(
      period_1 = 0.3018,
      period_2 = 0.3832,
      period_3 = 0.1933,
      period_4 = 0.500
    ),
    pp_and_a_rou = list(
      period_1 = 0.4336,
      period_2 = 0.4336,
      period_3 = 0.4336,
      period_4 = 0.500
    )
  ),

  depreciation_rates = list(
    intangible_assets = list(
      period_1 = 0.2244,
      period_2 = 0.2244,
      period_3 = 0.2244,
      period_4 = 0.2244
    ),
    pp_and_a_owned = list(
      period_1 = 0.0782,
      period_2 = 0.0782,
      period_3 = 0.0782,
      period_4 = 0.0782
    ),
    pp_and_a_rou = list(
      period_1 = 0.2327,
      period_2 = 0.2327,
      period_3 = 0.2327,
      period_4 = 0.2327
    )
  )
)

working_capital_ratios <- list(
  inventory_to_revenue = list(
    period_1 = 0.2,
    period_2 = 0.2,
    period_3 = 0.2,
    period_4 = 0.2
  ),
  trade_receivables_to_revenue = list(
    period_1 = 0.17,
    period_2 = 0.17,
    period_3 = 0.17,
    period_4 = 0.17
  ),
  other_current_assets_to_revenue = list(
    period_1 = 0.038,
    period_2 = 0.038,
    period_3 = 0.038,
    period_4 = 0.038
  ),

  trade_payables_to_revenue = list(
    period_1 = 0.14,
    period_2 = 0.14,
    period_3 = 0.14,
    period_4 = 0.14
  ),
  other_current_liab_to_revenue = list(
    period_1 = 0.13,
    period_2 = 0.13,
    period_3 = 0.13,
    period_4 = 0.13
  ),
  other_non_current_liab_to_revenue = list(
    period_1 = 0.004,
    period_2 = 0.004,
    period_3 = 0.004,
    period_4 = 0.004
  )
)

balance_sheet_ratios <- list(
  other_non_current_assets_to_revenue = list(
    period_1 = 0.018,
    period_2 = 0.018,
    period_3 = 0.018,
    period_4 = 0.018
  ),

  short_term_loans_to_assets = list(
    period_1 = 0.032,
    period_2 = 0.032,
    period_3 = 0.032,
    period_4 = 0.032
  ),
  long_term_loans_to_assets = list(
    period_1 = 0.005,
    period_2 = 0.005,
    period_3 = 0.005,
    period_4 = 0.005
  ),

  lease_liability_growth = list(
    current = lapply(fixed_assets_params$growth_rates$pp_and_a_rou, 
                     function(x) (1 + x) * (1 - fixed_assets_params$depreciation_rates$pp_and_a_rou$period_1)),
    non_current = lapply(fixed_assets_params$growth_rates$pp_and_a_rou,
                         function(x) (1 + x) * (1 - fixed_assets_params$depreciation_rates$pp_and_a_rou$period_1))
  ),

  provisions_to_inventory = list(
    period_1 = 0.170,
    period_2 = 0.170,
    period_3 = 0.170,
    period_4 = 0.170
  ),
  non_current_provisions_to_inventory = list(
    period_1 = 0.045,
    period_2 = 0.045,
    period_3 = 0.045,
    period_4 = 0.045
  ),

  other_equity_to_assets = list(
    period_1 = -0.024,
    period_2 = -0.024,
    period_3 = -0.024,
    period_4 = -0.024
  )
)

get_bs_rate <- function(rates_list, category, subcategory = NULL, period) {
  if (is.null(subcategory)) {
    return(rates_list[[category]][[period]])
  } else {
    return(rates_list[[category]][[subcategory]][[period]])
  }
}
