calculate_proforma_projections <- function(revenue_growth_rates, expense_ratios,
                                           fixed_assets_params, working_capital_ratios,
                                           balance_sheet_ratios, interest_rates, tax_rates) {
  
  calculate_2024_revenue <- function() {
    q1_idx <- which(df_is$Period == "2024_Q1")
    q4_idx <- which(df_is$Period == "2024_Q4")
    total_2024_revenue <- sum(df_is$Total_revenue[q1_idx:q4_idx])
    return(total_2024_revenue)
  }
  
  projection_start_idx <- which(df_is$Period == projection_start)
  
  for (i in projection_start_idx:nrow(df_is)) {
    current_period <- df_is$Period[i]
    period_name <- get_period(current_period)
    prev_period <- i - 1
    
    if (current_period == projection_start) {
      for (region in revenue_regions) {
        q3_revenue <- df_is[[region]][i-1]  
        df_is[[region]][i] <- q3_revenue * (1 + revenue_growth_rates[[region]]$period_1)
      }
    } else {
      for (region in revenue_regions) {
        if (current_period == "2025") {
          q1_idx <- which(df_is$Period == "2024_Q1")
          q4_idx <- which(df_is$Period == "2024_Q4")
          year_2024_revenue <- sum(df_is[[region]][q1_idx:q4_idx])
          df_is[[region]][i] <- year_2024_revenue * 
            (1 + revenue_growth_rates[[region]][[period_name]])
        } else {
          df_is[[region]][i] <- df_is[[region]][i-1] * 
            (1 + revenue_growth_rates[[region]][[period_name]])
        }
      }
    }
    
    df_is$Total_revenue[i] <- rowSums(df_is[i, revenue_regions])

    if (current_period == projection_start) {
      revenue_base <- calculate_2024_revenue()
    } else {
      revenue_base <- df_is$Total_revenue[i]
    }
    
    df_is$Materials_and_services[i] <- df_is$Total_revenue[i] * 
      expense_ratios$materials_and_services[[period_name]]
    df_is$Employee_benefits[i] <- df_is$Total_revenue[i] * 
      expense_ratios$employee_benefits[[period_name]]
    df_is$Other_operating_expenses[i] <- df_is$Total_revenue[i] * 
      expense_ratios$other_operating_expenses[[period_name]]
    
    df_is$EBITDA[i] <- df_is$Total_revenue[i] - 
      df_is$Materials_and_services[i] - 
      df_is$Employee_benefits[i] - 
      df_is$Other_operating_expenses[i]

    df_bs$Inventories[i] <- revenue_base * 
      working_capital_ratios$inventory_to_revenue[[period_name]]
    df_bs$Trade_receivables[i] <- revenue_base * 
      working_capital_ratios$trade_receivables_to_revenue[[period_name]]
    df_bs$Other_current_assets[i] <- revenue_base * 
      working_capital_ratios$other_current_assets_to_revenue[[period_name]]
    df_bs$Other_non_current_assets[i] <- revenue_base * 
      balance_sheet_ratios$other_non_current_assets_to_revenue[[period_name]]
    df_bs$Trade_payables[i] <- revenue_base * 
      working_capital_ratios$trade_payables_to_revenue[[period_name]]
    df_bs$Other_current_liabilities[i] <- revenue_base * 
      working_capital_ratios$other_current_liab_to_revenue[[period_name]]
    df_bs$Other_non_current_liabilities[i] <- revenue_base * 
      working_capital_ratios$other_non_current_liab_to_revenue[[period_name]]
    
    if (current_period == projection_start) {
      df_bs$Intangible_assets[i] <- df_bs$Intangible_assets[prev_period] * 
        (1 + fixed_assets_params$growth_rates$intangible_assets[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$intangible_assets[[period_name]] / 4)
      
      df_bs$PP_and_A_owned[i] <- df_bs$PP_and_A_owned[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_owned[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]] / 4)
      
      df_bs$PP_and_A_ROU[i] <- df_bs$PP_and_A_ROU[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4)

      df_is$Depreciation_and_amortization[i] <- (
        df_bs$Intangible_assets[prev_period] * 
          fixed_assets_params$depreciation_rates$intangible_assets[[period_name]] / 4 +
          df_bs$PP_and_A_owned[prev_period] * 
          fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]] / 4 +
          df_bs$PP_and_A_ROU[prev_period] * 
          fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4
      )

      rou_growth <- fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]] / 4
      rou_depr <- fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4
      lease_growth <- (1 + rou_growth) * (1 - rou_depr)
      
      df_bs$Lease_liabilities_current[i] <- df_bs$Lease_liabilities_current[prev_period] * 
        lease_growth
      df_bs$Lease_liabilities_non_current[i] <- df_bs$Lease_liabilities_non_current[prev_period] * 
        lease_growth
      
    } else {
      df_bs$Intangible_assets[i] <- df_bs$Intangible_assets[prev_period] * 
        (1 + fixed_assets_params$growth_rates$intangible_assets[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$intangible_assets[[period_name]])
      
      df_bs$PP_and_A_owned[i] <- df_bs$PP_and_A_owned[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_owned[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]])
      
      df_bs$PP_and_A_ROU[i] <- df_bs$PP_and_A_ROU[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]])
      
      df_is$Depreciation_and_amortization[i] <- (
        df_bs$Intangible_assets[prev_period] * 
          (1 + fixed_assets_params$growth_rates$intangible_assets[[period_name]]) *
          fixed_assets_params$depreciation_rates$intangible_assets[[period_name]] +
          df_bs$PP_and_A_owned[prev_period] * 
          (1 + fixed_assets_params$growth_rates$pp_and_a_owned[[period_name]]) * 
          fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]] +
          df_bs$PP_and_A_ROU[prev_period] * 
          (1 + fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]]) *
          fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]]
      )

      rou_growth <- fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]]
      rou_depr <- fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]]
      lease_growth <- (1 + rou_growth) * (1 - rou_depr)
      
      df_bs$Lease_liabilities_current[i] <- df_bs$Lease_liabilities_current[prev_period] * 
        lease_growth
      df_bs$Lease_liabilities_non_current[i] <- df_bs$Lease_liabilities_non_current[prev_period] * 
        lease_growth
    }

    df_is$EBIT[i] <- df_is$EBITDA[i] - df_is$Depreciation_and_amortization[i]

    df_bs$Total_Operating_Assets[i] <- sum(df_bs[i, bs_operating_asset_categories])

    df_bs$Reserve_for_invested_equity[i] <- df_bs$Reserve_for_invested_equity[1] 
    
    df_bs$Other_equity[i] <- df_bs$Total_Assets[prev_period] * 
      balance_sheet_ratios$other_equity_to_assets[[period_name]]
    
    df_bs$Short_term_loans[i] <- df_bs$Total_Assets[prev_period] * 
      balance_sheet_ratios$short_term_loans_to_assets[[period_name]]
    df_bs$Long_term_loans[i] <- df_bs$Total_Assets[prev_period] * 
      balance_sheet_ratios$long_term_loans_to_assets[[period_name]]
    
    df_bs$Provisions[i] <- df_bs$Inventories[i] * 
      balance_sheet_ratios$provisions_to_inventory[[period_name]]
    df_bs$Non_current_provisions[i] <- df_bs$Inventories[i] * 
      balance_sheet_ratios$non_current_provisions_to_inventory[[period_name]]

    df_bs$Total_Interest_Bearing_Debt[i] <- sum(df_bs[i, bs_interest_bearing_debt_categories])

    df_is$Net_interest[i] <- df_bs$Cash_and_equivalents[prev_period] * 
      interest_rates$savings_interest[[period_name]] -
      df_bs$Total_Interest_Bearing_Debt[i] * interest_rates$debt_interest[[period_name]]

    df_is$EBT[i] <- df_is$EBIT[i] + df_is$Net_interest[i]
    df_is$Corporate_income_tax[i] <- df_is$EBT[i] * tax_rates[[period_name]]
    df_is$Net_income[i] <- df_is$EBT[i] - df_is$Corporate_income_tax[i]

    df_bs$Retained_earnings[i] <- df_bs$Retained_earnings[prev_period] + 
      df_is$Net_income[i]

    df_bs$Total_Liabilities_Equity[i] <- sum(df_bs[i, bs_liability_equity_categories])

    df_bs$Cash_and_equivalents[i] <- df_bs$Total_Liabilities_Equity[i] - 
      df_bs$Total_Operating_Assets[i]

    df_bs$Total_Assets[i] <- df_bs$Total_Operating_Assets[i] + 
      df_bs$Cash_and_equivalents[i]
  }
  
  return(TRUE)
}

run_projections_default <- function() {
  return(calculate_proforma_projections(revenue_growth_rates, expense_ratios,
                                        fixed_assets_params, working_capital_ratios,
                                        balance_sheet_ratios, interest_rates, tax_rates))
}
