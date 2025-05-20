calculate_proforma_projections <- function(df_is, df_bs, revenue_growth_rates, expense_ratios,
                                         fixed_assets_params, working_capital_ratios,
                                         balance_sheet_ratios, interest_rates, tax_rates) {
  
  # Helper function for period determination
  get_period <- function(year) {
    if (year %in% c("2024_Q4", "2025")) return("period_1")
    if (year %in% as.character(2026:2028)) return("period_2")
    if (year %in% as.character(2029:2030)) return("period_3")
    if (year == "terminal") return("period_4")
    stop(paste("Invalid year:", year))
  }
  
  # Helper function to calculate full year 2024 revenue
  calculate_2024_revenue <- function() {
    q1_idx <- which(df_is$Period == "2024_Q1")
    q4_idx <- which(df_is$Period == "2024_Q4")
    total_2024_revenue <- sum(df_is$Total_revenue[q1_idx:q4_idx])
    return(total_2024_revenue)
  }
  
  # Start projections from Q4 2024
  projection_start <- which(df_is$Period == "2024_Q4")
  
  # Revenue regions for calculations
  revenue_regions <- c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World")
  
  # Main projection loop
  for (i in projection_start:nrow(df_is)) {
    current_period <- df_is$Period[i]
    period_name <- get_period(current_period)
    prev_period <- i - 1
    
    # 1. Project revenue
    if (current_period == "2024_Q4") {
      # Q4 should grow from Q3 at the period growth rate
      for (region in revenue_regions) {
        q3_revenue <- df_is[[region]][i-1]  # Q3 2024 revenue
        df_is[[region]][i] <- q3_revenue * (1 + revenue_growth_rates[[region]]$period_1)
      }
    } else {
      # Project revenue for other periods
      for (region in revenue_regions) {
        if (current_period == "2025") {
          # Calculate full year 2024 revenue (Q1-Q4)
          q1_idx <- which(df_is$Period == "2024_Q1")
          q4_idx <- which(df_is$Period == "2024_Q4")
          year_2024_revenue <- sum(df_is[[region]][q1_idx:q4_idx])
          # Apply growth to full year revenue
          df_is[[region]][i] <- year_2024_revenue * 
            (1 + revenue_growth_rates[[region]][[period_name]])
        } else {
          df_is[[region]][i] <- df_is[[region]][i-1] * 
            (1 + revenue_growth_rates[[region]][[period_name]])
        }
      }
    }
    
    # Calculate total revenue
    df_is$Total_revenue[i] <- rowSums(df_is[i, revenue_regions])
    
    # For balance sheet calculations, use different revenue base depending on period
    if (current_period == "2024_Q4") {
      # For Q4 2024, use the full year 2024 revenue for ratio calculations
      revenue_base <- calculate_2024_revenue()
    } else {
      revenue_base <- df_is$Total_revenue[i]
    }
    
    # 2-3. Calculate operating expenses and EBITDA
    df_is$Materials_and_services[i] <- df_is$Total_revenue[i] * 
      expense_ratios$materials_and_services[[period_name]]
    df_is$Employee_benefits[i] <- df_is$Total_revenue[i] * 
      expense_ratios$employee_benefits[[period_name]]
    df_is$Other_operating_expenses[i] <- df_is$Total_revenue[i] * 
      expense_ratios$other_operating_expenses[[period_name]]
    
    # EBITDA calculation
    df_is$EBITDA[i] <- df_is$Total_revenue[i] - 
      df_is$Materials_and_services[i] - 
      df_is$Employee_benefits[i] - 
      df_is$Other_operating_expenses[i]
    
    # 4. Project balance sheet items based on revenue_base
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
    
    # 5. Project fixed assets and related items
    if (current_period == "2024_Q4") {
      # For Q4 2024, use quarterly rates
      df_bs$Intangible_assets[i] <- df_bs$Intangible_assets[prev_period] * 
        (1 + fixed_assets_params$growth_rates$intangible_assets[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$intangible_assets[[period_name]] / 4)
      
      df_bs$PP_and_A_owned[i] <- df_bs$PP_and_A_owned[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_owned[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]] / 4)
      
      df_bs$PP_and_A_ROU[i] <- df_bs$PP_and_A_ROU[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4)
      
      # D&A calculation for Q4 2024
      df_is$Depreciation_and_amortization[i] <- (
        df_bs$Intangible_assets[prev_period] * 
          fixed_assets_params$depreciation_rates$intangible_assets[[period_name]] / 4 +
          df_bs$PP_and_A_owned[prev_period] * 
          fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]] / 4 +
          df_bs$PP_and_A_ROU[prev_period] * 
          fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4
      )
      
      # Lease liabilities for Q4 2024
      rou_growth <- fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]] / 4
      rou_depr <- fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4
      lease_growth <- (1 + rou_growth) * (1 - rou_depr)
      
      df_bs$Lease_liabilities_current[i] <- df_bs$Lease_liabilities_current[prev_period] * 
        lease_growth
      df_bs$Lease_liabilities_non_current[i] <- df_bs$Lease_liabilities_non_current[prev_period] * 
        lease_growth
      
    } else {
      # For annual periods, use annual rates
      df_bs$Intangible_assets[i] <- df_bs$Intangible_assets[prev_period] * 
        (1 + fixed_assets_params$growth_rates$intangible_assets[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$intangible_assets[[period_name]])
      
      df_bs$PP_and_A_owned[i] <- df_bs$PP_and_A_owned[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_owned[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]])
      
      df_bs$PP_and_A_ROU[i] <- df_bs$PP_and_A_ROU[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]])
      
      # D&A calculation for annual periods
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
      
      # Lease liabilities for annual periods
      rou_growth <- fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]]
      rou_depr <- fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]]
      lease_growth <- (1 + rou_growth) * (1 - rou_depr)
      
      df_bs$Lease_liabilities_current[i] <- df_bs$Lease_liabilities_current[prev_period] * 
        lease_growth
      df_bs$Lease_liabilities_non_current[i] <- df_bs$Lease_liabilities_non_current[prev_period] * 
        lease_growth
    }
    
    # 8. Calculate EBIT
    df_is$EBIT[i] <- df_is$EBITDA[i] - df_is$Depreciation_and_amortization[i]
    
    # Calculate total operating assets
    df_bs$Total_Operating_Assets[i] <- sum(df_bs[i, c("Intangible_assets", "PP_and_A_owned", 
                                                     "PP_and_A_ROU", "Inventories", 
                                                     "Trade_receivables", "Other_current_assets",
                                                     "Other_non_current_assets")])
    
    # 9-11. Project remaining balance sheet items
    df_bs$Reserve_for_invested_equity[i] <- df_bs$Reserve_for_invested_equity[1]  # Keep fixed
    
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
    
    # 12. Calculate total interest bearing debt
    df_bs$Total_Interest_Bearing_Debt[i] <- sum(df_bs[i, c("Short_term_loans", "Long_term_loans",
                                                          "Lease_liabilities_current", 
                                                          "Lease_liabilities_non_current")])
    
    # 13. Calculate net interest
    df_is$Net_interest[i] <- df_bs$Cash_and_equivalents[prev_period] * 
      interest_rates$savings_interest[[period_name]] -
      df_bs$Total_Interest_Bearing_Debt[i] * interest_rates$debt_interest[[period_name]]
    
    # 14-16. Calculate EBT, tax, and net income
    df_is$EBT[i] <- df_is$EBIT[i] + df_is$Net_interest[i]
    df_is$Corporate_income_tax[i] <- df_is$EBT[i] * tax_rates[[period_name]]
    df_is$Net_income[i] <- df_is$EBT[i] - df_is$Corporate_income_tax[i]
    
    # 17. Update retained earnings
    df_bs$Retained_earnings[i] <- df_bs$Retained_earnings[prev_period] + 
      df_is$Net_income[i]
    
    # Calculate total liabilities and equity before cash
    df_bs$Total_Liabilities_Equity[i] <- sum(df_bs[i, c("Reserve_for_invested_equity", 
                                                       "Retained_earnings", "Other_equity",
                                                       "Short_term_loans", "Long_term_loans",
                                                       "Lease_liabilities_current", 
                                                       "Lease_liabilities_non_current",
                                                       "Trade_payables", "Provisions",
                                                       "Other_current_liabilities",
                                                       "Other_non_current_liabilities",
                                                       "Non_current_provisions")])
    
    # 18. Calculate balancing cash
    df_bs$Cash_and_equivalents[i] <- df_bs$Total_Liabilities_Equity[i] - 
      df_bs$Total_Operating_Assets[i]
    
    # Update total assets
    df_bs$Total_Assets[i] <- df_bs$Total_Operating_Assets[i] + 
      df_bs$Cash_and_equivalents[i]
  }
  
  return(list(income_statement = df_is, balance_sheet = df_bs))
}
