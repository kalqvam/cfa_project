create_proforma_statements <- function(historical_data, historical_bs_data, 
                                       revenue_growth_rates, expense_ratios, 
                                       fixed_assets_params, working_capital_ratios,
                                       balance_sheet_ratios, interest_rates, tax_rates) {
  
  # Helper function to determine period
  get_period <- function(year) {
    if (year %in% c("2024_Q4", "2025")) return("period_1")
    if (year %in% as.character(2026:2028)) return("period_2")
    if (year %in% as.character(2029:2030)) return("period_3")
    if (year == "terminal") return("period_4")
    stop(paste("Invalid year:", year))
  }
  
  # Create periods vector
  historical_periods <- c("2021", "2022", "2023", paste0("2024_Q", 1:4))
  projection_periods <- as.character(2025:2030)
  all_periods <- c(historical_periods, projection_periods)
  
  # Initialize dataframes
  df_is <- data.frame(Period = all_periods)
  df_bs <- data.frame(Period = all_periods)
  
  # Define regions and categories
  revenue_regions <- c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World")
  
  # Add historical data
  for (region in revenue_regions) {
    df_is[[region]] <- c(
      unlist(historical_data[[region]]),
      NA,  # Q4 2024
      rep(NA, length(projection_periods))
    )
  }
  
  # Add historical expense data with proper signs (keeping them positive)
  expense_categories <- c("Materials_and_services", "Employee_benefits", 
                          "Other_operating_expenses", "Depreciation_and_amortization", 
                          "Net_interest")
  
  for (category in expense_categories) {
    df_is[[category]] <- c(
      unlist(historical_data[[category]]),
      NA,  # Q4 2024
      rep(NA, length(projection_periods))
    )
  }
  
  # Add historical balance sheet data
  bs_categories <- names(historical_bs_data)
  for (category in bs_categories) {
    df_bs[[category]] <- c(
      unlist(historical_bs_data[[category]]),
      NA,  # Q4 2024
      rep(NA, length(projection_periods))
    )
  }
  
  # Initialize supplementary columns for balance sheet calculations
  df_bs$Total_Assets <- NA
  df_bs$Total_Operating_Assets <- NA
  df_bs$Total_Liabilities_Equity <- NA
  df_bs$Total_Interest_Bearing_Debt <- NA
  
  # Calculate historical total revenue
  df_is$Total_revenue <- rowSums(df_is[, revenue_regions])
  
  # Calculate historical metrics
  historical_end <- which(df_is$Period == "2024_Q3")
  for(i in 1:historical_end) {
    # Calculate total assets and operating assets for historical periods
    df_bs$Total_Operating_Assets[i] <- sum(df_bs[i, c("Intangible_assets", "PP_and_A_owned", 
                                                      "PP_and_A_ROU", "Inventories", 
                                                      "Trade_receivables", "Other_current_assets",
                                                      "Other_non_current_assets")])
    
    df_bs$Total_Assets[i] <- df_bs$Total_Operating_Assets[i] + df_bs$Cash_and_equivalents[i]
    
    df_bs$Total_Liabilities_Equity[i] <- sum(df_bs[i, c("Reserve_for_invested_equity", 
                                                        "Retained_earnings", "Other_equity",
                                                        "Short_term_loans", "Long_term_loans",
                                                        "Lease_liabilities_current", 
                                                        "Lease_liabilities_non_current",
                                                        "Trade_payables", "Provisions",
                                                        "Other_current_liabilities",
                                                        "Other_non_current_liabilities",
                                                        "Non_current_provisions")])
    
    df_bs$Total_Interest_Bearing_Debt[i] <- sum(df_bs[i, c("Short_term_loans", "Long_term_loans",
                                                           "Lease_liabilities_current", 
                                                           "Lease_liabilities_non_current")])
    
    # Historical income statement calculations
    df_is$EBITDA[i] <- df_is$Total_revenue[i] - 
      df_is$Materials_and_services[i] - 
      df_is$Employee_benefits[i] - 
      df_is$Other_operating_expenses[i]
    
    df_is$EBIT[i] <- df_is$EBITDA[i] - df_is$Depreciation_and_amortization[i]
    df_is$EBT[i] <- df_is$EBIT[i] + df_is$Net_interest[i]
    df_is$Corporate_income_tax[i] <- -df_is$EBT[i] * tax_rates$period_1
    df_is$Net_income[i] <- df_is$EBT[i] + df_is$Corporate_income_tax[i]
  }
  
  # Start projections from Q4 2024
  projection_start <- which(df_is$Period == "2024_Q4")
  
  # Helper function to calculate full year 2024 revenue
  calculate_2024_revenue <- function() {
    q1_idx <- which(df_is$Period == "2024_Q1")
    q4_idx <- which(df_is$Period == "2024_Q4")
    total_2024_revenue <- sum(df_is$Total_revenue[q1_idx:q4_idx])
    return(total_2024_revenue)
  }
  
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
    
    # 2-3. Calculate operating expenses and EBITDA (expenses as positive numbers)
    df_is$Materials_and_services[i] <- df_is$Total_revenue[i] * 
      expense_ratios$materials_and_services[[period_name]]
    df_is$Employee_benefits[i] <- df_is$Total_revenue[i] * 
      expense_ratios$employee_benefits[[period_name]]
    df_is$Other_operating_expenses[i] <- df_is$Total_revenue[i] * 
      expense_ratios$other_operating_expenses[[period_name]]
    
    # EBITDA calculation (subtracting expenses)
    df_is$EBITDA[i] <- df_is$Total_revenue[i] - 
      df_is$Materials_and_services[i] - 
      df_is$Employee_benefits[i] - 
      df_is$Other_operating_expenses[i]
    
    # 4. Project balance sheet items based on revenue_base instead of current_revenue
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
    
    # Rest of the function remains the same...
    # [Previous code for fixed assets, D&A, lease liabilities, etc.]
    
    # 5. Project fixed assets and related items
    if (current_period == "2024_Q4") {
      # For Q4 2024, use quarterly rates (divide annual rates by 4)
      df_bs$Intangible_assets[i] <- df_bs$Intangible_assets[prev_period] * 
        (1 + fixed_assets_params$growth_rates$intangible_assets[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$intangible_assets[[period_name]] / 4)
      
      df_bs$PP_and_A_owned[i] <- df_bs$PP_and_A_owned[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_owned[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]] / 4)
      
      df_bs$PP_and_A_ROU[i] <- df_bs$PP_and_A_ROU[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]] / 4) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4)
      
      # Update D&A calculation for Q4 2024 with quarterly rates
      df_is$Depreciation_and_amortization[i] <- (
        df_bs$Intangible_assets[prev_period] * 
          fixed_assets_params$depreciation_rates$intangible_assets[[period_name]] / 4 +
          df_bs$PP_and_A_owned[prev_period] * 
          fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]] / 4 +
          df_bs$PP_and_A_ROU[prev_period] * 
          fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4
      )
      
      # Calculate lease liabilities with quarterly rates for Q4 2024
      rou_growth <- fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]] / 4
      rou_depr <- fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]] / 4
      lease_growth <- (1 + rou_growth) * (1 - rou_depr)
      
      df_bs$Lease_liabilities_current[i] <- df_bs$Lease_liabilities_current[prev_period] * 
        lease_growth
      df_bs$Lease_liabilities_non_current[i] <- df_bs$Lease_liabilities_non_current[prev_period] * 
        lease_growth
      
    } else {
      # For all other periods, use the original annual rates calculation
      df_bs$Intangible_assets[i] <- df_bs$Intangible_assets[prev_period] * 
        (1 + fixed_assets_params$growth_rates$intangible_assets[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$intangible_assets[[period_name]])
      
      df_bs$PP_and_A_owned[i] <- df_bs$PP_and_A_owned[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_owned[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_owned[[period_name]])
      
      df_bs$PP_and_A_ROU[i] <- df_bs$PP_and_A_ROU[prev_period] * 
        (1 + fixed_assets_params$growth_rates$pp_and_a_rou[[period_name]]) * 
        (1 - fixed_assets_params$depreciation_rates$pp_and_a_rou[[period_name]])
      
      # Regular D&A calculation for other periods
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
      
      # Regular lease liabilities calculation for other periods
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
    
    # Calculate total operating assets before remaining BS items
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
  
  # Ensure proper column order for income statement
  df_is <- df_is[, c("Period",
                     # Revenue sections
                     revenue_regions,
                     "Total_revenue",
                     # Operating expenses
                     "Materials_and_services",
                     "Employee_benefits",
                     "Other_operating_expenses",
                     # Profitability metrics
                     "EBITDA",
                     # Non-operating items
                     "Depreciation_and_amortization",
                     "EBIT",
                     "Net_interest",
                     "EBT",
                     "Corporate_income_tax",
                     "Net_income")]
  
  # Ensure proper column order for balance sheet
  df_bs <- df_bs[, c("Period",
                     # Assets
                     "Intangible_assets",
                     "PP_and_A_owned",
                     "PP_and_A_ROU",
                     "Inventories",
                     "Trade_receivables",
                     "Other_current_assets",
                     "Cash_and_equivalents",
                     "Other_non_current_assets",
                     # Liabilities
                     "Short_term_loans",
                     "Long_term_loans",
                     "Lease_liabilities_current",
                     "Lease_liabilities_non_current",
                     "Trade_payables",
                     "Provisions",
                     "Other_current_liabilities",
                     "Other_non_current_liabilities",
                     "Non_current_provisions",
                     # Equity
                     "Reserve_for_invested_equity",
                     "Retained_earnings",
                     "Other_equity",
                     "Total_Assets",
                     "Total_Liabilities_Equity")]
  
  return(list(income_statement = df_is, balance_sheet = df_bs))
}

proforma_statements <- create_proforma_statements(
  historical_data = historical_data,
  historical_bs_data = historical_bs_data,
  revenue_growth_rates = revenue_growth_rates,
  expense_ratios = expense_ratios,
  fixed_assets_params = fixed_assets_params,
  working_capital_ratios = working_capital_ratios,
  balance_sheet_ratios = balance_sheet_ratios,
  interest_rates = interest_rates,
  tax_rates = tax_rates
)

# Access the results
income_statement <- proforma_statements$income_statement
balance_sheet <- proforma_statements$balance_sheet
