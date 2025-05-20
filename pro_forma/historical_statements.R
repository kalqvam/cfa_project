prepare_historical_dataframes <- function(historical_data, historical_bs_data, tax_rates) {
  
  historical_periods <- c("2021", "2022", "2023", paste0("2024_Q", 1:4))
  projection_periods <- as.character(2025:2030)
  all_periods <- c(historical_periods, projection_periods)
  
  df_is <- data.frame(Period = all_periods)
  
  revenue_regions <- c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World")
  for (region in revenue_regions) {
    df_is[[region]] <- c(
      unlist(historical_data[[region]]),
      NA, 
      rep(NA, length(projection_periods))
    )
  }
  
  expense_categories <- c("Materials_and_services", "Employee_benefits", 
                         "Other_operating_expenses", "Depreciation_and_amortization", 
                         "Net_interest")
  for (category in expense_categories) {
    df_is[[category]] <- c(
      unlist(historical_data[[category]]),
      NA,
      rep(NA, length(projection_periods))
    )
  }
  
  df_is$Total_revenue <- rowSums(df_is[, revenue_regions])
  
  df_bs <- data.frame(Period = all_periods)
  
  bs_categories <- names(historical_bs_data)
  for (category in bs_categories) {
    df_bs[[category]] <- c(
      unlist(historical_bs_data[[category]]),
      NA, 
      rep(NA, length(projection_periods))
    )
  }
  
  df_bs$Total_Assets <- NA
  df_bs$Total_Operating_Assets <- NA
  df_bs$Total_Liabilities_Equity <- NA
  df_bs$Total_Interest_Bearing_Debt <- NA
  
  historical_end <- which(df_is$Period == "2024_Q3")
  for(i in 1:historical_end) {
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
    
    df_is$EBITDA[i] <- df_is$Total_revenue[i] - 
      df_is$Materials_and_services[i] - 
      df_is$Employee_benefits[i] - 
      df_is$Other_operating_expenses[i]
    
    df_is$EBIT[i] <- df_is$EBITDA[i] - df_is$Depreciation_and_amortization[i]
    df_is$EBT[i] <- df_is$EBIT[i] + df_is$Net_interest[i]
    df_is$Corporate_income_tax[i] <- -df_is$EBT[i] * tax_rates$period_1
    df_is$Net_income[i] <- df_is$EBT[i] + df_is$Corporate_income_tax[i]
  }
  
  return(list(income_statement = df_is, balance_sheet = df_bs))
}

run_historical_preparation_defaults <- function() {
  return(prepare_historical_dataframes(historical_data, historical_bs_data, tax_rates))
}
