prepare_historical_dataframes <- function(historical_data, historical_bs_data, tax_rates) {
  historical_end_idx <- which(df_is$Period == historical_end)
  
  for (region in revenue_regions) {
    df_is[[region]][1:historical_end_idx] <- unlist(historical_data[[region]])
  }
  
  for (category in expense_categories) {
    df_is[[category]][1:historical_end_idx] <- unlist(historical_data[[category]])
  }
  
  for (category in names(historical_bs_data)) {
    df_bs[[category]][1:historical_end_idx] <- unlist(historical_bs_data[[category]])
  }
  
  for(i in 1:historical_end_idx) {
    df_is$Total_revenue[i] <- rowSums(df_is[i, revenue_regions])
    
    df_bs$Total_Operating_Assets[i] <- sum(df_bs[i, bs_operating_asset_categories])
    
    df_bs$Total_Assets[i] <- df_bs$Total_Operating_Assets[i] + df_bs$Cash_and_equivalents[i]
    
    df_bs$Total_Liabilities_Equity[i] <- sum(df_bs[i, bs_liability_equity_categories])
    
    df_bs$Total_Interest_Bearing_Debt[i] <- sum(df_bs[i, bs_interest_bearing_debt_categories])
    
    df_is$EBITDA[i] <- df_is$Total_revenue[i] - 
      df_is$Materials_and_services[i] - 
      df_is$Employee_benefits[i] - 
      df_is$Other_operating_expenses[i]
    
    df_is$EBIT[i] <- df_is$EBITDA[i] - df_is$Depreciation_and_amortization[i]
    df_is$EBT[i] <- df_is$EBIT[i] + df_is$Net_interest[i]
    df_is$Corporate_income_tax[i] <- -df_is$EBT[i] * tax_rates$period_1
    df_is$Net_income[i] <- df_is$EBT[i] + df_is$Corporate_income_tax[i]
  }
  
  return(TRUE)
}

run_historical_preparation_defaults <- function() {
  return(prepare_historical_dataframes(historical_data, historical_bs_data, tax_rates))
}
