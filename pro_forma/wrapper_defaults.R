run_complete_financial_model <- function() {
  populated_historical <- prepare_historical_dataframes(df_is, df_bs, historical_data, historical_bs_data, tax_rates)
  
  df_is <- populated_historical$df_is
  df_bs <- populated_historical$df_bs
  
  populated_projections <- calculate_proforma_projections(df_is, df_bs, revenue_growth_rates, expense_ratios,
                              fixed_assets_params, working_capital_ratios,
                              balance_sheet_ratios, interest_rates, tax_rates)

  df_is <- populated_projections$df_is
  df_bs <- populated_projections$df_bs
  
  formatted_statements <- format_financial_statements()

  return(list(income_statement = formatted_statements$income_statement, 
         balance_sheet = formatted_statements$balance_sheet))
}
