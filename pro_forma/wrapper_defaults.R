run_complete_financial_model <- function() {
  # Step 1: Populate historical data
  prepare_historical_dataframes(historical_data, historical_bs_data, tax_rates)
  
  # Step 2: Calculate projections
  calculate_proforma_projections(revenue_growth_rates, expense_ratios,
                              fixed_assets_params, working_capital_ratios,
                              balance_sheet_ratios, interest_rates, tax_rates)
  
  # Step 3: Format the financial statements
  formatted_statements <- format_financial_statements()
  
  # Create helpful summary information
  summary_info <- list(
    periods = list(
      historical = historical_periods,
      projection = projection_periods
    ),
    categories = list(
      revenue = revenue_regions,
      expenses = expense_categories,
      assets = bs_operating_asset_categories,
      liabilities_equity = bs_liability_equity_categories
    )
  )
  
  # Return everything in a structured format
  return(list(
    income_statement = formatted_statements$income_statement,
    balance_sheet = formatted_statements$balance_sheet,
    raw_data = list(
      income_statement = df_is,
      balance_sheet = df_bs
    ),
    model_structure = summary_info
  ))
}
