format_financial_statements <- function(df_is, df_bs) {
  revenue_regions <- c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World")
  
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
