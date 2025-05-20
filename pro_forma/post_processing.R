format_financial_statements <- function(df_is, df_bs) {
  formatted_is <- df_is
  formatted_bs <- df_bs
  
  formatted_is <- formatted_is[, c("Period",
                    revenue_regions,
                    "Total_revenue",
                    "Materials_and_services",
                    "Employee_benefits",
                    "Other_operating_expenses",
                    "EBITDA",
                    "Depreciation_and_amortization",
                    "EBIT",
                    "Net_interest",
                    "EBT",
                    "Corporate_income_tax",
                    "Net_income")]
  
  asset_columns <- c(bs_operating_asset_categories, "Cash_and_equivalents")
  liability_columns <- c("Short_term_loans", "Long_term_loans",
                       "Lease_liabilities_current", "Lease_liabilities_non_current",
                       "Trade_payables", "Provisions",
                       "Other_current_liabilities", "Other_non_current_liabilities",
                       "Non_current_provisions")
  equity_columns <- c("Reserve_for_invested_equity", "Retained_earnings", "Other_equity")
  total_columns <- c("Total_Assets", "Total_Liabilities_Equity")
  
  formatted_bs <- formatted_bs[, c("Period", 
                                asset_columns, 
                                liability_columns, 
                                equity_columns, 
                                total_columns)]
  
  return(list(income_statement = formatted_is, balance_sheet = formatted_bs))
}
