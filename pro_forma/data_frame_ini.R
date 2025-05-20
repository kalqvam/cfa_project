historical_periods <- c("2021", "2022", "2023", paste0("2024_Q", 1:4))
projection_periods <- as.character(2025:2030)
all_periods <- c(historical_periods, projection_periods)

historical_end <- "2024_Q3"
projection_start <- "2024_Q4"

revenue_regions <- c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World")

expense_categories <- c("Materials_and_services", "Employee_benefits", 
                      "Other_operating_expenses", "Depreciation_and_amortization", 
                      "Net_interest")

bs_operating_asset_categories <- c("Intangible_assets", "PPE_owned", 
                                "PPE_ROU", "Inventories", 
                                "Trade_receivables", "Other_current_assets",
                                "Other_non_current_assets")

bs_liability_equity_categories <- c("Reserve_for_invested_equity", 
                                  "Retained_earnings", "Other_equity",
                                  "Short_term_loans", "Long_term_loans",
                                  "Lease_liabilities_current", 
                                  "Lease_liabilities_non_current",
                                  "Trade_payables", "Provisions",
                                  "Other_current_liabilities",
                                  "Other_non_current_liabilities",
                                  "Non_current_provisions")

bs_interest_bearing_debt_categories <- c("Short_term_loans", "Long_term_loans",
                                      "Lease_liabilities_current", 
                                      "Lease_liabilities_non_current")

bs_categories <- names(historical_bs_data)

df_is <- data.frame(Period = all_periods)

for (region in revenue_regions) {
  df_is[[region]] <- NA
}

for (category in expense_categories) {
  df_is[[category]] <- NA
}

df_is$Total_revenue <- NA
df_is$EBITDA <- NA
df_is$EBIT <- NA
df_is$EBT <- NA
df_is$Corporate_income_tax <- NA
df_is$Net_income <- NA

df_bs <- data.frame(Period = all_periods)

for (category in bs_categories) {
  df_bs[[category]] <- NA
}

df_bs$Total_Assets <- NA
df_bs$Total_Operating_Assets <- NA
df_bs$Total_Liabilities_Equity <- NA
df_bs$Total_Interest_Bearing_Debt <- NA
