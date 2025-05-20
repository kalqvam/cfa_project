# Define periods globally
historical_periods <- c("2021", "2022", "2023", paste0("2024_Q", 1:4))
projection_periods <- as.character(2025:2030)
all_periods <- c(historical_periods, projection_periods)

# Key period markers
historical_end <- "2024_Q3"
projection_start <- "2024_Q4"

# Business segments
revenue_regions <- c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World")

# Financial categories
expense_categories <- c("Materials_and_services", "Employee_benefits", 
                      "Other_operating_expenses", "Depreciation_and_amortization", 
                      "Net_interest")

# Balance sheet categories
bs_operating_asset_categories <- c("Intangible_assets", "PP_and_A_owned", 
                                "PP_and_A_ROU", "Inventories", 
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
