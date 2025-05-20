historical_data <- list(
  Nordics = list(
    "2021" = 23.1, "2022" = 57.8, "2023" = 114.1,
    "2024_Q1" = 15.9, "2024_Q2" = 29.2, "2024_Q3" = 26.7
  ),
  Rest_of_Europe = list(
    "2021" = 4.1, "2022" = 40.7, "2023" = 144.2,
    "2024_Q1" = 20.3, "2024_Q2" = 19.8, "2024_Q3" = 21.1
  ),
  North_America = list(
    "2021" = 0, "2022" = 1.6, "2023" = 11.3,
    "2024_Q1" = 4.1, "2024_Q2" = 5.9, "2024_Q3" = 3.5
  ),
  Rest_of_World = list(
    "2021" = 0.2, "2022" = 3.5, "2023" = 14,
    "2024_Q1" = 2.8, "2024_Q2" = 3.0, "2024_Q3" = 2.0
  ),
  
  Materials_and_services = list(
    "2021" = 13.6, "2022" = 52.4, "2023" = 129.4,
    "2024_Q1" = 19.6, "2024_Q2" = 29.6, "2024_Q3" = 23.6
  ),
  Employee_benefits = list(
    "2021" = 7.5, "2022" = 21.9, "2023" = 48.9,
    "2024_Q1" = 18.2, "2024_Q2" = 18.6, "2024_Q3" = 18.2
  ),
  Other_operating_expenses = list(
    "2021" = 6.2, "2022" = 20.2, "2023" = 59.6,
    "2024_Q1" = 13.9, "2024_Q2" = 16.6, "2024_Q3" = 17.8
  ),
  Depreciation_and_amortization = list(
    "2021" = 1.1, "2022" = 3.4, "2023" = 6.5,
    "2024_Q1" = 2.3, "2024_Q2" = 2.5, "2024_Q3" = 2.8
  ),
  Net_interest = list(
    "2021" = -0.2, "2022" = -1.3, "2023" = 2.5,
    "2024_Q1" = 0.3, "2024_Q2" = -0.1, "2024_Q3" = -3.5
  )
)

projection_periods <- list(
  period_1 = c("2024_Q4", "2025"),
  period_2 = as.character(2026:2028),
  period_3 = as.character(2029:2030),
  period_4 = "terminal"
)

revenue_growth_rates <- list(
  Nordics = list(
    period_1 = 0.069,
    period_2 = 0.0995,
    period_3 = 0.0059,
    period_4 = 0.025
  ),
  Rest_of_Europe = list(
    period_1 = 0.1792,
    period_2 = 0.2033,
    period_3 = 0.1988,
    period_4 = 0.025
  ),
  North_America = list(
    period_1 = 0.6711,
    period_2 = 0.5708,
    period_3 = 0.2715,
    period_4 = 0.025
  ),
  Rest_of_World = list(
    period_1 = 0.4723,
    period_2 = 0.3246,
    period_3 = 0.1607,
    period_4 = 0.025
  )
)

print_growth_rates <- function(rates) {
  for (region in names(rates)) {
    cat("\n", region, ":\n")
    for (period in names(rates[[region]])) {
      cat(sprintf("%s: %.3f\n", period, rates[[region]][[period]]))
    }
  }
}

expense_ratios <- list(
  materials_and_services = list(
    period_1 = 0.499,
    period_2 = 0.495,
    period_3 = 0.491,
    period_4 = 0.488
  ),
  employee_benefits = list(
    period_1 = 0.242,
    period_2 = 0.202,
    period_3 = 0.193,
    period_4 = 0.184
  ),
  other_operating_expenses = list(
    period_1 = 0.212,
    period_2 = 0.199,
    period_3 = 0.1865,
    period_4 = 0.1745
  )
)

interest_rates <- list(
  debt_interest = list(
    period_1 = 0.05,
    period_2 = 0.05,
    period_3 = 0.05,
    period_4 = 0.05
  ),
  savings_interest = list(
    period_1 = 0.025, 
    period_2 = 0.025,
    period_3 = 0.025,
    period_4 = 0.025
  )
)

tax_rates <- list(
  period_1 = 0.20,
  period_2 = 0.20,
  period_3 = 0.20,
  period_4 = 0.20
)

get_rate <- function(rates_list, period, metric = NULL) {
  if (is.null(metric)) {
    return(rates_list[[period]])
  } else {
    return(rates_list[[metric]][[period]])
  }
}

get_period <- function(year) {
  if (year %in% c("2024_Q4", "2025")) return("period_1")
  if (year %in% as.character(2026:2028)) return("period_2")
  if (year %in% as.character(2029:2030)) return("period_3")
  if (year == "terminal") return("period_4")
  stop(paste("Invalid year:", year))
}
