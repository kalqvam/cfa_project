projection_periods <- list(
  period_1 = c("2024_Q4", "2025"),
  period_2 = as.character(2026:2028),
  period_3 = as.character(2029:2030),
  period_4 = "terminal"
)

revenue_growth_rates <- list(
  Nordics = list(
    period_1 = 0.0690,
    period_2 = 0.0995,
    period_3 = 0.0059,
    period_4 = 0.0250
  ),
  Rest_of_Europe = list(
    period_1 = 0.1792,
    period_2 = 0.2033,
    period_3 = 0.1988,
    period_4 = 0.0250
  ),
  North_America = list(
    period_1 = 0.6711,
    period_2 = 0.5708,
    period_3 = 0.2715,
    period_4 = 0.0250
  ),
  Rest_of_World = list(
    period_1 = 0.4723,
    period_2 = 0.3246,
    period_3 = 0.1607,
    period_4 = 0.0250
  )
)

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
