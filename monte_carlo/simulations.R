#install.packages(c("Matrix", "MASS"))
library(Matrix)
library(MASS)
library(dplyr)

generate_simulation_inputs <- function(
    base_inputs,
    projection_periods = 2025:2030,
    n_scenarios = 100,
    apply_shocks = TRUE
) {
  
  get_growth_rate <- function(rates, region, period) {
    # Handle both list and vector structures
    if (is.list(rates[[region]])) {
      return(rates[[region]][[period]])
    } else {
      return(rates[[region]][period])
    }
  }
  
  # Extract base rates for reference
  base_growth_rates <- list(
    Nordics = list(
      period_1 = get_growth_rate(base_inputs$revenue_growth_rates, "Nordics", "period_1"),
      period_2 = get_growth_rate(base_inputs$revenue_growth_rates, "Nordics", "period_2"),
      period_3 = get_growth_rate(base_inputs$revenue_growth_rates, "Nordics", "period_3"),
      period_4 = get_growth_rate(base_inputs$revenue_growth_rates, "Nordics", "period_4")
    ),
    Rest_of_Europe = list(
      period_1 = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_Europe", "period_1"),
      period_2 = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_Europe", "period_2"),
      period_3 = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_Europe", "period_3"),
      period_4 = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_Europe", "period_4")
    ),
    North_America = list(
      period_1 = get_growth_rate(base_inputs$revenue_growth_rates, "North_America", "period_1"),
      period_2 = get_growth_rate(base_inputs$revenue_growth_rates, "North_America", "period_2"),
      period_3 = get_growth_rate(base_inputs$revenue_growth_rates, "North_America", "period_3"),
      period_4 = get_growth_rate(base_inputs$revenue_growth_rates, "North_America", "period_4")
    ),
    Rest_of_World = list(
      period_1 = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_World", "period_1"),
      period_2 = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_World", "period_2"),
      period_3 = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_World", "period_3"),
      period_4 = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_World", "period_4")
    )
  )
  
  base_expense_ratios <- list(
    materials = c(
      period_1 = base_inputs$expense_ratios$materials_and_services$period_1,
      period_2 = base_inputs$expense_ratios$materials_and_services$period_2,
      period_3 = base_inputs$expense_ratios$materials_and_services$period_3,
      period_4 = base_inputs$expense_ratios$materials_and_services$period_4
    ),
    employee = c(
      period_1 = base_inputs$expense_ratios$employee_benefits$period_1,
      period_2 = base_inputs$expense_ratios$employee_benefits$period_2,
      period_3 = base_inputs$expense_ratios$employee_benefits$period_3,
      period_4 = base_inputs$expense_ratios$employee_benefits$period_4
    ),
    other = c(
      period_1 = base_inputs$expense_ratios$other_operating_expenses$period_1,
      period_2 = base_inputs$expense_ratios$other_operating_expenses$period_2,
      period_3 = base_inputs$expense_ratios$other_operating_expenses$period_3,
      period_4 = base_inputs$expense_ratios$other_operating_expenses$period_4
    )
  )
  
  simulated_scenarios <- vector("list", n_scenarios)
  
  # Beta distribution parameters for working capital ratios
  wc_beta_params <- list(
    inventory = list(
      mean = base_inputs$working_capital_ratios$inventory_to_revenue$period_1,
      sd = 0.025,  # Adjust these SDs based on desired volatility
      shape_param = 5
    ),
    receivables = list(
      mean = base_inputs$working_capital_ratios$trade_receivables_to_revenue$period_1,
      sd = 0.027,
      shape_param = 5
    ),
    payables = list(
      mean = base_inputs$working_capital_ratios$trade_payables_to_revenue$period_1,
      sd = 0.013,
      shape_param = 5
    ),
    other_ratio = list(
      mean = base_inputs$working_capital_ratios$other_current_liab_to_revenue$period_1,
      sd = 0.0225,
      shape_param = 5
    )
  )
  
  corr_matrix <- create_correlation_matrix()
  nwc_corr_matrix <- create_nwc_correlation_matrix()
  n_years <- length(projection_periods)
  n_vars <- nrow(corr_matrix)
  
  # Storage lists for all scenarios
  simulated_scenarios <- vector("list", n_scenarios)
  
  shock_impact_tracking <- if(apply_shocks) {
    list(
      eu_policy_shock = list(
        count = 0, 
        avg_shock_severity = c(),
        before_growth = c(),
        after_growth = c()
      ),
      na_policy_shock = list(
        count = 0, 
        avg_shock_severity = c(),
        before_growth = c(),
        after_growth = c()
      ),
      tech_advancement_shock = list(
        count = 0, 
        avg_shock_severity = c(),
        before_growth = c(),
        after_growth = c()
      )
    )
  } else NULL
  
  for(scenario in 1:n_scenarios) {
    
    scenario_draws <- generate_correlated_normals(n_years, corr_matrix)
    scenario_draws_wc <- generate_correlated_normals(n_years, nwc_corr_matrix)
    
    # Use the updated functions from function_ini.R - no redefinition needed
    nordics_growth <- simulate_mean_reverting_growth_with_correlation(
      initial_value = get_growth_rate(base_inputs$revenue_growth_rates, "Nordics", "period_1"),
      target_value = get_growth_rate(base_inputs$revenue_growth_rates, "Nordics", "period_4"),
      correlated_draws = scenario_draws[1:3, 1]
    )
    
    roe_growth <- simulate_mean_reverting_growth_with_correlation(
      initial_value = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_Europe", "period_1"),
      target_value = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_Europe", "period_4"),
      correlated_draws = scenario_draws[1:3, 2]
    )
    
    na_growth <- simulate_mean_reverting_growth_with_correlation(
      initial_value = get_growth_rate(base_inputs$revenue_growth_rates, "North_America", "period_1"),
      target_value = get_growth_rate(base_inputs$revenue_growth_rates, "North_America", "period_4"),
      correlated_draws = scenario_draws[1:3, 3]
    )
    
    row_growth <- simulate_mean_reverting_growth_with_correlation(
      initial_value = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_World", "period_1"),
      target_value = get_growth_rate(base_inputs$revenue_growth_rates, "Rest_of_World", "period_4"),
      correlated_draws = scenario_draws[1:3, 4]
    )
    
    # Simulate all expense ratios and working capital ratios using updated functions
    materials_ratio <- simulate_converging_ratio_with_correlation(
      initial_mean = base_expense_ratios$materials["period_1"],
      target_mean = base_expense_ratios$materials["period_4"],
      initial_sd = 0.005,
      target_sd = 0.005,
      correlated_draws = scenario_draws[1:3, 5]
    )
    
    employee_ratio <- simulate_converging_ratio_with_correlation(
      initial_mean = base_expense_ratios$employee["period_1"] - 0.045,
      target_mean = base_expense_ratios$employee["period_4"],
      initial_sd = 0.015,
      target_sd = 0.005,
      correlated_draws = scenario_draws[1:3, 6]
    )
    
    other_ratio <- simulate_converging_ratio_with_correlation(
      initial_mean = base_expense_ratios$other["period_1"] - 0.025,
      target_mean = base_expense_ratios$other["period_4"],
      initial_sd = 0.015,
      target_sd = 0.005,
      correlated_draws = scenario_draws[1:3, 7]
    )
    
    # Simulate all working capital ratios
    inventory_ratio <- simulate_converging_ratio_with_correlation(
      initial_mean = wc_beta_params$inventory$mean,
      target_mean = wc_beta_params$inventory$mean,
      initial_sd = wc_beta_params$inventory$sd * 0.7,
      target_sd = wc_beta_params$inventory$sd * 0.1,
      correlated_draws = scenario_draws_wc[1:3, 1],
      shape_param = wc_beta_params$inventory$shape_param
    )
    
    receivables_ratio <- simulate_converging_ratio_with_correlation(
      initial_mean = wc_beta_params$receivables$mean,
      target_mean = wc_beta_params$receivables$mean,
      initial_sd = wc_beta_params$receivables$sd * 0.7,
      target_sd = wc_beta_params$receivables$sd * 0.1,
      correlated_draws = scenario_draws_wc[1:3, 2],
      shape_param = wc_beta_params$receivables$shape_param
    )
    
    payables_ratio <- simulate_converging_ratio_with_correlation(
      initial_mean = wc_beta_params$payables$mean,
      target_mean = wc_beta_params$payables$mean,
      initial_sd = wc_beta_params$payables$sd * 0.7,
      target_sd = wc_beta_params$payables$sd * 0.1,
      correlated_draws = scenario_draws_wc[1:3, 3],
      shape_param = wc_beta_params$payables$shape_param
    )
    
    other_current_ratio <- simulate_converging_ratio_with_correlation(
      initial_mean = wc_beta_params$other_ratio$mean,
      target_mean = wc_beta_params$other_ratio$mean,
      initial_sd = wc_beta_params$other_ratio$sd * 0.7,
      target_sd = wc_beta_params$other_ratio$sd * 0.1,
      correlated_draws = scenario_draws_wc[1:3, 4],
      shape_param = wc_beta_params$other_ratio$shape_param
    )
    
    # Generate new random draws for rates and asset growths
    rf_rate_draws <- rnorm(n_years)
    mrp_draws <- rnorm(n_years)
    ppe_draws <- rnorm(n_years)
    rou_draws <- rnorm(n_years)
    
    # Simulate risk-free rate
    rf_mean <- base_inputs$dcf_inputs$risk_free_rate
    rf_sd <- 0.003
    rf_rate <- rf_mean + rf_sd * rf_rate_draws
    
    # Simulate market risk premium
    mrp_mean <- base_inputs$dcf_inputs$market_risk_premium
    mrp_sd <- 0.01
    mrp <- mrp_mean + mrp_sd * mrp_draws
    
    # Ensure rates stay within reasonable bounds
    rf_rate <- pmax(0, rf_rate)
    mrp <- pmax(0.02, mrp)
    
    growth_rates <- list(
      Nordics = list(
        period_1 = nordics_growth["period_1"],
        period_2 = nordics_growth["period_2"],
        period_3 = nordics_growth["period_3"],
        period_4 = nordics_growth["period_4"]
      ),
      Rest_of_Europe = list(
        period_1 = roe_growth["period_1"],
        period_2 = roe_growth["period_2"],
        period_3 = roe_growth["period_3"],
        period_4 = roe_growth["period_4"]
      ),
      North_America = list(
        period_1 = na_growth["period_1"],
        period_2 = na_growth["period_2"],
        period_3 = na_growth["period_3"],
        period_4 = na_growth["period_4"]
      ),
      Rest_of_World = list(
        period_1 = row_growth["period_1"],
        period_2 = row_growth["period_2"],
        period_3 = row_growth["period_3"],
        period_4 = row_growth["period_4"]
      )
    )
    
    expense_ratios <- list(
      materials_and_services = list(
        period_1 = materials_ratio[1],
        period_2 = materials_ratio[2],
        period_3 = materials_ratio[3],
        period_4 = materials_ratio[4]
      ),
      employee_benefits = list(
        period_1 = employee_ratio[1],
        period_2 = employee_ratio[2],
        period_3 = employee_ratio[3],
        period_4 = employee_ratio[4]
      ),
      other_operating_expenses = list(
        period_1 = other_ratio[1],
        period_2 = other_ratio[2],
        period_3 = other_ratio[3],
        period_4 = other_ratio[4]
      )
    )
    
    new_working_capital_ratios <- base_inputs$working_capital_ratios
    
    # Update with simulated values
    new_working_capital_ratios$inventory_to_revenue <- list(
      period_1 = inventory_ratio[1],
      period_2 = inventory_ratio[2],
      period_3 = inventory_ratio[3],
      period_4 = inventory_ratio[6]  # Use last value for terminal period
    )
    
    new_working_capital_ratios$trade_receivables_to_revenue <- list(
      period_1 = receivables_ratio[1],
      period_2 = receivables_ratio[2],
      period_3 = receivables_ratio[3],
      period_4 = receivables_ratio[6]
    )
    
    new_working_capital_ratios$trade_payables_to_revenue <- list(
      period_1 = payables_ratio[1],
      period_2 = payables_ratio[2],
      period_3 = payables_ratio[3],
      period_4 = payables_ratio[6]
    )
    
    new_working_capital_ratios$other_current_liab_to_revenue <- list(
      period_1 = other_current_ratio[1],
      period_2 = other_current_ratio[2],
      period_3 = other_current_ratio[3],
      period_4 = other_current_ratio[6]
    )
    
    # 3. Apply shocks if enabled
    if(apply_shocks) {
      for(shock_name in names(shock_scenarios)) {
        shock_info <- shock_scenarios[[shock_name]]
        
        if(shock_info$shock_distribution() > 0) {
          shock_impact_tracking[[shock_name]]$count <- 
            shock_impact_tracking[[shock_name]]$count + 1
          
          shock_k <- shock_info$shock_severity()
          shock_impact_tracking[[shock_name]]$avg_shock_severity <- 
            c(shock_impact_tracking[[shock_name]]$avg_shock_severity, shock_k)
          
          for(region in shock_info$regions) {
            for(period_label in c("period_1", "period_2", "period_3", "period_4")) {
              before_growth <- growth_rates[[region]][[period_label]]
              
              shock_impact_tracking[[shock_name]]$before_growth <- 
                c(shock_impact_tracking[[shock_name]]$before_growth, as.numeric(before_growth))
              
              growth_rates[[region]][[period_label]] <- before_growth * shock_k
              
              after_growth <- growth_rates[[region]][[period_label]]
              shock_impact_tracking[[shock_name]]$after_growth <- 
                c(shock_impact_tracking[[shock_name]]$after_growth, as.numeric(after_growth))
            }
          }
        }
      }
    }
    
    intangibles_mean <- base_inputs$fixed_assets_params$growth_rates$intangible_assets$period_1
    intangibles_sd <- 0.04  # Using same volatility as before for consistency
    intangibles_growth <- intangibles_mean + intangibles_sd * scenario_draws[, 8]
    
    # PP&E (owned)
    ppe_mean <- base_inputs$fixed_assets_params$growth_rates$ppe_owned$period_1
    ppe_sd <- 0.08  # Higher volatility for PP&E
    ppe_growth <- ppe_mean + ppe_sd * ppe_draws
    
    # ROU assets
    rou_mean <- base_inputs$fixed_assets_params$growth_rates$ppe_rou$period_1
    rou_sd <- 0.06  # Moderate volatility for ROU assets
    rou_growth <- rou_mean + rou_sd * rou_draws
    
    # Ensure growth rates stay within reasonable bounds
    intangibles_growth <- pmax(0, intangibles_growth)  # Prevent negative growth
    ppe_growth <- pmax(0, ppe_growth)
    rou_growth <- pmax(0, rou_growth)
    
    # Create a deep copy of fixed_assets_params
    new_fixed_assets_params <- base_inputs$fixed_assets_params
    
    # Update only the intangible assets growth rates
    new_fixed_assets_params$growth_rates$intangible_assets <- list(
      period_1 = intangibles_growth[1],
      period_2 = intangibles_growth[2],
      period_3 = intangibles_growth[3],
      period_4 = intangibles_growth[6]  # Use last value for terminal period
    )
    
    new_fixed_assets_params$growth_rates$ppe_owned <- list(
      period_1 = ppe_growth[1],
      period_2 = ppe_growth[2],
      period_3 = ppe_growth[3],
      period_4 = ppe_growth[6]  # Use last value for terminal period
    )
    
    new_fixed_assets_params$growth_rates$ppe_rou <- list(
      period_1 = rou_growth[1],
      period_2 = rou_growth[2],
      period_3 = rou_growth[3],
      period_4 = rou_growth[6]  # Use last value for terminal period
    )
    
    # Create a new dcf_inputs structure with simulated rates
    new_dcf_inputs <- base_inputs$dcf_inputs
    new_dcf_inputs$risk_free_rate <- rf_rate[1]  # Use first period rate
    new_dcf_inputs$market_risk_premium <- mrp[1]  # Use first period premium
    
    # Store the simulated values for this scenario
    simulated_scenarios[[scenario]] <- list(
      # Growth rates by region
      revenue_growth_rates = growth_rates,
      expense_ratios = expense_ratios,
      working_capital_ratios = new_working_capital_ratios,
      
      # Asset growth rates
      fixed_assets_params = new_fixed_assets_params,
      
      # Add the new DCF inputs
      dcf_inputs = new_dcf_inputs
    )
  }
  
  if(apply_shocks) {
    for(shock_name in names(shock_scenarios)) {
      cat("\nShock Scenario:", shock_name, "\n")
      cat("Number of Occurrences:", shock_impact_tracking[[shock_name]]$count, "\n")
      cat("Average Shock Severity:", 
          mean(shock_impact_tracking[[shock_name]]$avg_shock_severity), "\n")
      cat("Average Growth Before Shock:", 
          mean(shock_impact_tracking[[shock_name]]$before_growth), "\n")
      cat("Average Growth After Shock:", 
          mean(shock_impact_tracking[[shock_name]]$after_growth), "\n")
    }
  }
  
  return(simulated_scenarios)
}

run_monte_carlo_dcf <- function(
    base_inputs,
    n_scenarios = 100,
    historical_data,
    historical_bs_data
) {
  # Generate all simulation scenarios
  simulation_inputs <- generate_simulation_inputs(
    base_inputs = base_inputs,
    n_scenarios = n_scenarios
  )
  
  # Storage for results
  valuation_results <- vector("list", n_scenarios)
  
  # Run DCF for each scenario
  for(i in 1:n_scenarios) {
    # Create clean copies of the global dataframes for this scenario
    scenario_df_is <- df_is
    scenario_df_bs <- df_bs
    
    # Apply simulation inputs to this scenario
    current_inputs <- simulation_inputs[[i]]
    
    # Create proforma statements with simulated inputs
    proforma_statements <- run_complete_financial_model_with_params(
      df_is = scenario_df_is,
      df_bs = scenario_df_bs,
      revenue_growth_rates = current_inputs$revenue_growth_rates,
      expense_ratios = current_inputs$expense_ratios,
      fixed_assets_params = current_inputs$fixed_assets_params,
      working_capital_ratios = current_inputs$working_capital_ratios,
      balance_sheet_ratios = balance_sheet_ratios,  # Keep global
      interest_rates = interest_rates,  # Keep global
      tax_rates = tax_rates  # Keep global
    )
    
    # Calculate WACC with simulated inputs
    scenario_wacc_results <- calculate_wacc(current_inputs$dcf_inputs)
    
    # Run DCF calculation with simulated inputs
    dcf_result <- calculate_dcf(
      income_statement = proforma_statements$income_statement,
      balance_sheet = proforma_statements$balance_sheet,
      wacc_results = scenario_wacc_results,
      fixed_assets_params = current_inputs$fixed_assets_params
    )
    
    # Store results
    valuation_results[[i]] <- list(
      scenario = i,
      price_per_share = dcf_result$price_per_share,
      enterprise_value = dcf_result$enterprise_value,
      equity_value = dcf_result$equity_value,
      wacc = dcf_result$wacc,
      risk_free_rate = current_inputs$dcf_inputs$risk_free_rate,
      market_risk_premium = current_inputs$dcf_inputs$market_risk_premium
    )
  }
  
  # Convert results to data frame
  results_df <- bind_rows(valuation_results)
  
  # Calculate summary statistics
  summary_stats <- list(
    mean_price = mean(results_df$price_per_share),
    median_price = median(results_df$price_per_share),
    sd_price = sd(results_df$price_per_share),
    quantiles = quantile(results_df$price_per_share, probs = c(0.05, 0.25, 0.75, 0.95))
  )
  
  return(list(
    detailed_results = results_df,
    summary = summary_stats
  ))
}

base_inputs <- list(
  revenue_growth_rates = revenue_growth_rates,
  expense_ratios = expense_ratios,
  fixed_assets_params = fixed_assets_params,
  working_capital_ratios = working_capital_ratios,
  balance_sheet_ratios = balance_sheet_ratios,
  interest_rates = interest_rates,
  tax_rates = tax_rates,
  dcf_inputs = dcf_inputs
)

# Then run the Monte Carlo simulation
monte_carlo_results <- run_monte_carlo_dcf(
  base_inputs = base_inputs,
  n_scenarios = 100,
  historical_data = historical_data,
  historical_bs_data = historical_bs_data
)
