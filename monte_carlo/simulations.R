#install.packages(c("Matrix", "MASS"))
library(Matrix)
library(MASS)
library(dplyr)

generate_simulation_inputs_v2 <- function(
    base_inputs,
    projection_periods = 2025:2030,
    n_scenarios = 100,
    apply_shocks = TRUE
) {
  
  # Extract base inputs
  base_growth_rates <- base_inputs$revenue_growth_rates
  base_expense_ratios <- base_inputs$expense_ratios
  
  # Generate correlation matrices
  corr_matrix <- create_correlation_matrix()
  nwc_corr_matrix <- create_nwc_correlation_matrix()
  n_years <- length(projection_periods)
  
  # Initialize shock tracking if enabled
  shock_impact_tracking <- if(apply_shocks) {
    list(
      eu_policy_shock = list(count = 0, avg_shock_severity = c(), before_growth = c(), after_growth = c()),
      na_policy_shock = list(count = 0, avg_shock_severity = c(), before_growth = c(), after_growth = c()),
      na_competition_shock = list(count = 0, avg_shock_severity = c(), before_growth = c(), after_growth = c()),
      tech_advancement_shock = list(count = 0, avg_shock_severity = c(), before_growth = c(), after_growth = c())
    )
  } else NULL
  
  simulated_scenarios <- vector("list", n_scenarios)
  
  for(scenario in 1:n_scenarios) {
    
    # Generate correlated draws for all variables
    scenario_draws <- generate_correlated_normals(n_years, corr_matrix)
    scenario_draws_wc <- generate_correlated_normals(n_years, nwc_corr_matrix)
    
    # Simulate each variable group using configs
    revenue_growth_rates <- simulate_variable_group(
      config = simulation_configs$revenue_growth,
      base_inputs = base_inputs,
      scenario_draws = scenario_draws
    )
    
    expense_ratios <- simulate_variable_group(
      config = simulation_configs$expense_ratios,
      base_inputs = base_inputs,
      scenario_draws = scenario_draws
    )
    
    working_capital_ratios <- simulate_variable_group(
      config = simulation_configs$working_capital,
      base_inputs = base_inputs,
      scenario_draws = scenario_draws,
      scenario_draws_wc = scenario_draws_wc
    )
    
    # Simulate fixed assets (doesn't fit standard pattern due to different correlation structure)
    rf_rate_draws <- rnorm(n_years)
    mrp_draws <- rnorm(n_years)
    ppe_draws <- rnorm(n_years)
    rou_draws <- rnorm(n_years)
    
    # Fixed assets simulation
    intangibles_mean <- base_inputs$fixed_assets_params$growth_rates$intangible_assets$period_1
    intangibles_sd <- 0.04
    intangibles_growth <- intangibles_mean + intangibles_sd * scenario_draws[, 8]
    
    ppe_mean <- base_inputs$fixed_assets_params$growth_rates$ppe_owned$period_1
    ppe_sd <- 0.08
    ppe_growth <- ppe_mean + ppe_sd * ppe_draws
    
    rou_mean <- base_inputs$fixed_assets_params$growth_rates$ppe_rou$period_1
    rou_sd <- 0.06
    rou_growth <- rou_mean + rou_sd * rou_draws
    
    # Apply bounds
    intangibles_growth <- pmax(0, intangibles_growth)
    ppe_growth <- pmax(0, ppe_growth)
    rou_growth <- pmax(0, rou_growth)
    
    # Create fixed assets parameters
    new_fixed_assets_params <- base_inputs$fixed_assets_params
    new_fixed_assets_params$growth_rates$intangible_assets <- as.list(intangibles_growth[c(1:3, 6)])
    new_fixed_assets_params$growth_rates$ppe_owned <- as.list(ppe_growth[c(1:3, 6)])
    new_fixed_assets_params$growth_rates$ppe_rou <- as.list(rou_growth[c(1:3, 6)])
    names(new_fixed_assets_params$growth_rates$intangible_assets) <- paste0("period_", 1:4)
    names(new_fixed_assets_params$growth_rates$ppe_owned) <- paste0("period_", 1:4)
    names(new_fixed_assets_params$growth_rates$ppe_rou) <- paste0("period_", 1:4)
    
    # DCF parameters simulation
    rf_mean <- base_inputs$dcf_inputs$risk_free_rate
    rf_sd <- 0.003
    rf_rate <- rf_mean + rf_sd * rf_rate_draws
    
    mrp_mean <- base_inputs$dcf_inputs$market_risk_premium
    mrp_sd <- 0.01
    mrp <- mrp_mean + mrp_sd * mrp_draws
    
    # Apply bounds
    rf_rate <- pmax(0, rf_rate)
    mrp <- pmax(0.02, mrp)
    
    # Create DCF inputs
    new_dcf_inputs <- base_inputs$dcf_inputs
    new_dcf_inputs$risk_free_rate <- rf_rate[1]
    new_dcf_inputs$market_risk_premium <- mrp[1]
    
    # Apply shocks if enabled
    if(apply_shocks) {
      shock_result <- apply_shock_scenarios(
        growth_rates = revenue_growth_rates, 
        shock_scenarios = shock_scenarios,
        shock_tracking = shock_impact_tracking
      )
      revenue_growth_rates <- shock_result$growth_rates
      shock_impact_tracking <- shock_result$shock_tracking
    }
    
    # Store complete scenario
    simulated_scenarios[[scenario]] <- list(
      revenue_growth_rates = revenue_growth_rates,
      expense_ratios = expense_ratios,
      working_capital_ratios = working_capital_ratios,
      fixed_assets_params = new_fixed_assets_params,
      dcf_inputs = new_dcf_inputs
    )
  }
  
  # Print shock summary if enabled
  if(apply_shocks) {
    for(shock_name in names(shock_scenarios)) {
      cat("\nShock Scenario:", shock_name, "\n")
      cat("Number of Occurrences:", shock_impact_tracking[[shock_name]]$count, "\n")
      if(length(shock_impact_tracking[[shock_name]]$avg_shock_severity) > 0) {
        cat("Average Shock Severity:", mean(shock_impact_tracking[[shock_name]]$avg_shock_severity), "\n")
        cat("Average Growth Before Shock:", mean(shock_impact_tracking[[shock_name]]$before_growth), "\n")
        cat("Average Growth After Shock:", mean(shock_impact_tracking[[shock_name]]$after_growth), "\n")
      }
    }
  }
  
  return(simulated_scenarios)
}

# Helper function for applying shocks (extracted for clarity)
apply_shock_scenarios <- function(growth_rates, shock_scenarios, shock_tracking = NULL) {
  
  for(shock_name in names(shock_scenarios)) {
    shock_info <- shock_scenarios[[shock_name]]
    
    if(shock_info$shock_distribution() > 0) {
      
      # Update tracking if provided
      if(!is.null(shock_tracking)) {
        shock_tracking[[shock_name]]$count <- shock_tracking[[shock_name]]$count + 1
      }
      
      shock_k <- shock_info$shock_severity()
      
      # Update tracking
      if(!is.null(shock_tracking)) {
        shock_tracking[[shock_name]]$avg_shock_severity <- 
          c(shock_tracking[[shock_name]]$avg_shock_severity, shock_k)
      }
      
      for(region in shock_info$regions) {
        for(period_label in paste0("period_", 1:4)) {
          
          # Track before/after if tracking enabled
          if(!is.null(shock_tracking)) {
            before_growth <- growth_rates[[region]][[period_label]]
            shock_tracking[[shock_name]]$before_growth <- 
              c(shock_tracking[[shock_name]]$before_growth, as.numeric(before_growth))
          }
          
          # Apply shock
          growth_rates[[region]][[period_label]] <- 
            growth_rates[[region]][[period_label]] * shock_k
          
          # Track after
          if(!is.null(shock_tracking)) {
            after_growth <- growth_rates[[region]][[period_label]]
            shock_tracking[[shock_name]]$after_growth <- 
              c(shock_tracking[[shock_name]]$after_growth, as.numeric(after_growth))
          }
        }
      }
    }
  }
  
  return(list(
    growth_rates = growth_rates,
    shock_tracking = shock_tracking
  ))
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
