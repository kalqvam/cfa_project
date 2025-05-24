create_normal_distribution <- function(mean = 0, sd = 1) {
  force(mean)
  force(sd)
  
  function(n = 1) {
    rnorm(n, mean = mean, sd = sd)
  }
}

create_beta_distribution <- function(shape1 = 2, shape2 = 2, min = 0, max = 1) {
  force(shape1)
  force(shape2)
  force(min)
  force(max)
  
  function(n = 1) {
    x <- rbeta(n, shape1, shape2)
    
    if (min != 0 || max != 1) {
      x <- x * (max - min) + min
    }
    
    return(x)
  }
}

create_poisson_distribution <- function(lambda = 1) {
  force(lambda)
  
  function(n = 1) {
    rpois(n, lambda = lambda)
  }
}

generate_samples <- function(dist_fn, n = 100) {
  samples <- dist_fn(n)
  
  list(
    samples = samples,
    summary = summary(samples),
    sd = sd(samples)
  )
}

# Updated mean-reverting growth function with specific Monte Carlo logic
simulate_mean_reverting_growth_with_correlation <- function(
    initial_value,
    target_value,
    correlated_draws,
    theta = 0.2,
    sigma = 0.025
) {
  # Simulate for all periods at once using the specific logic from simulations.R
  period_1 <- initial_value + sigma * correlated_draws[1]
  period_2 <- period_1 * (1.25) + theta * (target_value - period_1) + sigma * correlated_draws[2]
  period_3 <- period_2 * (0.7) + theta * (target_value - period_2) + sigma * correlated_draws[3]
  period_4 <- target_value
  
  return(c(period_1 = period_1, 
           period_2 = period_2, 
           period_3 = period_3, 
           period_4 = period_4))
}

# Updated converging ratio function with specific Monte Carlo logic
simulate_converging_ratio_with_correlation <- function(
    initial_mean,
    target_mean,
    initial_sd,
    target_sd,
    correlated_draws,
    shape_param = 5,
    k = 0.025
) {
  # Input validation
  if (is.null(initial_mean) || is.null(target_mean) || is.null(initial_sd) || 
      is.na(initial_mean) || is.na(target_mean) || is.na(initial_sd) ||
      initial_mean == "" || target_mean == "" || initial_sd == "") {
    stop("Invalid inputs: all parameters must have valid numeric values")
  }
  
  # Create vector only for the actual draws
  values <- numeric(length(correlated_draws))
  
  for(i in 1:length(correlated_draws)) {
    unif_value <- pnorm(correlated_draws[i])
    
    # Ensure valid bounds for beta distribution
    mean_i <- max(0.001, min(0.999, initial_mean))
    sd_i <- min(initial_sd, sqrt(mean_i * (1 - mean_i)))
    variance <- sd_i^2
    
    # Safety checks for alpha/beta calculation
    if (variance <= 0 || mean_i <= 0 || mean_i >= 1) {
      stop(paste("Invalid parameters for beta distribution:",
                 "mean =", mean_i, "variance =", variance))
    }
    
    alpha <- (((1 - mean_i) / variance - 1/mean_i) * mean_i^2)
    beta <- (alpha * (1/mean_i - 1))
    
    alpha_corr <- alpha * (1 + k * correlated_draws[i])
    
    # Safety check for alpha/beta
    if (alpha <= 0 || beta <= 0) {
      stop(paste("Invalid alpha/beta parameters:",
                 "alpha =", alpha, "beta =", beta))
    }
    
    values[i] <- qbeta(unif_value, 20 * alpha_corr, 20 * beta)
  }
  
  # Add terminal value
  values <- c(values, target_mean)
  
  return(values)
}

generate_correlated_normals <- function(n_scenarios, corr_matrix) {
  mvrnorm(n_scenarios, 
          mu = rep(0, nrow(corr_matrix)), 
          Sigma = corr_matrix)
}

generate_period_correlated_draws <- function(corr_matrix) {
  draws <- mvrnorm(
    n = 1,
    mu = rep(0, nrow(corr_matrix)), 
    Sigma = corr_matrix
  )
  return(draws)
}

simulate_variable_group <- function(config, base_inputs, scenario_draws, scenario_draws_wc = NULL) {
  
  if (config$simulation_type == "mean_reverting") {
    
    results <- list()
    
    for (i in seq_along(config$regions)) {
      region <- config$regions[i]
      col_idx <- config$correlation_columns[i]
      
      # Get base values for this region
      initial_value <- base_inputs$revenue_growth_rates[[region]]$period_1
      target_value <- base_inputs$revenue_growth_rates[[region]]$period_4
      
      # Simulate with correlation
      simulated_values <- simulate_mean_reverting_growth_with_correlation(
        initial_value = initial_value,
        target_value = target_value,
        correlated_draws = scenario_draws[1:3, col_idx],
        theta = config$theta,
        sigma = config$sigma
      )
      
      results[[region]] <- as.list(simulated_values)
      names(results[[region]]) <- paste0("period_", 1:4)
    }
    
    return(results)
    
  } else if (config$simulation_type == "converging_ratio") {
    
    results <- list()
    
    for (i in seq_along(config$categories)) {
      category <- config$categories[i]
      col_idx <- config$correlation_columns[i]
      
      # Get base values for this category
      initial_mean <- base_inputs$expense_ratios[[category]]$period_1 + config$adjustments[i]
      target_mean <- base_inputs$expense_ratios[[category]]$period_4
      
      # Simulate with correlation
      simulated_values <- simulate_converging_ratio_with_correlation(
        initial_mean = initial_mean,
        target_mean = target_mean,
        initial_sd = config$initial_sds[i],
        target_sd = config$target_sds[i],
        correlated_draws = scenario_draws[1:3, col_idx]
      )
      
      results[[category]] <- as.list(simulated_values)
      names(results[[category]]) <- paste0("period_", 1:4)
    }
    
    return(results)
    
  } else if (config$simulation_type == "converging_ratio_beta") {
    
    results <- list()
    
    for (i in seq_along(config$categories)) {
      category <- config$categories[i]
      
      # Get base values for this category
      base_mean <- base_inputs$working_capital_ratios[[category]]$period_1
      
      # Simulate with correlation (using separate WC correlation matrix)
      simulated_values <- simulate_converging_ratio_with_correlation(
        initial_mean = base_mean,
        target_mean = base_mean,  # WC ratios stay constant over time
        initial_sd = config$base_sds[i] * config$convergence_factors[i],
        target_sd = config$base_sds[i] * 0.1,
        correlated_draws = scenario_draws_wc[1:3, i],
        shape_param = config$shape_params[i]
      )
      
      results[[category]] <- as.list(simulated_values[c(1:3, 6)])  # Select relevant periods
      names(results[[category]]) <- paste0("period_", 1:4)
    }
    
    return(results)
  }

  simulation_configs <- list(
  revenue_growth = list(
    regions = c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World"),
    correlation_columns = 1:4,
    simulation_type = "mean_reverting",
    theta = 0.2,
    sigma = 0.025,
    base_path = c("revenue_growth_rates", "Nordics"),  # How to access base values
    multipliers = c(1, 1.25, 0.7, 1)  # Period-specific multipliers for mean reversion
  ),
  
  expense_ratios = list(
    categories = c("materials_and_services", "employee_benefits", "other_operating_expenses"),
    correlation_columns = 5:7,
    simulation_type = "converging_ratio",
    adjustments = c(0, -0.045, -0.025),  # Category-specific mean adjustments
    initial_sds = c(0.005, 0.015, 0.015),
    target_sds = c(0.005, 0.005, 0.005),
    base_path = c("expense_ratios", "materials_and_services")  # How to access base values
  ),
  
  working_capital = list(
    categories = c("inventory_to_revenue", "trade_receivables_to_revenue", 
                   "trade_payables_to_revenue", "other_current_liab_to_revenue"),
    correlation_matrix = "nwc_corr_matrix",  # Uses separate correlation matrix
    simulation_type = "converging_ratio_beta",
    base_sds = c(0.025, 0.027, 0.013, 0.0225),
    shape_params = rep(5, 4),
    convergence_factors = c(0.7, 0.7, 0.7, 0.7),  # How much SD decreases over time
    base_path = c("working_capital_ratios", "inventory_to_revenue")
  )
)

}
