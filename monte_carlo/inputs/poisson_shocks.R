# monte_carlo/inputs/poisson_shocks.R

# Source the distribution creation function
source("monte_carlo/inputs/function_ini.R")

# Consolidated shock severity configurations
shock_severity_configs <- list(
  eu_policy = list(
    levels = c(0.9, 0.75, 0.5, 0.2),
    probabilities = c(0.5, 0.30, 0.15, 0.05)
  ),
  na_policy = list(
    levels = c(0.9, 0.75, 0.5, 0.2),
    probabilities = c(0.4, 0.30, 0.20, 0.10)
  ),
  na_competition = list(
    levels = c(0.9, 0.75, 0.5, 0.2),
    probabilities = c(0.4, 0.30, 0.20, 0.10)
  ),
  tech_advancement = list(
    levels = c(0.6, 0.75, 0.9, 1.1, 1.25, 1.4),
    probabilities = c(0.05, 0.15, 0.2, 0.2, 0.25, 0.15)
  )
)

# Simplified shock severity function generator
create_shock_severity_function <- function(config) {
  function() {
    sample(config$levels, 1, prob = config$probabilities)
  }
}

# Main shock scenarios configuration
shock_scenarios <- list(
  eu_policy_shock = list(
    regions = c("Nordics", "Rest_of_Europe"),
    shock_distribution = create_poisson_distribution(lambda = 0.15),
    shock_severity = create_shock_severity_function(shock_severity_configs$eu_policy)
  ),
  na_policy_shock = list(
    regions = c("North_America"),
    shock_distribution = create_poisson_distribution(lambda = 0.25),
    shock_severity = create_shock_severity_function(shock_severity_configs$na_policy)
  ),
  na_competition_shock = list(
    regions = c("North_America"),
    shock_distribution = create_poisson_distribution(lambda = 0.3),
    shock_severity = create_shock_severity_function(shock_severity_configs$na_competition)
  ),
  tech_advancement_shock = list(
    regions = c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World"),
    shock_distribution = create_poisson_distribution(lambda = 0.5),
    shock_severity = create_shock_severity_function(shock_severity_configs$tech_advancement)
  )
)
