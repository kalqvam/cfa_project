shock_scenarios <- list(
  eu_policy_shock = list(
    regions = c("Nordics", "Rest_of_Europe"),
    lambda = 0.15,
    shock_distribution = create_poisson_distribution(lambda = 0.15),
    shock_severity = function() {
      possible_k <- c(0.9, 0.75, 0.5, 0.2)
      probabilities <- c(0.5, 0.30, 0.15, 0.05)
      sample(possible_k, 1, prob = probabilities)
    }
  ),
  na_policy_shock = list(
    regions = c("North_America"),
    lambda = 0.25,
    shock_distribution = create_poisson_distribution(lambda = 0.25),
    shock_severity = function() {
      possible_k <- c(0.9, 0.75, 0.5, 0.2)
      probabilities <- c(0.4, 0.30, 0.20, 0.10)
      sample(possible_k, 1, prob = probabilities)
    }
  ),
  na_competition_shock = list(
    regions = c("North_America"),
    lambda = 0.3,
    shock_distribution = create_poisson_distribution(lambda = 0.3),
    shock_severity = function() {
      possible_k <- c(0.9, 0.75, 0.5, 0.2)
      probabilities <- c(0.4, 0.30, 0.20, 0.10)
      sample(possible_k, 1, prob = probabilities)
    }
  ),
  tech_advancement_shock = list(
    regions = c("Nordics", "Rest_of_Europe", "North_America", "Rest_of_World"),
    lambda = 0.5,
    shock_distribution = create_poisson_distribution(lambda = 0.5),
    shock_severity = function() {
      possible_k <- c(0.6, 0.75, 0.9, 1.1, 1.25, 1.4)
      probabilities <- c(0.05, 0.15, 0.2, 0.2, 0.25, 0.15)
      sample(possible_k, 1, prob = probabilities)
    }
  )
)
