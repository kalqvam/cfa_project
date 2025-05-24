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

create_shock_severity_function <- function(config) {
  function() {
    sample(config$levels, 1, prob = config$probabilities)
  }
}
