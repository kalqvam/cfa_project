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

simulate_mean_reverting_growth_with_correlation <- function(
    initial_value,
    target_value,
    theta,
    sigma,
    correlated_draw
) {
  dX <- theta * (target_value - initial_value) + sigma * correlated_draw
  new_value <- initial_value + dX - 0.025
  
  return(new_value)
}

simulate_converging_ratio_with_correlation <- function(
    initial_mean,
    target_mean,
    initial_sd,
    target_sd,
    correlated_draw,
    shape_param = 5
) {
  unif_value <- pnorm(correlated_draw)
  
  mean_i <- max(0.001, min(0.999, initial_mean))
  sd_i <- min(initial_sd, sqrt(mean_i * (1 - mean_i)))
  variance <- sd_i^2
  alpha <- ((1 - mean_i) / variance - 1/mean_i) * mean_i^2
  beta <- alpha * (1/mean_i - 1)
  
  value <- qbeta(unif_value, alpha, beta)
  
  return(value)
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
