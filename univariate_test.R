bootstrap_test_univariate <- function(x, null_value, alternative = c("two.sided", "greater", "less"), alpha, num_bootstrap, test_statistic = c("Mean","Median","Variance"), seed){
  
  
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }
  
  x <- x[!is.na(x)]
  
  if (length(x) < 2) {
    stop("`x` must contain at least two non-missing values.")
  }
  
  if (!is.numeric(null_value) || length(null_value) != 1) {
    stop("`null_value` must be a single numeric value.")
  }
  
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be between 0 and 1.")
  }
  
  if (!is.numeric(num_bootstrap) || num_bootstrap <= 0) {
    stop("`num_bootstrap` must be a positive integer.")
  }
  
  
  alternative    <- match.arg(alternative)
  test_statistic <- match.arg(test_statistic)
  
  
  if (!is.null(seed)) set.seed(seed)
  
  
  # Define statistic and null-transformed data
  stat_func <- switch(
    test_statistic,
    Mean     = mean,
    Median   = median,
    Variance = var
  )
  
  x_null <- adjust_null_data(x, test_statistic, null_value)
  
  # Bootstrap under null (for testing) and from original data (for confidence intervals)
  b_adjusted <- bootstrap_samples(x_null, num_bootstrap, stat_func)
  
  b_samples <- bootstrap_samples(x, num_bootstrap, stat_func)
  
  observed_stat <- stat_func(x)
  
  p_value <- compute_pvalue(alternative, observed_stat, b_adjusted, null_value)
  
  conf_int <- compute_ci(b_samples, alpha, alternative)
  
  
  out <- list(
    statistic = test_statistic,
    null_value = null_value,
    significance_level = alpha,
    p_value = p_value,
    conf_int = conf_int,
    alternative = alternative,
    method = paste("Bootstrap test for", test_statistic),
    B = num_bootstrap,
    bootstrap_samples = b_samples
  )
  return(out)
}


