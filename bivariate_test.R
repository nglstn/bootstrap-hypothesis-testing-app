bootstrap_test_diff <- function(x, y, null_value, alternative = c("two.sided", "greater", "less"), alpha, num_bootstrap, test_statistic = c("Mean","Median","Variance"), seed, paired = FALSE){
  
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }
  
  x <- x[!is.na(x)]
  
  if (length(x) < 2) {
    stop("`x` must contain at least two non-missing values.")
  }
  
  if (!is.numeric(y)) {
    stop("`y` must be numeric.")
  }
  
  y <- y[!is.na(y)]
  
  if (length(y) < 2) {
    stop("`y` must contain at least two non-missing values.")
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
  
  
  stat_func <- switch(
    test_statistic,
    Mean     = mean,
    Median   = median,
    Variance = var
  )
  
  if (paired) {
    # Check length
    if (length(x) != length(y)) {
      stop("For paired tests, x and y must be the same length.")
    }
    
    diff <- x - y
    
    
    
    diff_null <- adjust_null_data(diff, test_statistic, null_value)
    
    # Bootstrap under null (for testing) and from original data (for confidence intervals)
    b_adjusted <- bootstrap_samples(diff_null, num_bootstrap, stat_func)
    
    b_samples <- bootstrap_samples(diff, num_bootstrap, stat_func)
    
    observed_stat <- stat_func(diff)
    
  }else{
    z <- c(x,y)
    
    var_pooled <- ((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) / (length(x) + length(y) - 2)
    
    pooled_stat <- switch(
      test_statistic,
      Mean     = mean(z),
      Median   = median(z),
      Variance = var_pooled
    )
    
   
    null_data <- adjust_null_data_bivariate_independent(x, y, test_statistic, null_value, pooled_stat)
    
    x_null <- null_data$x_null
    y_null <- null_data$y_null
    
    # Bootstrap under null (for testing) and from original data (for confidence intervals)
    
    b_adjusted <- bootstrap_samples_bivariate_independent(x_null, y_null, num_bootstrap, stat_func, test_statistic)
    
    b_samples <- bootstrap_samples_bivariate_independent(x, y, num_bootstrap, stat_func, test_statistic)
    
    if (test_statistic == "Mean" || test_statistic == "Median"){
      observed_stat <- stat_func(x)-stat_func(y)
    }else{
      observed_stat <- stat_func(x)/stat_func(y)}
  }
  
  
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
