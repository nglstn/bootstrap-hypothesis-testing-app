bootstrap_samples <- function(x, num_bootstrap_samples, stat_func){
  n <- length(x)
  B <- num_bootstrap_samples
  bootstrap_samples <- replicate(
    B,
    stat_func(sample(x, size = n, replace = TRUE))
  )
  
  return(bootstrap_samples)
}

bootstrap_samples_bivariate_independent <- function(x, y, num_bootstrap_samples, stat_func, statistic_label){
  n_x <- length(x)
  n_y <- length(y)
  B <- num_bootstrap_samples
  if (statistic_label == "Mean" || statistic_label == "Median"){
    bootstrap_samples <- replicate(
      B,{
        x_b <- sample(x, size = n_x, replace = TRUE)
        y_b <- sample(y, size = n_y, replace = TRUE)
        stat_func(x_b)-stat_func(y_b)
      })
  }else{
    bootstrap_samples <- replicate(
      B,{
        x_b <- sample(x, size = n_x, replace = TRUE)
        y_b <- sample(y, size = n_y, replace = TRUE)
        stat_func(x_b)/stat_func(y_b)
      })}
  return(bootstrap_samples)
}


adjust_null_data <- function(x, test_statistic, null_value){
  
  x_null <- switch(
    test_statistic,
    Mean = x - mean(x) + null_value,
    Median = x - median(x) + null_value,
    Variance = mean(x) + (x-mean(x)) / sqrt(var(x)) * sqrt(null_value)
  )
  
  return(x_null)
}


adjust_null_data_bivariate_independent <- function(x, y, statistic_label, null_value, pooled_stat){
  
  
  if(statistic_label == "Mean"){
    x_null <- x - mean(x) + pooled_stat + null_value/2
    y_null <- y - mean(y) + pooled_stat - null_value/2
  }else if(statistic_label == "Median"){
    x_null <- x - median(x) + pooled_stat + null_value/2
    y_null <- y - median(y) + pooled_stat - null_value/2
  }else{
    x_null <- mean(x) + (x - mean(x)) / (sqrt(var(x))/sqrt(pooled_stat)) * (sqrt(null_value))
    y_null <- mean(y) + (y - mean(y)) / (sqrt(var(y))/sqrt(pooled_stat)) * (sqrt(null_value))
  }
  
  return(list(x_null = x_null, y_null = y_null))
}


compute_pvalue <- function(alternative, observed_stat, b_adjusted, null_value){
  
  p_value <- switch(
    alternative,
    two.sided = mean(abs(b_adjusted - null_value) >= abs(observed_stat - null_value)),
    greater = mean(b_adjusted >= observed_stat),
    less = mean(b_adjusted <= observed_stat)
  )
  
  return(p_value)
}

compute_ci <- function(bootstrap_samples, alpha, alternative){
  ci <- switch(
    alternative,
    two.sided = quantile(bootstrap_samples, probs = c(alpha/2, 1 - alpha/2), names = FALSE),
    greater = c(
      quantile(bootstrap_samples, probs = alpha),
      Inf
    ),
    less = c(
      -Inf,
      quantile(bootstrap_samples, probs = 1 - alpha)
    )
  )
  
  return(ci)
}



