## Estimate data from distribution to compare to discretized data
## This function is used in fit_distrib optimizer function

distrib_fn <- function(par, 
                       target, 
                       breaks, #breaks are closed on left, open on right
                       distrib = c("norm", "lnorm", "gamma", "nbinom", "weibull"), 
                       output = c("all", "error")) {
  
  distrib <- match.arg(distrib)
  
  output <- match.arg(output)
  
  # number of observations
  n <- sum(target)
  
  # random draws from distribution
  draws <- switch(
    distrib,
    norm = rnorm(n, mean = par[1], sd = par[2]),
    lnorm = rlnorm(n, meanlog = par[1], sdlog = par[2]),
    gamma = rgamma(n, shape = par[1], rate = par[2]),
    nbinom = rnbinom(n, size = par[1], prob = par[2]),
    weibull = rweibull(n, shape = par[1], scale = par[2])
  )
  
  # Replace NaN with 0 if parameters are out of bounds
  draws[is.na(draws)] <- 0
  
  # draws discretized by breaks
  result <- as.vector(table(cut(draws, breaks = breaks, right = FALSE)))
  
  # calculate error
  err <- abs((target - result)/target) # absolute value of proportional error
  err_sum <- sum(err) # sum of error
  
  out <- switch(
    output,
    all = list(target = target, 
               result = result,
               breaks = breaks,
               par = par,
               error = err_sum),
    error = err_sum
  )
  
  return(out)
}