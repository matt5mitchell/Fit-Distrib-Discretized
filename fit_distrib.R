## Fit distribution to discretized data
## Runs distrib_fn through optimizer multiple times to find best fit


fit_distrib <- function(par,
                        target,
                        breaks,
                        distrib = c("norm", "lnorm", "gamma", "nbinom", "weibull"),
                        times = 10,
                        ...) {
  
  # method for optim function
  method <- if (hasArg(method)) {method} else {"BFGS"}
  
  par_opt <- list() # list to store parameters from optimization
  error <- c() # vector to store errors from optimization
  
  # run optimizer multiple times and save results
  for (i in 1:times) {
    tmp <- optim(par, 
                 distrib_fn, 
                 target = target, 
                 breaks = breaks, 
                 distrib = "weibull", 
                 output = "error",
    )
    
    par_opt[[i]] <- tmp$par
    error[i] <- tmp$value
  }
  
  # parameters with minimum error
  par_best <- par_opt[[which(error == min(error))]]
  
  # run best parameters through distrib_fn to get discretized results
  result <- distrib_fn(par_best, 
                       target = target, 
                       breaks = breaks, 
                       distrib = "weibull", 
                       output = "all")
  
  # output
  out <- list(
    distrib = distrib,
    par = par_best,
    result = data.frame(result[c(1,2)]),
    opt_method = method,
    times = times
  )
  return(out)
}