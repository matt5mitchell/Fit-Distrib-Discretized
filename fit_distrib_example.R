## Example: Fit distribution to discretized data

## Note: starting with reasonable parameters is important for optimization

## Weibull distribution is useful for time-to-event data, and reasonable parameters are:
## Shape = 1 (unless there is reason to believe there is a strong time effect)
## Scale = estimated mean (take a guess!)

#### Example: months homeless ----
# From Portland, OR 2019 Point-in-Time Count: 
# https://ahomeforeveryone.net/s/2019-PIT-Report_FINAL.pdf

# Discretized data to fit distribution to
target <- c(337, 288, 322, 999)

# Numeric "breaks" where the data has been discretized
breaks <- c(0, 7, 13, 25, 120) #0-6, 7-12, 13-24, 25+

# Initial parameters
par <- c(shape = 1, scale = 24)

# Fit distribution to discretized data
fit <- fit_distrib(par = par,
                   target = target,
                   breaks = breaks,
                   distrib = "weibull",
                   times = 10)
print(fit)
