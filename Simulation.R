library(mice)
library(magrittr)
library(dplyr)
library(purrr)
library(mvtnorm)
set.seed(123)

# Simulation parameters
nsim = 100
correlation = .5

# simulation function
simulate <- function(n){
  # sample data from multivariate normal distribution
  data <- rmvnorm(n = n, mean = c(0, 0), 
                  sigma = matrix(c(1, correlation, correlation, 1), 
                                 nrow = 2, ncol = 2))
  # ampute data
  missing <- ampute(data, 
                    patterns = matrix(c( 0, 1), ncol = 2, nrow = 1, byrow = TRUE),
                    mech = "MCAR")
  # impute data
  imp <- mice(missing$amp, method = "norm", m = 10, maxit = 10, print = FALSE)
  # calculate true data statistics
  fit <- data %$% lm(y ~ x)
  anova <- fit %>% anova 
  # calculate imputed data statistics
  #
  # prepare output for return
  return(list(data = data, 
              miss = missing,
              imp = imp, 
              fit = fit,
              anova = anova))
}

# run simulation
result <- replicate(nsim, simulate(n = 1000), simplify = FALSE)
