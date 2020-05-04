library(mice)
library(miceadds)
library(magrittr)
library(dplyr)
library(purrr)
library(mvtnorm)
library(ggplot)
set.seed(123)

# Simulation parameters
nsim = 1000
rho = 0 # correlation set to zero to conform to null hypothesis (no effect)

# Generate data function
make.data <- function(n, correlation){
  data <- rmvnorm(n = n, mean = c(0, 0),
                  sigma = matrix(c(1, correlation, correlation, 1),
                                 nrow = 2, ncol = 2))
  colnames(data) <- c("y", "x")
  data %>% as_tibble() %>% return()
}

# simulation function
simulate <- function(n, rho, prop.mis = .75){
  # sample data from multivariate normal distribution
  data <- make.data(n = n, correlation = rho)
  # ampute data
  missing <- ampute(data, #ampute is a function from mice that makes missingness
                    patterns = matrix(c(0, 1, 1, 0), ncol = 2, nrow = 2, byrow = TRUE),
                    prop = prop.mis, 
                    mech = "MCAR")
  # impute data
  imp <- mice(missing$amp, method = "norm", m = 10, maxit = 10, print = FALSE)
  return(list(data = data,
              miss = missing,
              imp = imp))
}

# run simulation
SIM01 <- replicate(nsim, simulate(n = 1000, rho = rho, prop.mis = .01), simplify = FALSE)
SIM25 <- replicate(nsim, simulate(n = 1000, rho = rho, prop.mis = .25), simplify = FALSE)
SIM50 <- replicate(nsim, simulate(n = 1000, rho = rho, prop.mis = .50), simplify = FALSE)
SIM75 <- replicate(nsim, simulate(n = 1000, rho = rho, prop.mis = .75), simplify = FALSE)

# save sim, functions, seed, etc as a workspace dump
save.image(file = "Workspaces/Simulations.RData")
