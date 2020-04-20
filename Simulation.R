library(mice)
library(miceadds)
library(magrittr)
library(dplyr)
library(purrr)
library(mvtnorm)
set.seed(123)

# Simulation parameters
nsim = 1000
rho = .5 # correlation

# Generate data function
make.data <- function(n, correlation){
  data <- rmvnorm(n = n, mean = c(0, 0),
                  sigma = matrix(c(1, correlation, correlation, 1),
                                 nrow = 2, ncol = 2))
  colnames(data) <- c("y", "x")
  data %>% as_tibble() %>% return()
}

# simulation function
simulate <- function(n, rho){
  # sample data from multivariate normal distribution
  data <- make.data(n = n, correlation = rho)
  # ampute data
  missing <- ampute(data, #ampute is a function from mice that makes missingness
                    patterns = matrix(c(0, 1, 1, 0), ncol = 2, nrow = 2, byrow = TRUE),
                    mech = "MCAR")
  # impute data
  imp <- mice(missing$amp, method = "norm", m = 10, maxit = 10, print = FALSE)
  return(list(data = data,
              miss = missing,
              imp = imp))
}

# run simulation
SIM <- replicate(nsim, simulate(n = 1000, rho = rho), simplify = FALSE)

# evaluation function
evaluate <- function(sim){
  # calculate true data statistics
  fit <- sim$data %$% lm(y ~ x) 
  anova <- fit %>% anova
  # calculate observed data statistics
  fit.mis <- sim$miss$amp %$% lm(y ~ x)
  anova.mis <- fit.mis %>% anova
  # calculate imputed data statistics
  fit.imp <- with(data = sim$imp, expr = lm(y ~ x))
  fit.imp.empty <- with(data = sim$imp, expr = lm(y ~ 1))
  fit.imp.pool <- pool(fit.imp)
  fit.anova.D1 <- D1(fit.imp, fit.imp.empty)
  fit.anova.D2 <- D2(fit.imp, fit.imp.empty) #cf. micombine.F from miceadds
  fit.anova.D3 <- D3(fit.imp, fit.imp.empty)
  F.vector <- unlist(with(sim$imp, anova(lm(y ~ x))$'F value'[1])$analyses)
  micomb <- micombine.F(F.vector, anova$Df[[1]], display = FALSE)
  avg.F.imp <- mean(F.vector)
  avg.p.imp <- mean(unlist(with(sim$imp, anova(lm(y ~ x))$'Pr(>F)'[1])$analyses))
  # prepare output for return
  return(list(truefit = fit, 
              trueanova = anova,
              missfit = fit.mis, 
              missanova = anova.mis,
              fit = fit.imp, 
              emptyfit = fit.imp.empty,
              pool = fit.imp.pool,
              D1 = fit.anova.D1,
              D2 = fit.anova.D2,
              D3 = fit.anova.D3,
              micomb = round(micomb, 4),
              Fvector = F.vector, 
              Fbar = avg.F.imp,
              pbar = avg.p.imp))
}

EVAL <- map(SIM, evaluate)

# Grab Anova's
grab.F <- function(x){
  data.frame(true = x$trueanova$`F value`[1], 
             mis = x$missanova$`F value`[1], 
             D1 = x$D1$result[1],
             D2 = x$D2$result[1],
             D3 = x$D3$result[1], 
             micombine = x$micomb[1],
             Fbar = x$Fbar)
}
mapply(grab.F, EVAL)

# Grab p-values
grab.p <- function(x){
  data.frame(true = x$trueanova$`Pr(>F)`[1], 
             mis = x$missanova$`Pr(>F)`[1], 
             D1 = x$D1$result[4],
             D2 = x$D2$result[4],
             D3 = x$D3$result[4], 
             micombine = x$micomb[2],
             pbar = x$pbar)
}
mapply(grab.p, EVAL)
# TODO evaluation/plotting script
