library(mice)
library(magrittr)
library(dplyr)
library(purrr)
library(mvtnorm)
set.seed(123)

# Simulation parameters
nsim = 100
rho = .5 # correlation

# simulation function
simulate <- function(n, correlation = rho){
  # sample data from multivariate normal distribution
  data <- rmvnorm(n = n, mean = c(0, 0), 
                  sigma = matrix(c(1, correlation, correlation, 1), 
                                 nrow = 2, ncol = 2))
  colnames(data) <- c("y", "x")
  data <- data %>% as_tibble()
  # ampute data
  missing <- ampute(data, 
                    patterns = matrix(c(0, 1), ncol = 2, nrow = 1, byrow = TRUE),
                    mech = "MCAR")
  # impute data
  imp <- mice(missing$amp, method = "norm", m = 10, maxit = 10, print = FALSE)
  # calculate true data statistics
  fit <- data %$% lm(y ~ x)
  anova <- fit %>% anova 
  # calculate imputed data statistics
  fit.mis <- missing$amp %$% lm(y ~ x)
  anova.mis <- fit.mis %>% anova
  # calculate imputed data statistics
  fit.imp<- imp %>%
    mice::complete("all") %>%
    map(lm, formula = y ~ x)
  fit.imp.empty <- imp %>%
    mice::complete("all") %>%
    map(lm, formula = y ~ 1)
  fit.anova.D1 <- D1(fit.imp, fit.imp.empty)
  fit.anova.D2 <- D2(fit.imp, fit.imp.empty) #cf. micombine.F from miceadds
  #fit.anova.D3 <- D3(fit.imp, fit.imp.empty)
  avg.F.imp <- mean(unlist(with(imp, anova(lm(y ~ x))$'F value'[1])$analyses))
  avg.p.imp <- mean(unlist(with(imp, anova(lm(y ~ x))$'Pr(>F)'[1])$analyses))
  # prepare output for return
  return(list(data = data, 
              miss = missing,
              imp = imp, 
              stats = list(true = list(fit = fit, anova = anova),
                           miss = list(fit = fit.mis, anova = anova.mis),
                           imp = list(fit = fit.imp, emptyfit = fit.imp.empty, 
                                      D1 = fit.anova.D1, 
                                      D2 = fit.anova.D2, 
                                      Fbar = avg.F.imp, 
                                      pbar = avg.p.imp))))
}

# run simulation
result <- replicate(nsim, simulate(n = 1000), simplify = FALSE)

# TODO: write evaluation code
