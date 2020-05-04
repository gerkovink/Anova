# Read in the simulation data
load(file = "Workspaces/Simulations.RData")

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

EVAL01 <- map(SIM01, evaluate)
EVAL25 <- map(SIM25, evaluate)
EVAL50 <- map(SIM50, evaluate)
EVAL75 <- map(SIM75, evaluate)

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
F.out01 <- mapply(grab.F, EVAL01, SIMPLIFY = FALSE) %>% do.call(rbind, .)
F.out25 <- mapply(grab.F, EVAL25, SIMPLIFY = FALSE) %>% do.call(rbind, .)
F.out50 <- mapply(grab.F, EVAL50, SIMPLIFY = FALSE) %>% do.call(rbind, .)
F.out75 <- mapply(grab.F, EVAL75, SIMPLIFY = FALSE) %>% do.call(rbind, .)

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
p.out01 <- mapply(grab.p, EVAL01, SIMPLIFY = FALSE) %>% do.call(rbind, .)
p.out25 <- mapply(grab.p, EVAL25, SIMPLIFY = FALSE) %>% do.call(rbind, .)
p.out50 <- mapply(grab.p, EVAL50, SIMPLIFY = FALSE) %>% do.call(rbind, .)
p.out75 <- mapply(grab.p, EVAL75, SIMPLIFY = FALSE) %>% do.call(rbind, .)

# Save the evaluations and the resulting p-value and F-value objects
save(list = c("EVAL01", "EVAL25", "EVAL50", "EVAL75"), 
     file = "Workspaces/Evaluations_objects.RData")
save(list = c("F.out01", "F.out25", "F.out50", "F.out75", 
              "p.out01", "p.out25", "p.out50", "p.out75"), 
     file = "Workspaces/Evaluations_processed.RData")
