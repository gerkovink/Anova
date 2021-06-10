load(file = "Workspaces/Evaluations_processed.RData")
library(ggplot2)
library(dplyr)
library(magrittr)
library(gridExtra)

# old plot function
qqplot.old <- function(x, title = "ECDF and theoretical CDF"){
  ggplot(aes(x), data = data.frame(x)) +
    stat_function(fun=punif,
                  args=list(0, 1),
                  col = "orange",
                  lwd = 1.1) +
    stat_ecdf(aes(x = x), pad = FALSE) +
    labs(title = title) +
    xlab("Empirical CDF") +
    ylab("CDF") +
    xlim(0, 1)
}

# plot function
qqplot <- function(x, title = "ECDF and theoretical CDF"){
  d <- data.frame(x = x)
  d %>% ggplot(aes(sample = x)) +
    geom_abline(intercept = 0, slope = 1, colour = "orange", lwd = 1.5) + 
    stat_qq(distribution = qunif, 
            dparams = list(min = 0, max = 1), geom = "line", lwd = 1.2) +
    xlim(0, 1) + 
    ylim(0, 1) + 
    coord_flip() + 
    labs(title = title) + 
    ylab("Empirical CDF") +  
    xlab("CDF") 
}

# Normal plot example
qqplot(p.out01$true) 
# Adjusted plot example
qqplot(p.out01$true) + xlim(0, .1) + ylim(0, .1)

# 01% missingness
qqplot(p.out01$true) 
qqplot(p.out01$mis)
qqplot(p.out01$D1)
qqplot(p.out01$D2)
qqplot(p.out01$D3)
qqplot(p.out01$micombine)
qqplot(p.out01$pbar)

# 25% missingness
qqplot(p.out25$true)
qqplot(p.out25$mis)
qqplot(p.out25$D1)
qqplot(p.out25$D2)
qqplot(p.out25$D3)
qqplot(p.out25$micombine)
qqplot(p.out25$pbar)

# 50% missingness
qqplot(p.out50$true)
qqplot(p.out50$mis)
qqplot(p.out50$D1)
qqplot(p.out50$D2)
qqplot(p.out50$D3)
qqplot(p.out50$micombine)
qqplot(p.out50$pbar)

# 75% missingness
qqplot(p.out75$true)
qqplot(p.out75$mis)
qqplot(p.out75$D1)
qqplot(p.out75$D2)
qqplot(p.out75$D3)
qqplot(p.out75$micombine)
qqplot(p.out75$pbar)

# 75% missingness adjusted
qqplot(p.out75$true) + xlim(0, .1) + ylim(0, .1)
qqplot(p.out75$mis) + xlim(0, .1) + ylim(0, .1)
qqplot(p.out75$D1) + xlim(0, .1) + ylim(0, .1)
qqplot(p.out75$D2) + xlim(0, .1) + ylim(0, .1)
qqplot(p.out75$D3) + xlim(0, .1) + ylim(0, .1)
qqplot(p.out75$micombine) + xlim(0, .1) + ylim(0, .1)
qqplot(p.out75$pbar) + xlim(0, .1) + ylim(0, .1)

#side by side
p1 <- qqplot(p.out75$D1, title = "D1")
p2 <- qqplot(p.out75$D2, title = "D2")
p3 <- qqplot(p.out75$D3, title = "D3")
grid.arrange(p1, p2, p3, 
             nrow = 1, respect=TRUE, 
             top = "ECDF and theoretical CDF")

#side by side
p1 <- qqplot(p.out01$true, title = "true")
p2 <- qqplot(p.out25$true, title = "true")
p3 <- qqplot(p.out50$true, title = "true")
p4 <- qqplot(p.out75$true, title = "true")

p5 <- qqplot(p.out01$mis, title = "CCA 1% missing")
p6 <- qqplot(p.out25$mis, title = "CCA 25% missing")
p7 <- qqplot(p.out50$mis, title = "CCA 50% missing")
p8 <- qqplot(p.out75$mis, title = "CCA 75% missing")

p9 <- qqplot(p.out01$D1, title = "D1 1% missing")
p10 <- qqplot(p.out25$D1, title = "D1 25% missing")
p11 <- qqplot(p.out50$D1, title = "D1 50% missing")
p12 <- qqplot(p.out75$D1, title = "D1 75% missing")

p13 <- qqplot(p.out01$D2, title = "D2 1% missing")
p14 <- qqplot(p.out25$D2, title = "D2 25% missing")
p15 <- qqplot(p.out50$D2, title = "D2 50% missing")
p16 <- qqplot(p.out75$D2, title = "D2 75% missing")

p17 <- qqplot(p.out01$D3, title = "D3 1% missing")
p18 <- qqplot(p.out25$D3, title = "D3 25% missing")
p19 <- qqplot(p.out50$D3, title = "D3 50% missing")
p20 <- qqplot(p.out75$D3, title = "D3 75% missing")

p21 <- qqplot(p.out01$pbar, title = "pbar 1% missing")
p22 <- qqplot(p.out25$pbar, title = "pbar 25% missing")
p23 <- qqplot(p.out50$pbar, title = "pbar 50% missing")
p24 <- qqplot(p.out75$pbar, title = "pbar 75% missing")

p25 <- qqplot(p.out01$pmed, title = "pmed 1% missing")
p26 <- qqplot(p.out25$pmed, title = "pmed 25% missing")
p27 <- qqplot(p.out50$pmed, title = "pmed 50% missing")
p28 <- qqplot(p.out75$pmed, title = "pmed 75% missing")

grid.arrange(p1, p2, p3, p4, 
             p5, p6, p7, p8,
             p9, p10, p11, p12, 
             p13, p14, p15, p16, 
             p17, p18, p19, p20,
             p21, p22, p23, p24,
             p25, p26, p27, p28, 
             nrow = 7, ncol = 4, respect=TRUE, 
             top = "ECDF and theoretical CDF")

