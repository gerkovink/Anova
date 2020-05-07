load(file = "Workspaces/Evaluations_processed.RData")
library(ggplot2)
library(gridExtra)

# plot function
qqplot <- function(x, title = "ECDF and theoretical CDF"){
  ggplot(aes(x), data = data.frame(x)) +
    stat_function(fun=punif, 
                  args=list(0, 1), 
                  col = "orange", 
                  lwd = 1.1) +
    stat_ecdf() +
    labs(title = title) + 
    xlab("Empirical CDF") + 
    ylab("CDF") + 
    xlim(0, 1)
}

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

#side by side
p1 <- qqplot(p.out75$D1, title = "D1")
p2 <- qqplot(p.out75$D2, title = "D2")
p3 <- qqplot(p.out75$D3, title = "D3")
grid.arrange(p1, p2, p3, 
             nrow = 1, respect=TRUE, 
             top = "ECDF and theoretical CDF")

