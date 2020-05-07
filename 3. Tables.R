load("Workspaces/Evaluations_processed.RData")

# Test whether the p-values follow a uniform distribution
apply(p.out01, 2, function(x) ks.test(x, "punif", 0, 1))
apply(p.out25, 2, function(x) ks.test(x, "punif", 0, 1))
apply(p.out50, 2, function(x) ks.test(x, "punif", 0, 1))
apply(p.out75, 2, function(x) ks.test(x, "punif", 0, 1))

# Test how many p=values <.05 (we expect .05)
# apply(p.out25, 2, function(x) mean(x < .05))

data.frame(mis01 = apply(p.out01, 2, function(x) mean(x < .05)),
           mis25 = apply(p.out25, 2, function(x) mean(x < .05)),
           mis50 = apply(p.out50, 2, function(x) mean(x < .05)),
           mis75 = apply(p.out75, 2, function(x) mean(x < .05)))
           
# What is the mean p-value
# apply(p.out, 2, function(x) mean(x))

data.frame(mis01 = apply(p.out01, 2, function(x) mean(x)),
           mis25 = apply(p.out25, 2, function(x) mean(x)),
           mis50 = apply(p.out50, 2, function(x) mean(x)),
           mis75 = apply(p.out75, 2, function(x) mean(x)))

# Bias in p-value with respect to the population p=value 
# population = .5 under null hypothesis
# apply(p.out, 2, function(x) mean(x - .5))

data.frame(mis01 = apply(p.out01, 2, function(x) mean(x - .5)),
           mis25 = apply(p.out25, 2, function(x) mean(x - .5)),
           mis50 = apply(p.out50, 2, function(x) mean(x - .5)),
           mis75 = apply(p.out75, 2, function(x) mean(x - .5)))

# Bias in p-value with respect to the sampled complete p=value 
# population = .5 under null hypothesis
# truth <- p.out$true
# apply(p.out, 2, function(x) mean(x - truth))

truth01 <- p.out01$true
truth25 <- p.out25$true
truth50 <- p.out50$true
truth75 <- p.out75$true
data.frame(mis01 = apply(p.out01, 2, function(x) mean(x - truth01)),
           mis25 = apply(p.out25, 2, function(x) mean(x - truth25)),
           mis50 = apply(p.out50, 2, function(x) mean(x - truth50)),
           mis75 = apply(p.out75, 2, function(x) mean(x - truth75)))

# Bias in p-value with respect to the sampled incomplete p=value 
# population = .5 under null hypothesis
# mis <- p.out$mis
# apply(p.out, 2, function(x) mean(x - mis))

mis01 <- p.out01$mis
mis25 <- p.out25$mis
mis50 <- p.out50$mis
mis75 <- p.out75$mis
data.frame(mis01 = apply(p.out01, 2, function(x) mean(x - mis01)),
           mis25 = apply(p.out25, 2, function(x) mean(x - mis25)),
           mis50 = apply(p.out50, 2, function(x) mean(x - mis50)),
           mis75 = apply(p.out75, 2, function(x) mean(x - mis75)))

# Correlations with truth
cor(p.out01)[1, ]
cor(p.out25)[1, ]
cor(p.out50)[1, ]
cor(p.out75)[1, ]

# proportions correct
sign01 <- p.out01 < .05
sign25 <- p.out25 < .05
sign50 <- p.out50 < .05
sign75 <- p.out75 < .05

p.correct01 <- apply(sign01, 2, function(x) mean(x == sign01[, 1]))
p.correct25 <- apply(sign25, 2, function(x) mean(x == sign25[, 1]))
p.correct50 <- apply(sign50, 2, function(x) mean(x == sign50[, 1]))
p.correct75 <- apply(sign75, 2, function(x) mean(x == sign75[, 1]))

