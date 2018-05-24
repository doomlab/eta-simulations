means = c(2.5, 3, 3.5, 4, 4.5, 5)
gm = mean(means)
ss = sum((means - gm)^2)/length(means)

h = ss/(ss + 1)
m = ss/(ss + sqrt(3))
l = ss/(ss + sqrt(5))

h2 = ss/(ss + 1)
m2 = ss/(ss + 3)
l2 = ss/(ss + 5)

c(h, m, l)
c(h2, m2, l2)

tapply(fulldata$BN1.ges, list(fulldata$levels, fulldata$stdev), mean)

sigma = matrix(c(1,0,0,0,0,0,
                   0,1,0,0,0,0,
                   0,0,1,0,0,0,
                   0,0,0,1,0,0,
                   0,0,0,0,1,0,
                   0,0,0,0,0,1), nrow = 6, ncol = 6)

dataset = rmvnorm(100, means, sigma)
apply(dataset, 2, sd)
apply(dataset, 2, mean)
