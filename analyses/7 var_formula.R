####import full dataset#####
fulldata = read.csv("fulldata.csv")

estimate = fulldata[ sample(nrow(fulldata), 1000), ]
with(estimate, plot(BN1.ges, stdev))

##linear model
linear.model = lm(stdev ~ BN1.ges, data = estimate)
summary(linear.model)
abline(a = linear.model$coefficients[1], b = linear.model$coefficients[2], col = "green")
##nonlinear
non.model = nls(formula = (stdev ~ a + BN1.ges^b), 
                data = estimate, 
                start = c(a = 4, b = -.8))
summary(non.model)
a = coef(non.model)[1]
b = coef(non.model)[2]
curve(a + x^b, from = 0, 
      to = 1, add = T, col = "blue")

##other estimation
other.model = lm(log(stdev) ~ log(BN1.ges), data = estimate)
summary(other.model) 
a = other.model$coefficients[1]
b = other.model$coefficients[2]
curve(a + b*log(x), from = 0, 
      to = 1, add = T, col = "purple")

##other estimation
other.model2 = lm(stdev ~ exp(BN1.ges), data = estimate)
summary(other.model2) 
a = other.model2$coefficients[1]
b = other.model2$coefficients[2]
curve(a + b*exp(x), from = 0, 
      to = 1, add = T, col = "red")

##test going backwards
varlinear = function(x){ linear.model$coefficients[1] + linear.model$coefficients[2]*x }
varnon = function(x){ exp(other.model$coefficients[1] + other.model$coefficients[2]*log(x)) }
varlinear(.25)
varlinear(.15)
varlinear(.08)

varnon(.25)
varnon(.15)
varnon(.08)
