##set working directory
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Eta simulations/totals")

####import full dataset#####
fulldata = read.csv("fulldata.csv")

####calculate the various effects####
##do not need ges, already calculated
#calculate partial eta squared
fulldata$RM1.pes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main)
fulldata$RM2.pes = fulldata$RM2.ssm.main / (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main)
fulldata$MIX.pes = fulldata$MIX.ssm.main / (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main)
fulldata$BN1.pes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.pes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all)

#calculate full eta squared
fulldata$RM1.fes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main + fulldata$RM1.ssr.p)
fulldata$RM2.fes = fulldata$RM2.ssm.main / 
  (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main + fulldata$RM2.ssm.other + fulldata$RM2.ssm.interact + fulldata$RM2.ssr.p + fulldata$RM2.ssr.other + fulldata$RM2.ssr.interact)
fulldata$MIX.fes = fulldata$MIX.ssm.main / 
  (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main + fulldata$MIX.ssm.other + fulldata$MIX.ssm.interact + fulldata$MIX.ssr.p + fulldata$MIX.ssr.other + fulldata$MIX.ssr.interact)
fulldata$BN1.fes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.fes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all + fulldata$BN2.ssm.other + fulldata$BN2.ssm.interact)

#calculate full omega squared
fulldata$RM1.fos = (fulldata$RM1.dfm*((fulldata$RM1.ssm.main/fulldata$RM1.dfm)-(fulldata$RM1.ssr.main/fulldata$RM1.dfr)))/
  ((fulldata$RM1.ssm.main+fulldata$RM1.ssr.main+fulldata$RM1.ssr.p)+(fulldata$RM1.ssm.p/(fulldata$RM1.dfr/fulldata$RM1.dfm)))
fulldata$RM2.fos = (fulldata$RM2.dfm*((fulldata$RM2.ssm.main/fulldata$RM2.dfm)-(fulldata$RM2.ssr.main/fulldata$RM2.dfr)))/
  ((fulldata$RM2.ssm.main+fulldata$RM2.ssr.main+fulldata$RM2.ssm.other+fulldata$RM2.ssr.other+fulldata$RM2.ssm.interact+fulldata$RM2.ssr.interact+fulldata$RM2.ssr.p)+(fulldata$RM2.ssm.p/(fulldata$RM2.dfr/fulldata$RM2.dfm)))
fulldata$MIX.fos = (fulldata$MIX.dfm*((fulldata$MIX.ssm.main/fulldata$MIX.dfm)-(fulldata$MIX.ssr.main/fulldata$MIX.dfr)))/
  ((fulldata$MIX.ssm.main+fulldata$MIX.ssr.main+fulldata$MIX.ssm.other+fulldata$MIX.ssr.other+fulldata$MIX.ssm.interact+fulldata$MIX.ssr.interact+fulldata$MIX.ssr.p)+(fulldata$MIX.ssm.p/(fulldata$MIX.dfr/fulldata$MIX.dfm)))
fulldata$BN1.fos = (fulldata$BN1.dfm*((fulldata$BN1.ssm.main/fulldata$BN1.dfm)-(fulldata$BN1.ssr.main/fulldata$BN1.dfr)))/
  ((fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)+(fulldata$BN1.ssr.main/fulldata$BN1.dfr))
fulldata$BN2.fos = (fulldata$BN2.dfm*((fulldata$BN2.ssm.main/fulldata$BN2.dfm)-(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))/
  ((fulldata$BN2.ssm.main + fulldata$BN2.ssr.all + fulldata$BN2.ssm.other + fulldata$BN2.ssm.interact)+(fulldata$BN2.ssr.all/fulldata$BN2.dfr))
fulldata$RM1.fos[fulldata$RM1.fos<0] = 0
fulldata$RM2.fos[fulldata$RM2.fos<0] = 0
fulldata$MIX.fos[fulldata$MIX.fos<0] = 0
fulldata$BN1.fos[fulldata$BN1.fos<0] = 0
fulldata$BN2.fos[fulldata$BN2.fos<0] = 0

#calculate partial omega squared
#RM1 NA
#BN1 NA
fulldata$RM2.pos = (fulldata$RM2.dfm*((fulldata$RM2.ssm.main/fulldata$RM2.dfm)-(fulldata$RM2.ssr.main/fulldata$RM2.dfr)))/
  (fulldata$RM2.ssm.main+fulldata$RM2.ssr.main+fulldata$RM2.ssm.p+(fulldata$RM2.ssm.p/(fulldata$RM2.dfr/fulldata$RM2.dfm)))
fulldata$BN2.pos = (fulldata$BN2.dfm*((fulldata$BN2.ssm.main/fulldata$BN2.dfm)-(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))/
  (fulldata$BN2.ssm.main+(((fulldata$N*fulldata$levels)-fulldata$BN2.dfm)*(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))
fulldata$MIX.pos = (fulldata$MIX.dfm*((fulldata$MIX.ssm.main/fulldata$MIX.dfm)-(fulldata$MIX.ssr.main/fulldata$MIX.dfr)))/
  (fulldata$MIX.ssm.main+fulldata$MIX.ssr.main+fulldata$MIX.ssm.p+(fulldata$MIX.ssm.p/(fulldata$MIX.dfr/fulldata$MIX.dfm)))
fulldata$RM2.pos[fulldata$RM2.pos<0] = 0
fulldata$BN2.pos[fulldata$BN2.pos<0] = 0
fulldata$MIX.pos[fulldata$MIX.pos<0] = 0

####calculate the means of each of these combinations####
mean_data = with(fulldata, aggregate.data.frame(RM1.ges, list(N, stdev, levels, correl), mean))
temp = with(fulldata, aggregate.data.frame(RM2.ges, list(N, stdev, levels, correl), mean))
temp2 = with(fulldata, aggregate.data.frame(BN1.ges, list(N, stdev, levels, correl), mean))
temp3 = with(fulldata, aggregate.data.frame(BN2.ges, list(N, stdev, levels, correl), mean))
temp4 = with(fulldata, aggregate.data.frame(MIX.ges, list(N, stdev, levels, correl), mean))
temp5 = with(fulldata, aggregate.data.frame(RM1.pes, list(N, stdev, levels, correl), mean))
temp6 = with(fulldata, aggregate.data.frame(RM2.pes, list(N, stdev, levels, correl), mean))
temp7 = with(fulldata, aggregate.data.frame(BN1.pes, list(N, stdev, levels, correl), mean))
temp8 = with(fulldata, aggregate.data.frame(BN2.pes, list(N, stdev, levels, correl), mean))
temp9 = with(fulldata, aggregate.data.frame(MIX.pes, list(N, stdev, levels, correl), mean))
temp10 = with(fulldata, aggregate.data.frame(RM1.fes, list(N, stdev, levels, correl), mean))
temp11 = with(fulldata, aggregate.data.frame(RM2.fes, list(N, stdev, levels, correl), mean))
temp12 = with(fulldata, aggregate.data.frame(BN1.fes, list(N, stdev, levels, correl), mean))
temp13 = with(fulldata, aggregate.data.frame(BN2.fes, list(N, stdev, levels, correl), mean))
temp14 = with(fulldata, aggregate.data.frame(MIX.fes, list(N, stdev, levels, correl), mean))
temp15 = with(fulldata, aggregate.data.frame(RM1.fos, list(N, stdev, levels, correl), mean))
temp16 = with(fulldata, aggregate.data.frame(RM2.fos, list(N, stdev, levels, correl), mean))
temp17 = with(fulldata, aggregate.data.frame(BN1.fos, list(N, stdev, levels, correl), mean))
temp18 = with(fulldata, aggregate.data.frame(BN2.fos, list(N, stdev, levels, correl), mean))
temp19 = with(fulldata, aggregate.data.frame(MIX.fos, list(N, stdev, levels, correl), mean))
temp20 = with(fulldata, aggregate.data.frame(RM2.pos, list(N, stdev, levels, correl), mean))
temp21 = with(fulldata, aggregate.data.frame(BN2.pos, list(N, stdev, levels, correl), mean))
temp22 = with(fulldata, aggregate.data.frame(MIX.pos, list(N, stdev, levels, correl), mean))
colnames(mean_data) = c("N", "stdev", "levels", "correl", "RM1.ges")
mean_data = cbind(mean_data,
                  "RM2.ges" = temp$x,
                  "BN1.ges" = temp2$x, 
                  "BN2.ges" = temp3$x,
                  "MIX.ges" = temp4$x,
                  "RM1.pes" = temp5$x,
                  "RM2.pes" = temp6$x,
                  "BN1.pes" = temp7$x,
                  "BN2.pes" = temp8$x,
                  "MIX.pes" = temp9$x,
                  "RM1.fes" = temp10$x,
                  "RM2.fes" = temp11$x,
                  "BN1.fes" = temp12$x,
                  "BN2.fes" = temp13$x,
                  "MIX.fes" = temp14$x,
                  "RM1.fos" = temp15$x,
                  "RM2.fos" = temp16$x,
                  "BN1.fos" = temp17$x,
                  "BN2.fos" = temp18$x,
                  "MIX.fos" = temp19$x,
                  "RM2.pos" = temp20$x,
                  "BN2.pos" = temp21$x,
                  "MIX.pos" = temp22$x
)


####calculate the variance of each of the combinations####
stdev_data = with(fulldata, aggregate.data.frame(RM1.ges, list(N, stdev, levels, correl), var))
temp = with(fulldata, aggregate.data.frame(RM2.ges, list(N, stdev, levels, correl), var))
temp2 = with(fulldata, aggregate.data.frame(BN1.ges, list(N, stdev, levels, correl), var))
temp3 = with(fulldata, aggregate.data.frame(BN2.ges, list(N, stdev, levels, correl), var))
temp4 = with(fulldata, aggregate.data.frame(MIX.ges, list(N, stdev, levels, correl), var))
temp5 = with(fulldata, aggregate.data.frame(RM1.pes, list(N, stdev, levels, correl), var))
temp6 = with(fulldata, aggregate.data.frame(RM2.pes, list(N, stdev, levels, correl), var))
temp7 = with(fulldata, aggregate.data.frame(BN1.pes, list(N, stdev, levels, correl), var))
temp8 = with(fulldata, aggregate.data.frame(BN2.pes, list(N, stdev, levels, correl), var))
temp9 = with(fulldata, aggregate.data.frame(MIX.pes, list(N, stdev, levels, correl), var))
temp10 = with(fulldata, aggregate.data.frame(RM1.fes, list(N, stdev, levels, correl), var))
temp11 = with(fulldata, aggregate.data.frame(RM2.fes, list(N, stdev, levels, correl), var))
temp12 = with(fulldata, aggregate.data.frame(BN1.fes, list(N, stdev, levels, correl), var))
temp13 = with(fulldata, aggregate.data.frame(BN2.fes, list(N, stdev, levels, correl), var))
temp14 = with(fulldata, aggregate.data.frame(MIX.fes, list(N, stdev, levels, correl), var))
temp15 = with(fulldata, aggregate.data.frame(RM1.fos, list(N, stdev, levels, correl), var))
temp16 = with(fulldata, aggregate.data.frame(RM2.fos, list(N, stdev, levels, correl), var))
temp17 = with(fulldata, aggregate.data.frame(BN1.fos, list(N, stdev, levels, correl), var))
temp18 = with(fulldata, aggregate.data.frame(BN2.fos, list(N, stdev, levels, correl), var))
temp19 = with(fulldata, aggregate.data.frame(MIX.fos, list(N, stdev, levels, correl), var))
temp20 = with(fulldata, aggregate.data.frame(RM2.pos, list(N, stdev, levels, correl), var))
temp21 = with(fulldata, aggregate.data.frame(BN2.pos, list(N, stdev, levels, correl), var))
temp22 = with(fulldata, aggregate.data.frame(MIX.pos, list(N, stdev, levels, correl), var))
colnames(stdev_data) = c("N", "stdev", "levels", "correl", "RM1.ges")
stdev_data = cbind(stdev_data,
                   "RM2.ges" = temp$x,
                   "BN1.ges" = temp2$x, 
                   "BN2.ges" = temp3$x,
                   "MIX.ges" = temp4$x,
                   "RM1.pes" = temp5$x,
                   "RM2.pes" = temp6$x,
                   "BN1.pes" = temp7$x,
                   "BN2.pes" = temp8$x,
                   "MIX.pes" = temp9$x,
                   "RM1.fes" = temp10$x,
                   "RM2.fes" = temp11$x,
                   "BN1.fes" = temp12$x,
                   "BN2.fes" = temp13$x,
                   "MIX.fes" = temp14$x,
                   "RM1.fos" = temp15$x,
                   "RM2.fos" = temp16$x,
                   "BN1.fos" = temp17$x,
                   "BN2.fos" = temp18$x,
                   "MIX.fos" = temp19$x,
                   "RM2.pos" = temp20$x,
                   "BN2.pos" = temp21$x,
                   "MIX.pos" = temp22$x
)
remove(temp)
remove(temp2)
remove(temp3)
remove(temp4)
remove(temp5)
remove(temp6)
remove(temp7)
remove(temp8)
remove(temp9)
remove(temp10)
remove(temp11)
remove(temp12)
remove(temp13)
remove(temp14)
remove(temp15)
remove(temp16)
remove(temp17)
remove(temp18)
remove(temp19)
remove(temp20)
remove(temp21)
remove(temp22)

####calculate the variance for F####
##calculate the df
stdev_data$df1 = stdev_data$levels - 1
stdev_data$RM.df2 = (stdev_data$N - 1)*(stdev_data$levels - 1)

##use that for var
var.f = function (DF1, DF2) { (2*DF2^2*(DF1 + DF2 - 2)) / (DF1*(DF2-2)^2*(DF2 - 4))}
var.b = function (DF1, DF2) {
  alpha = DF1/2
  beta = DF2/2
  alpha*beta / ((alpha+beta)^2)*(alpha+beta+1)
}
var.b2 = function (DF1, DF2) {
  alpha = DF1
  beta = DF2
  alpha*beta / ((alpha+beta)^2)*(alpha+beta+1)
}

var.d = function (n, eta, groups) {
  df = (groups - 1)*(n - 1)
  r = sqrt(eta)
  d = 2*r / sqrt(1 - r^2)
  c = 3 / 4*df -  1 
  variance = (1/n)*((n-1)/(n-3))*(1+n*d^2) - ((d^2)/(c*(n-1))^2)
  newr = sqrt(variance^2 / (4+variance^2))
  newr^2
  
}

stdev_data$RM.var.f = var.f(stdev_data$df1, stdev_data$RM.df2)
stdev_data$RM.var.b = var.b(stdev_data$df1, stdev_data$RM.df2)
stdev_data$RM.var.b2 = var.b2(fes_data$beta.1.shape1, fes_data$beta.1.shape2)
stdev_data$RM.var.d = var.d(mean_data$N, mean_data$RM1.fes, mean_data$levels)

