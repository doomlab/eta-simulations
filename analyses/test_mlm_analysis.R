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


####means####
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

####stdevs####
stdev_data = with(fulldata, aggregate.data.frame(RM1.ges, list(N, stdev, levels, correl), sd))
temp = with(fulldata, aggregate.data.frame(RM2.ges, list(N, stdev, levels, correl), sd))
temp2 = with(fulldata, aggregate.data.frame(BN1.ges, list(N, stdev, levels, correl), sd))
temp3 = with(fulldata, aggregate.data.frame(BN2.ges, list(N, stdev, levels, correl), sd))
temp4 = with(fulldata, aggregate.data.frame(MIX.ges, list(N, stdev, levels, correl), sd))
temp5 = with(fulldata, aggregate.data.frame(RM1.pes, list(N, stdev, levels, correl), sd))
temp6 = with(fulldata, aggregate.data.frame(RM2.pes, list(N, stdev, levels, correl), sd))
temp7 = with(fulldata, aggregate.data.frame(BN1.pes, list(N, stdev, levels, correl), sd))
temp8 = with(fulldata, aggregate.data.frame(BN2.pes, list(N, stdev, levels, correl), sd))
temp9 = with(fulldata, aggregate.data.frame(MIX.pes, list(N, stdev, levels, correl), sd))
temp10 = with(fulldata, aggregate.data.frame(RM1.fes, list(N, stdev, levels, correl), sd))
temp11 = with(fulldata, aggregate.data.frame(RM2.fes, list(N, stdev, levels, correl), sd))
temp12 = with(fulldata, aggregate.data.frame(BN1.fes, list(N, stdev, levels, correl), sd))
temp13 = with(fulldata, aggregate.data.frame(BN2.fes, list(N, stdev, levels, correl), sd))
temp14 = with(fulldata, aggregate.data.frame(MIX.fes, list(N, stdev, levels, correl), sd))
temp15 = with(fulldata, aggregate.data.frame(RM1.fos, list(N, stdev, levels, correl), sd))
temp16 = with(fulldata, aggregate.data.frame(RM2.fos, list(N, stdev, levels, correl), sd))
temp17 = with(fulldata, aggregate.data.frame(BN1.fos, list(N, stdev, levels, correl), sd))
temp18 = with(fulldata, aggregate.data.frame(BN2.fos, list(N, stdev, levels, correl), sd))
temp19 = with(fulldata, aggregate.data.frame(MIX.fos, list(N, stdev, levels, correl), sd))
temp20 = with(fulldata, aggregate.data.frame(RM2.pos, list(N, stdev, levels, correl), sd))
temp21 = with(fulldata, aggregate.data.frame(BN2.pos, list(N, stdev, levels, correl), sd))
temp22 = with(fulldata, aggregate.data.frame(MIX.pos, list(N, stdev, levels, correl), sd))
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

####run with the mlm####
library(nlme)
library(reshape)
mean_data$partno = 1:nrow(mean_data)

longmean = melt(mean_data, 
                id = c("partno", "N", "stdev", "levels", "correl"), 
                measured = c("RM1.ges","RM2.ges","BN1.ges","BN2.ges","MIX.ges",
                             "RM1.pes","RM2.pes","BN1.pes","BN2.pes","MIX.pes",
                             "RM1.fes","RM2.fes","BN1.fes","BN2.fes","MIX.fes",
                             "RM1.fos","RM2.fos","BN1.fos","BN2.fos","MIX.fos",
                             "RM2.pos","BN2.pos","MIX.pos"))
longmean$analysistype = c(rep("RM1", nrow(mean_data)), 
                          rep("RM2", nrow(mean_data)),
                          rep("BN1", nrow(mean_data)),
                          rep("BN2", nrow(mean_data)),
                          rep("MIX", nrow(mean_data)),
                          rep("RM1", nrow(mean_data)), 
                          rep("RM2", nrow(mean_data)),
                          rep("BN1", nrow(mean_data)),
                          rep("BN2", nrow(mean_data)),
                          rep("MIX", nrow(mean_data)),
                          rep("RM1", nrow(mean_data)), 
                          rep("RM2", nrow(mean_data)),
                          rep("BN1", nrow(mean_data)),
                          rep("BN2", nrow(mean_data)),
                          rep("MIX", nrow(mean_data)),
                          rep("RM1", nrow(mean_data)), 
                          rep("RM2", nrow(mean_data)),
                          rep("BN1", nrow(mean_data)),
                          rep("BN2", nrow(mean_data)),
                          rep("MIX", nrow(mean_data)),
                          rep("RM2", nrow(mean_data)), 
                          rep("BN2", nrow(mean_data)),
                          rep("MIX", nrow(mean_data)))

longmean$effecttype = c(rep("ges", 5*nrow(mean_data)), 
                        rep("pes", 5*nrow(mean_data)),
                        rep("fes", 5*nrow(mean_data)),
                        rep("fos", 5*nrow(mean_data)),
                        rep("pos", 3*nrow(mean_data)))                          

options(scipen = 999)

model1 = gls(value ~ 1, 
             data = longmean, 
             method = "ML", 
             na.action = "na.omit")
summary(model1)

model2 = lme(value ~ 1, 
             data = longmean, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model2)
anova(model1,model2)

model3 = lme(value ~ N+stdev+levels+correl+analysistype+effecttype, 
             data = longmean, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model3)

model4 = lme(value ~ N*stdev*levels*correl*effecttype, 
             data = longmean, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model4)$tTable
