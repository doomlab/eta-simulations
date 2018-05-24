
#fulldata = read.csv("fulldata.csv")
fulldata <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Project/data/fulldata.csv")


####################################################################### calculate effect sizes
fulldata$RM1.ges = fulldata$RM1.ges * 100
fulldata$RM2.ges = fulldata$RM2.ges * 100
fulldata$MIX.ges = fulldata$MIX.ges * 100
fulldata$BN1.ges = fulldata$BN1.ges * 100
fulldata$BN2.ges = fulldata$BN2.ges * 100

#calculate partial eta squared
fulldata$RM1.pes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main)
fulldata$RM2.pes = fulldata$RM2.ssm.main / (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main)
fulldata$MIX.pes = fulldata$MIX.ssm.main / (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main)
fulldata$BN1.pes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.pes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all)
fulldata$RM1.pes = fulldata$RM1.pes * 100
fulldata$RM2.pes = fulldata$RM2.pes * 100
fulldata$MIX.pes = fulldata$MIX.pes * 100
fulldata$BN1.pes = fulldata$BN1.pes * 100
fulldata$BN2.pes = fulldata$BN2.pes * 100

#calculate full eta squared
fulldata$RM1.fes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main + fulldata$RM1.ssr.p)
fulldata$RM2.fes = fulldata$RM2.ssm.main / 
  (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main + fulldata$RM2.ssm.other + fulldata$RM2.ssm.interact + fulldata$RM2.ssr.p + fulldata$RM2.ssr.other + fulldata$RM2.ssr.interact)
fulldata$MIX.fes = fulldata$MIX.ssm.main / 
  (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main + fulldata$MIX.ssm.other + fulldata$MIX.ssm.interact + fulldata$MIX.ssr.p + fulldata$MIX.ssr.other + fulldata$MIX.ssr.interact)
fulldata$BN1.fes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.fes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all + fulldata$BN2.ssm.other + fulldata$BN2.ssm.interact)
fulldata$RM1.fes = fulldata$RM1.fes * 100
fulldata$RM2.fes = fulldata$RM2.fes * 100
fulldata$MIX.fes = fulldata$MIX.fes * 100
fulldata$BN1.fes = fulldata$BN1.fes * 100
fulldata$BN2.fes = fulldata$BN2.fes * 100

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
fulldata$RM1.fos = fulldata$RM1.fos * 100
fulldata$RM2.fos = fulldata$RM2.fos * 100
fulldata$MIX.fos = fulldata$MIX.fos * 100
fulldata$BN1.fos = fulldata$BN1.fos * 100
fulldata$BN2.fos = fulldata$BN2.fos * 100
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
fulldata$RM2.pos = fulldata$RM2.pos * 100
fulldata$BN2.pos = fulldata$BN2.pos * 100
fulldata$MIX.pos = fulldata$MIX.pos * 100
fulldata$RM2.pos[fulldata$RM2.pos<0] = 0
fulldata$BN2.pos[fulldata$BN2.pos<0] = 0
fulldata$MIX.pos[fulldata$MIX.pos<0] = 0
####################################################################### calculate effect sizes


################################################################################### create means
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
################################################################################### create means


################################################################################### create stdevs
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

options(scipen = 999)
################################################################################### create stdevs



################################################ average together across design type and effect size type
mean_data$average = "NA"
nsim = nrow(mean_data)
round = 0
for(i in 1:nsim){
  round = round+1
  mean_data$average[round] = mean(mean_data[round,5]:mean_data[round,27])
}

stdev_data$average = "NA"
nsim = nrow(stdev_data)
round = 0
for(i in 1:nsim){
  round = round+1
  stdev_data$average[round] = mean(stdev_data[round,5]:mean(stdev_data[round,27]))
}
################################################ average together across design type and effect size type




############################################################################## testing differences in means
mean_model = lm(average ~ N*levels*correl*stdev, data = mean_data)
summary(mean_model)

#split by stdev
SDfive_mean = subset(mean_data, stdev == 5)
SDthree_mean = subset(mean_data, stdev == 3)
SDone_mean = subset(mean_data, stdev == 1)

mean_model_sdfive = lm(average ~ N*levels*correl, data = SDfive_mean)
summary(mean_model_sdfive)

mean_model_sdthree = lm(average ~ N*levels*correl, data = SDthree_mean)
summary(mean_model_sdthree)

mean_model_sdone = lm(average ~ N*levels*correl, data = SDone_mean)
summary(mean_model_sdone)


#SD 5 n:levels interaction
SDfive_mean$levelslo = SDfive_mean$levels + sd(SDfive_mean$levels)
SDfive_mean$levelshi = SDfive_mean$levels - sd(SDfive_mean$levels)
mean_model_sdfive_levelslo = lm(average ~ N*levelslo, data = SDfive_mean)
summary(mean_model_sdfive_levelslo)
mean_model_sdfive_levelshi = lm(average ~ N*levelshi, data = SDfive_mean)
summary(mean_model_sdfive_levelshi)

#SD 3 no interactions

#SD 1 split by correlation
SDone_mean$corrello = SDone_mean$correl + sd(SDone_mean$correl)
SDone_mean$correlhi = SDone_mean$correl - sd(SDone_mean$correl)

mean_model_sdone_corrello = lm(average ~ N*levels*corrello, data = SDone_mean)
summary(mean_model_sdone_corrello)
mean_model_sdone_correlhi = lm(average ~ N*levels*correlhi, data = SDone_mean)
summary(mean_model_sdone_correlhi)

#SD 1 split by correlation and levels
SDone_mean$levelslo = SDone_mean$levels + sd(SDone_mean$levels)
SDone_mean$levelshi = SDone_mean$levels - sd(SDone_mean$levels)

mean_model_sdone_corrello_levelslo = lm(average ~ N*levelslo*corrello, data = SDone_mean)
summary(mean_model_sdone_corrello_levelslo)
mean_model_sdone_corrello_levelshi = lm(average ~ N*levelshi*corrello, data = SDone_mean)
summary(mean_model_sdone_corrello_levelshi)

mean_model_sdone_correlhi_levelslo = lm(average ~ N*levelslo*correlhi, data = SDone_mean)
summary(mean_model_sdone_correlhi_levelslo)
mean_model_sdone_correlhi_levelshi = lm(average ~ N*levelshi*correlhi, data = SDone_mean)
summary(mean_model_sdone_correlhi_levelshi)
############################################################################## testing differences in means


############################################################################## testing differences in stdev
stdev_model = lm(average ~ N*levels*correl*stdev, data = stdev_data)
summary(stdev_model)

#split by SD
SDfive_stdev = subset(stdev_data, stdev == 5)
SDthree_stdev = subset(stdev_data, stdev == 3)
SDone_stdev = subset(stdev_data, stdev == 1)

stdev_model_sdfive = lm(average ~ N*levels*correl, data = SDfive_stdev)
summary(stdev_model_sdfive)

stdev_model_sdthree = lm(average ~ N*levels*correl, data = SDthree_stdev)
summary(stdev_model_sdthree)

stdev_model_sdone = lm(average ~ N*levels*correl, data = SDone_stdev)
summary(stdev_model_sdone)


#SD 1 split by correlation
SDone_stdev$corrello = SDone_stdev$correl + sd(SDone_stdev$correl)
SDone_stdev$correlhi = SDone_stdev$correl - sd(SDone_stdev$correl)

stdev_model_sdone_corrello = lm(average ~ N*levels*corrello, data = SDone_stdev)
summary(stdev_model_sdone_corrello)
stdev_model_sdone_correlhi = lm(average ~ N*levels*correlhi, data = SDone_stdev)
summary(stdev_model_sdone_correlhi)


#SD 1 split by correlation and levels
SDone_stdev$levelslo = SDone_stdev$levels + sd(SDone_stdev$levels)
SDone_stdev$levelshi = SDone_stdev$levels - sd(SDone_stdev$levels)

stdev_model_sdone_corrello_levelslo = lm(average ~ N*levelslo*corrello, data = SDone_stdev)
summary(stdev_model_sdone_corrello_levelslo)
stdev_model_sdone_corrello_levelshi = lm(average ~ N*levelshi*corrello, data = SDone_stdev)
summary(stdev_model_sdone_corrello_levelshi)

stdev_model_sdone_correlhi_levelslo = lm(average ~ N*levelslo*correlhi, data = SDone_stdev)
summary(stdev_model_sdone_correlhi_levelslo)
stdev_model_sdone_correlhi_levelshi = lm(average ~ N*levelshi*correlhi, data = SDone_stdev)
summary(stdev_model_sdone_correlhi_levelshi)
############################################################################## testing differences in stdev


############################################## Mean Tables
#Overall
summary = summary(mean_model)
mean_model$coefficients
Predictor = names(mean_model$coefficients)
Estimate = unname(mean_model$coefficients)
tvalue = unname(summary$coefficients[c(1:16),3])
pvalue = unname(summary$coefficients[c(1:16),4])
overall_mean_table = as.data.frame(cbind(Predictor,Estimate,tvalue,pvalue))

library(stringr)
nsim = nrow(overall_mean_table)
round = 0
y = 1
y = y[-1]
overall_mean_table$Estimate = as.numeric(as.character(overall_mean_table$Estimate))
overall_mean_table$tvalue = as.numeric(as.character(overall_mean_table$tvalue))
overall_mean_table$pvalue = as.numeric(as.character(overall_mean_table$pvalue))
for(i in 1:nsim){
  round = round+1
  
  overall_mean_table[round,2] = round(overall_mean_table[round,2], digits = 2)
  overall_mean_table[round,3] = round(overall_mean_table[round,3], digits = 2)
  
  x = overall_mean_table[round,4]
  if(x < .001){
    x = "< .001"
  } else if(x < .01 && x > .001){
    x = "< .01"
  } else {
    x = round(x, digits = 2)
    x = as.character(x)
    x = str_replace(as.character(x), "^0\\.", ".")
  }
  y = append(y,x)
}
overall_mean_table$pvalue = y
overall_mean_table = subset(overall_mean_table, Predictor != "(Intercept)")

#SD5
summary = summary(mean_model_sdfive)
Predictor = names(mean_model_sdfive$coefficients)
Estimate = unname(mean_model_sdfive$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(5,8)
Correlation = rep("-",8)
Levels = rep("-",8)
temp1 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

#SD5 Split by Levels
summary1 = summary(mean_model_sdfive_levelslo)
Predictor = names(mean_model_sdfive_levelslo$coefficients)
Estimate = unname(mean_model_sdfive_levelslo$coefficients)
tvalue = unname(summary1$coefficients[c(1:4),3])
pvalue = unname(summary1$coefficients[c(1:4),4])
SD = rep(5,4)
Correlation = rep("-",4)
Levels = rep("Low",4)
temp2 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

summary2 = summary(mean_model_sdfive_levelshi)
Predictor = names(mean_model_sdfive_levelshi$coefficients)
Estimate = unname(mean_model_sdfive_levelshi$coefficients)
tvalue = unname(summary2$coefficients[c(1:4),3])
pvalue = unname(summary2$coefficients[c(1:4),4])
SD = rep(5,4)
Correlation = rep("-",4)
Levels = rep("High",4)
temp3 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

#SD3
summary = summary(mean_model_sdthree)
Predictor = names(mean_model_sdthree$coefficients)
Estimate = unname(mean_model_sdthree$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(3,8)
Correlation = rep("-",8)
Levels = rep("-",8)
temp4 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

#SD1
summary = summary(mean_model_sdone)
Predictor = names(mean_model_sdone$coefficients)
Estimate = unname(mean_model_sdone$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("-",8)
Levels = rep("-",8)
temp5 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))


#SD1 split by correlation and levels
summary = summary(mean_model_sdone_corrello_levelslo)
Predictor = names(mean_model_sdone_corrello_levelslo$coefficients)
Estimate = unname(mean_model_sdone_corrello_levelslo$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("Low",8)
Levels = rep("Low",8)
temp6 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

summary = summary(mean_model_sdone_corrello_levelshi)
Predictor = names(mean_model_sdone_corrello_levelshi$coefficients)
Estimate = unname(mean_model_sdone_corrello_levelshi$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("Low",8)
Levels = rep("High",8)
temp7 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

summary = summary(mean_model_sdone_correlhi_levelslo)
Predictor = names(mean_model_sdone_correlhi_levelslo$coefficients)
Estimate = unname(mean_model_sdone_correlhi_levelslo$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("High",8)
Levels = rep("Low",8)
temp8 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

summary = summary(mean_model_sdone_correlhi_levelshi)
Predictor = names(mean_model_sdone_correlhi_levelshi$coefficients)
Estimate = unname(mean_model_sdone_correlhi_levelshi$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("High",8)
Levels = rep("High",8)
temp9 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))


#Combine all splits together
interaction_mean_table = rbind(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9)
nsim = nrow(interaction_mean_table)
round = 0
y = 1
y = y[-1]
interaction_mean_table$Estimate = as.numeric(as.character(interaction_mean_table$Estimate))
interaction_mean_table$tvalue = as.numeric(as.character(interaction_mean_table$tvalue))
interaction_mean_table$pvalue = as.numeric(as.character(interaction_mean_table$pvalue))
for(i in 1:nsim){
  round = round+1
  
  interaction_mean_table[round,5] = round(interaction_mean_table[round,5], digits = 2)
  interaction_mean_table[round,6] = round(interaction_mean_table[round,6], digits = 2)
  
  x = interaction_mean_table[round,7]
  if(x < .001){
    x = "< .001"
  } else if(x < .01 && x > .001){
    x = "< .01"
  } else {
    x = round(x, digits = 2)
    x = as.character(x)
    x = str_replace(as.character(x), "^0\\.", ".")
  }
  y = append(y,x)
}
interaction_mean_table$pvalue = y
interaction_mean_table = subset(interaction_mean_table, Predictor != "(Intercept)")
############################################## Mean Tables



############################################# Stdev Tables
#Overall
summary = summary(stdev_model)
mean_model$coefficients
Predictor = names(stdev_model$coefficients)
Estimate = unname(stdev_model$coefficients)
tvalue = unname(summary$coefficients[c(1:16),3])
pvalue = unname(summary$coefficients[c(1:16),4])
overall_sd_table = as.data.frame(cbind(Predictor,Estimate,tvalue,pvalue))

nsim = nrow(overall_sd_table)
round = 0
y = 1
y = y[-1]
overall_sd_table$Estimate = as.numeric(as.character(overall_sd_table$Estimate))
overall_sd_table$tvalue = as.numeric(as.character(overall_sd_table$tvalue))
overall_sd_table$pvalue = as.numeric(as.character(overall_sd_table$pvalue))
for(i in 1:nsim){
  round = round+1
  
  overall_sd_table[round,2] = round(overall_sd_table[round,2], digits = 2)
  overall_sd_table[round,3] = round(overall_sd_table[round,3], digits = 2)
  
  x = overall_sd_table[round,4]
  if(x < .001){
    x = "< .001"
  } else if(x < .01 && x > .001){
    x = "< .01"
  } else {
    x = round(x, digits = 2)
    x = as.character(x)
    x = str_replace(as.character(x), "^0\\.", ".")
  }
  y = append(y,x)
}
overall_sd_table$pvalue = y
overall_sd_table = subset(overall_sd_table, Predictor != "(Intercept)")

#SD5
summary = summary(stdev_model_sdfive)
Predictor = names(stdev_model_sdfive$coefficients)
Estimate = unname(stdev_model_sdfive$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(5,8)
Correlation = rep("-",8)
Levels = rep("-",8)
temp1 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

#SD3
summary = summary(stdev_model_sdthree)
Predictor = names(stdev_model_sdthree$coefficients)
Estimate = unname(stdev_model_sdthree$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(3,8)
Correlation = rep("-",8)
Levels = rep("-",8)
temp2 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

#SD1
summary = summary(stdev_model_sdone)
Predictor = names(stdev_model_sdone$coefficients)
Estimate = unname(stdev_model_sdone$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("-",8)
Levels = rep("-",8)
temp3 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

summary = summary(stdev_model_sdone_corrello_levelslo)
Predictor = names(stdev_model_sdone_corrello_levelslo$coefficients)
Estimate = unname(stdev_model_sdone_corrello_levelslo$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("Low",8)
Levels = rep("Low",8)
temp4 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

summary = summary(stdev_model_sdone_corrello_levelshi)
Predictor = names(stdev_model_sdone_corrello_levelshi$coefficients)
Estimate = unnames(stdev_model_sdone_corrello_levelshi$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("Low",8)
Levels = rep("High",8)
temp5 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

summary = summary(stdev_model_sdone_correlhi_levelslo)
Predictor = names(stdev_model_sdone_correlhi_levelslo$coefficients)
Estimate = unnames(stdev_model_sdone_correlhi_levelslo$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("High",8)
Levels = rep("Low",8)
temp6 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

summary = summary(stdev_model_sdone_correlhi_levelshi)
Predictor = names(stdev_model_sdone_correlhi_levelshi$coefficients)
Estimate = unname(stdev_model_sdone_correlhi_levelshi$coefficients)
tvalue = unname(summary$coefficients[c(1:8),3])
pvalue = unname(summary$coefficients[c(1:8),4])
SD = rep(1,8)
Correlation = rep("High",8)
Levels = rep("High",8)
temp7 = as.data.frame(cbind(Predictor,SD,Levels,Correlation,Estimate,tvalue,pvalue))

#Combine all together
interaction_sd_table = rbind(temp1,temp2,temp3,temp4,temp5,temp6,temp7)
nsim = nrow(interaction_sd_table)
round = 0
y = 1
y = y[-1]
interaction_sd_table$Estimate = as.numeric(as.character(interaction_sd_table$Estimate))
interaction_sd_table$tvalue = as.numeric(as.character(interaction_sd_table$tvalue))
interaction_sd_table$pvalue = as.numeric(as.character(interaction_sd_table$pvalue))
for(i in 1:nsim){
  round = round+1
  
  interaction_sd_table[round,5] = round(interaction_sd_table[round,5], digits = 2)
  interaction_sd_table[round,6] = round(interaction_sd_table[round,6], digits = 2)
  
  x = interaction_sd_table[round,7]
  if(x < .001){
    x = "< .001"
  } else if(x < .01 && x > .001){
    x = "< .01"
  } else {
    x = round(x, digits = 2)
    x = as.character(x)
    x = str_replace(as.character(x), "^0\\.", ".")
  }
  y = append(y,x)
}
interaction_sd_table$pvalue = y
interaction_sd_table = subset(interaction_sd_table, Predictor != "(Intercept)")


View(overall_mean_table)
View(interaction_mean_table)
View(overall_sd_table)
View(interaction_sd_table)
