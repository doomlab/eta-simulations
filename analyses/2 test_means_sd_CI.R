##working directory
#setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Eta simulations/totals")

#fulldata = read.csv("fulldata.csv")
fulldata <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Project/data/fulldata.csv")



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

options(scipen = 999)


##loop over effect size 
SDloop = c(5,3,1)

##loop over design type by effect size type 
designloop = c("RM1.ges", "RM2.ges", "BN1.ges",
               "BN2.ges", "MIX.ges", "RM1.pes", 
               "RM2.pes", "BN1.pes", "BN2.pes", 
               "MIX.pes","RM1.fes", "RM2.fes", 
               "BN1.fes", "BN2.fes", "MIX.fes", 
               "RM1.fos", "RM2.fos", "BN1.fos", 
               "BN2.fos", "MIX.fos", "RM2.pos", 
               "BN2.pos", "MIX.pos")

####what are the differences in means/sds####
allmean = matrix(NA, length(designloop)*length(SDloop), 18)
allsd = matrix(NA, length(designloop)*length(SDloop), 18)
colnames(allmean) = c("Interceptcoef","Ncoef","levelscoef","correlcoef",
                      "N_levelscoef","N_correlcoef","levels_correlcoef","N_levels_correlcoef",
                      "Interceptse","Nse","levelsse","correlse",
                      "N_levelsse","N_correlse","levels_correlse","N_levels_correlse",
                      "DV", "effectsize")
colnames(allsd) = c("Interceptcoef","Ncoef","levelscoef","correlcoef",
                    "N_levelscoef","N_correlcoef","levels_correlcoef","N_levels_correlcoef",
                    "Interceptse","Nse","levelsse","correlse",
                    "N_levelsse","N_correlse","levels_correlse","N_levels_correlse",
                    "DV", "effectsize")



round = 1

for (i in 1:length(SDloop)) { ##start SD loop
  
  mean_round = subset(mean_data, stdev == SDloop[i])
  sd_round = subset(stdev_data, stdev == SDloop[i])
  
  for (ii in 1:length(designloop)) { ##start designs loop
    
    outputmean = lm(as.formula(paste(designloop[ii], "~ N*levels*correl")), data = mean_round)
    outputsd = lm(as.formula(paste(designloop[ii], "~ N*levels*correl")), data = sd_round)
    
    ##pull out b, se, p
    allmean[round, 1:8] = coef(summary(outputmean))[, 1]
    allmean[round, 9:16] = coef(summary(outputmean))[, 2]
    allmean[round, 17] = designloop[ii]
    allmean[round, 18] = SDloop[i]
    
    allsd[round, 1:8] = coef(summary(outputsd))[, 1]
    allsd[round, 9:16] = coef(summary(outputsd))[, 2]
    allsd[round, 17] = designloop[ii]
    allsd[round, 18] = SDloop[i]
    
    round = round + 1
    
  } ## end DV loop
  
} ## end SD loop





##Add CI's
allmean = as.data.frame(allmean)
allmean[ ,1] = as.numeric(as.character(allmean[ ,1]))
allmean[ ,2] = as.numeric(as.character(allmean[ ,2]))
allmean[ ,3] = as.numeric(as.character(allmean[ ,3]))
allmean[ ,4] = as.numeric(as.character(allmean[ ,4]))
allmean[ ,5] = as.numeric(as.character(allmean[ ,5]))
allmean[ ,6] = as.numeric(as.character(allmean[ ,6]))
allmean[ ,7] = as.numeric(as.character(allmean[ ,7]))
allmean[ ,8] = as.numeric(as.character(allmean[ ,8]))
allmean[ ,9] = as.numeric(as.character(allmean[ ,9]))
allmean[ ,10] = as.numeric(as.character(allmean[ ,10]))
allmean[ ,11] = as.numeric(as.character(allmean[ ,11]))
allmean[ ,12] = as.numeric(as.character(allmean[ ,12]))
allmean[ ,13] = as.numeric(as.character(allmean[ ,13]))
allmean[ ,14] = as.numeric(as.character(allmean[ ,14]))
allmean[ ,15] = as.numeric(as.character(allmean[ ,15]))
allmean[ ,16] = as.numeric(as.character(allmean[ ,16]))

allmean$Ncilo = allmean$Ncoef - (allmean$Nse*2)
allmean$Ncihi = allmean$Ncoef + (allmean$Nse*2)
allmean$levelscilo = allmean$levelscoef - (allmean$levelsse*2)
allmean$levelscihi = allmean$levelscoef + (allmean$levelsse*2)
allmean$levelscilo = allmean$correlcoef - (allmean$correlse*2)
allmean$levelscihi = allmean$correlcoef + (allmean$correlse*2)
allmean$N_levelscilo = allmean$N_levelscoef - (allmean$N_levelsse*2)
allmean$N_levelscihi = allmean$N_levelscoef + (allmean$N_levelsse*2)
allmean$N_correlcilo = allmean$N_correlcoef - (allmean$N_correlse*2)
allmean$N_correlcihi = allmean$N_correlcoef + (allmean$N_correlse*2)
allmean$levels_correlcilo = allmean$levels_correlcoef - (allmean$levels_correlse*2)
allmean$levels_correlcihi = allmean$levels_correlcoef + (allmean$levels_correlse*2)
allmean$N_levels_correlcilo = allmean$N_levels_correlcoef - (allmean$N_levels_correlse*2)
allmean$N_levels_correlcihi = allmean$N_levels_correlcoef + (allmean$N_levels_correlse*2)


allsd = as.data.frame(allsd)
allsd[ ,1] = as.numeric(as.character(allsd[ ,1]))
allsd[ ,2] = as.numeric(as.character(allsd[ ,2]))
allsd[ ,3] = as.numeric(as.character(allsd[ ,3]))
allsd[ ,4] = as.numeric(as.character(allsd[ ,4]))
allsd[ ,5] = as.numeric(as.character(allsd[ ,5]))
allsd[ ,6] = as.numeric(as.character(allsd[ ,6]))
allsd[ ,7] = as.numeric(as.character(allsd[ ,7]))
allsd[ ,8] = as.numeric(as.character(allsd[ ,8]))
allsd[ ,9] = as.numeric(as.character(allsd[ ,9]))
allsd[ ,10] = as.numeric(as.character(allsd[ ,10]))
allsd[ ,11] = as.numeric(as.character(allsd[ ,11]))
allsd[ ,12] = as.numeric(as.character(allsd[ ,12]))
allsd[ ,13] = as.numeric(as.character(allsd[ ,13]))
allsd[ ,14] = as.numeric(as.character(allsd[ ,14]))
allsd[ ,15] = as.numeric(as.character(allsd[ ,15]))
allsd[ ,16] = as.numeric(as.character(allsd[ ,16]))

allsd$Ncilo = allsd$Ncoef - (allsd$Nse*2)
allsd$Ncihi = allsd$Ncoef + (allsd$Nse*2)
allsd$levelscilo = allsd$levelscoef - (allsd$levelsse*2)
allsd$levelscihi = allsd$levelscoef + (allsd$levelsse*2)
allsd$levelscilo = allsd$correlcoef - (allsd$correlse*2)
allsd$levelscihi = allsd$correlcoef + (allsd$correlse*2)
allsd$N_levelscilo = allsd$N_levelscoef - (allsd$N_levelsse*2)
allsd$N_levelscihi = allsd$N_levelscoef + (allsd$N_levelsse*2)
allsd$N_correlcilo = allsd$N_correlcoef - (allsd$N_correlse*2)
allsd$N_correlcihi = allsd$N_correlcoef + (allsd$N_correlse*2)
allsd$levels_correlcilo = allsd$levels_correlcoef - (allsd$levels_correlse*2)
allsd$levels_correlcihi = allsd$levels_correlcoef + (allsd$levels_correlse*2)
allsd$N_levels_correlcilo = allsd$N_levels_correlcoef - (allsd$N_levels_correlse*2)
allsd$N_levels_correlcihi = allsd$N_levels_correlcoef + (allsd$N_levels_correlse*2)



######################################################### grouping CI's by overlap or non-overlap MEANS
CI_means = subset(allmean[ , c(17:18, 8, 29:30)])
CI_means$overlap1 = "NA"
First = 1
Next = 1
nsim = nrow(CI_means) - 1
CI_means$overlap1[First] = "Group1"

for(i in 1:nsim){
  Next = Next + 1
  if(CI_means$N_levels_correlcihi[First] <= CI_means$N_levels_correlcilo[Next] |
     CI_means$N_levels_correlcilo[First] >= CI_means$N_levels_correlcihi[Next]){
    CI_means$overlap1[Next] = "x"
  } else {
    CI_means$overlap1[Next] = "Group1"
  }
}

View(CI_means)

######################################################### grouping CI's by overlap or non-overlap MEANS



######################################################### grouping CI's by overlap or non-overlap SD
CI_sd = subset(allsd[ , c(17:18, 8, 29:30)])
CI_sd$overlap1 = "NA"
First = 1
Next = 1
nsim = nrow(CI_sd) - 1
CI_sd$overlap1[First] = "Group1"

for(i in 1:nsim){
  Next = Next + 1
  if(CI_sd$N_levels_correlcihi[First] <= CI_sd$N_levels_correlcilo[Next] |
     CI_sd$N_levels_correlcilo[First] >= CI_sd$N_levels_correlcihi[Next]){
    CI_sd$overlap1[Next] = "x"
  } else {
    CI_sd$overlap1[Next] = "Group1"
  }
}


View(CI_sd)

