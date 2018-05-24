##working directory
#setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Eta simulations/totals")

library(NbClust)
library(factoextra)
library(cluster)
library(clValid)
library(fpc)
library(RankAggreg)

#fulldata = read.csv("fulldata.csv")
fulldata <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Project/data/fulldata.csv")


fulldata$RM1.ges = fulldata$RM1.ges * 100
fulldata$RM2.ges = fulldata$RM2.ges * 100

#calculate partial eta squared
fulldata$RM1.pes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main)
fulldata$RM2.pes = fulldata$RM2.ssm.main / (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main)
fulldata$MIX.pes = fulldata$MIX.ssm.main / (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main)
fulldata$BN1.pes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.pes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all)
fulldata$RM1.pes = fulldata$RM1.pes * 100
fulldata$RM2.pes = fulldata$RM2.pes * 100

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


allmean = as.data.frame(allmean)
allsd = as.data.frame(allsd)

#################################################################################### Means Cluster Analysis
clustermean = allmean[ , -c(1, 9:16, 19:30)]

clustermean$Ncoef = as.numeric(as.character(clustermean$Ncoef))
clustermean$levelscoef = as.numeric(as.character(clustermean$levelscoef))
clustermean$correlcoef = as.numeric(as.character(clustermean$correlcoef))
clustermean$N_levelscoef = as.numeric(as.character(clustermean$N_levelscoef))
clustermean$N_correlcoef = as.numeric(as.character(clustermean$N_correlcoef))
clustermean$levels_correlcoef = as.numeric(as.character(clustermean$levels_correlcoef))
clustermean$N_levels_correlcoef = as.numeric(as.character(clustermean$N_levels_correlcoef))



##who is the overall winner?
result <- clValid(clustermean[ , -c(8:9)], nClust = 2:10,
                  clMethods=c("hierarchical","kmeans","pam"),
                  validation=c("internal","stability"))
res <- getRanksWeights(result)
print(res$ranks[,1:3], quote=FALSE)

CEWS <- RankAggreg(x=res$ranks, k=5, weights=res$weights, seed=123, verbose=FALSE)
CEWS
plot(CEWS)

# 2. Compute dissimilarity matrix
d <- dist(clustermean[ , -c(8:9)], method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Cut tree into 2 groups
grp <- cutree(res.hc, k = 2)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 2, border = 2:5) # add rectangle

#RM1 and RM2 PES

#################################################################################### Means Cluster Analysis



################################################################################### SD Cluster Analysis
clustersd = allsd[ , -c(1, 9:16, 19:30)]

clustersd$Ncoef = as.numeric(as.character(clustersd$Ncoef))
clustersd$levelscoef = as.numeric(as.character(clustersd$levelscoef))
clustersd$correlcoef = as.numeric(as.character(clustersd$correlcoef))
clustersd$N_levelscoef = as.numeric(as.character(clustersd$N_levelscoef))
clustersd$N_correlcoef = as.numeric(as.character(clustersd$N_correlcoef))
clustersd$levels_correlcoef = as.numeric(as.character(clustersd$levels_correlcoef))
clustersd$N_levels_correlcoef = as.numeric(as.character(clustersd$N_levels_correlcoef))




##who is the overall winner?
resultsd <- clValid(clustersd[ , -c(8:9)], nClust = 2:10,
                  clMethods=c("hierarchical","kmeans","pam"),
                  validation=c("internal","stability"))
ressd <- getRanksWeights(resultsd)
print(ressd$ranks[,1:3], quote=FALSE)

CEWSsd <- RankAggreg(x=ressd$ranks, k=5, weights=ressd$weights, seed=123, verbose=FALSE)
CEWSsd
plot(CEWSsd)


pam.res <- pam(clustersd[ , -c(8:9)], 2)
# Visualize
fviz_cluster(pam.res)

pam.res$clustering
tempsd = clustersd[ , c(8:9)]
tempsd = cbind(tempsd, pam.res$clustering)

#RM1 ges, fes, fos ; BN1 pos

################################################################################### SD Cluster Analysis


