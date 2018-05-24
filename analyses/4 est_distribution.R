##erin's eta script
setwd("C:/Users/BuchananLab/Downloads")

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
fulldata$RM1.fes = fulldata$RM1.ssm.main / 
                  (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main + fulldata$RM1.ssr.p)
fulldata$RM2.fes = fulldata$RM2.ssm.main / 
                  (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main + fulldata$RM2.ssm.other + fulldata$RM2.ssm.interact + fulldata$RM2.ssr.p + fulldata$RM2.ssr.other + fulldata$RM2.ssr.interact)
fulldata$MIX.fes = fulldata$MIX.ssm.main / 
                  (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main + fulldata$MIX.ssm.other + fulldata$MIX.ssm.interact + fulldata$MIX.ssr.p + fulldata$MIX.ssr.other + fulldata$MIX.ssr.interact)
fulldata$BN1.fes = fulldata$BN1.ssm.main / 
                  (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.fes = fulldata$BN2.ssm.main / 
                  (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all + fulldata$BN2.ssm.other + fulldata$BN2.ssm.interact)

#calculate full omega squared
fulldata$RM1.fos = (fulldata$RM1.dfm*((fulldata$RM1.ssm.main/fulldata$RM1.dfm)-(fulldata$RM1.ssr.main/fulldata$RM1.dfr)))/
  ((fulldata$RM1.ssm.main+fulldata$RM1.ssr.main+fulldata$RM1.ssr.p)+(fulldata$RM1.ssm.p/(fulldata$N - 1)))
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
  (fulldata$BN2.ssm.main+(((fulldata$N*levels)-fulldata$BN2.dfm)*(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))
fulldata$MIX.pos = (fulldata$MIX.dfm*((fulldata$MIX.ssm.main/fulldata$MIX.dfm)-(fulldata$MIX.ssr.main/fulldata$MIX.dfr)))/
  (fulldata$MIX.ssm.main+fulldata$MIX.ssr.main+fulldata$MIX.ssm.p+(fulldata$MIX.ssm.p/(fulldata$MIX.dfr/fulldata$MIX.dfm)))
fulldata$RM2.pos[fulldata$RM2.pos<0] = 0
fulldata$BN2.pos[fulldata$BN2.pos<0] = 0
fulldata$MIX.pos[fulldata$MIX.pos<0] = 0

####look at values to see how they compare####
names(fulldata)
justes = fulldata[ , c(15, 27, 39, 47, 56:74)]
cor(justes[ , 1:5]) ##rm1, mix, bn1, bn2 versus rm2 GES
cor(justes[ , 6:10]) ##rm1, rm2, mix versus bn1/bn2 PES
cor(justes[ , 11:15]) ##rm1, mix, bn1, bn2 versus rm2 FES
cor(justes[ , 16:20]) ##rm1, mix, bn1, bn2 versus rm2 FOS
cor(justes[ , 21:23]) ##all separate POS

options(scipen = 999)

####what distribution do these match####

####ges rm1 versus rm2####
library(fitdistrplus)
round = 0
distributiondata = data.frame(N = 1:1152)
testoutput = c(1:48)
testoutput2 = c(1:48)

##loop a over N
Nloops = seq(20, 110, 6)
for (a in 1:length(Nloops)) {
  ##loops b over levels
  levelloop = 3:6 
  for (b in 1:length(levelloop)) {
    ##loop c over effect size
    SDloops = c(5, 3, 1)
    for (c in 1:length(SDloops)) {
    ##loop d over correlation
      corloops = c(0, .1, .3, .5, .7, .9)
      for (d in 1:length(corloops)) {
        
        dataset = subset(fulldata,
                         N == Nloops[a] &
                           levels == levelloop[b] &
                           stdev == SDloops[c] &
                           correl == corloops[d])
        
        round = round + 1
        distributiondata$N[round] = Nloops[a]
        distributiondata$levels[round] = levelloop[b]
        distributiondata$stdev[round] = SDloops[c]
        distributiondata$correl[round] = corloops[d]
        
        ##don't work with zeroes
        dataset2 = subset(dataset, RM1.ges > 0)
        dataset3 = subset(dataset, RM2.ges > 0)
        
        ##normal distribution
        normaloutput = fitdist(dataset2$RM1.ges, "norm", keepdata = T)
        normaloutput2 = fitdist(dataset3$RM2.ges, "norm", keepdata = T)
        distributiondata$norm.1.aic[round] = normaloutput$aic
        distributiondata$norm.1.mean[round] = normaloutput$estimate[1]
        distributiondata$norm.1.sd[round] = normaloutput$estimate[2]
        distributiondata$norm.2.aic[round] = normaloutput2$aic
        distributiondata$norm.2.mean[round] = normaloutput2$estimate[1]
        distributiondata$norm.2.stdev[round] = normaloutput2$estimate[2]
        
        ##exponential
        expoutput = fitdist(dataset2$RM1.ges, "exp", keepdata = T)
        expoutput2 = fitdist(dataset3$RM2.ges, "exp", keepdata = T)
        distributiondata$exp.1.aic[round] = expoutput$aic
        distributiondata$exp.1.rate[round] = expoutput$estimate
        distributiondata$exp.2.aic[round] = expoutput2$aic
        distributiondata$exp.2.rate[round] = expoutput2$estimate
        
        ##gamma
        gammaoutput = fitdist(dataset2$RM1.ges, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        gammaoutput2 = fitdist(dataset3$RM2.ges, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        distributiondata$gamma.1.aic[round] = gammaoutput$aic
        distributiondata$gamma.1.shape[round] = gammaoutput$estimate[1]
        distributiondata$gamma.1.rate[round] = gammaoutput$estimate[2]
        distributiondata$gamma.2.aic[round] = gammaoutput2$aic
        distributiondata$gamma.2.shape[round] = gammaoutput2$estimate[1]
        distributiondata$gamma.2.rate[round] = gammaoutput2$estimate[2]
        
        ##log normal
        lognormoutput = fitdist(dataset2$RM1.ges, "lnorm", keepdata = T)
        lognormoutput2 = fitdist(dataset3$RM2.ges, "lnorm", keepdata = T)
        distributiondata$lnorm.1.aic[round] = lognormoutput$aic
        distributiondata$lnorm.1.mean[round] = lognormoutput$estimate[1]
        distributiondata$lnorm.1.stdev[round] = lognormoutput$estimate[2]
        distributiondata$lnorm.2.aic[round] = lognormoutput2$aic
        distributiondata$lnorm.2.mean[round] = lognormoutput2$estimate[1]
        distributiondata$lnorm.2.stdev[round] = lognormoutput2$estimate[2]
        
        ##beta
        betaoutput = fitdist(dataset2$RM1.ges, "beta", keepdata = T)
        betaoutput2 = fitdist(dataset3$RM2.ges, "beta", keepdata = T)
        distributiondata$beta.1.aic[round] = betaoutput$aic
        distributiondata$beta.1.shape1[round] = betaoutput$estimate[1]
        distributiondata$beta.1.shape2[round] = betaoutput$estimate[2]
        distributiondata$beta.2.aic[round] = betaoutput2$aic
        distributiondata$beta.2.shape1[round] = betaoutput2$estimate[1]
        distributiondata$beta.2.shape2[round] = betaoutput2$estimate[2]
        
        ##chi square
        chioutput = fitdist(dataset2$RM1.ges, "chisq", start = list(df = 1))
        chioutput2 = fitdist(dataset3$RM2.ges, "chisq", start = list(df = 1))
        distributiondata$chi.1.aic[round] = chioutput$aic
        distributiondata$chi.1.df[round] = chioutput$estimate
        distributiondata$chi.2.aic[round] = chioutput2$aic
        distributiondata$chi.2.df[round] = chioutput2$estimate
        
        ##f
        foutput = fitdist(dataset2$RM1.ges, "f", keepdata = T, 
                          start = list(df1 = (dataset$RM1.dfm[1]), df2 = (dataset$RM1.dfr[1])), 
                          optim.method="SANN")
        foutput2 = fitdist(dataset3$RM2.ges, "f", keepdata = T, 
                           start = list(df1 = (dataset$RM2.dfm[1]), df2 = (dataset$RM2.dfr[1])), 
                           optim.method="SANN")
        distributiondata$f.1.aic[round] = foutput$aic
        distributiondata$f.1.df1[round] = foutput$estimate[1]
        distributiondata$f.1.df2[round] = foutput$estimate[2]
        distributiondata$f.2.aic[round] = foutput2$aic
        distributiondata$f.2.df1[round] = foutput2$estimate[1]
        distributiondata$f.2.df2[round] = foutput2$estimate[2]        

        ##logistic
        logoutput = fitdist(dataset2$RM1.ges, "logis", keepdata = T)
        logoutput2 = fitdist(dataset3$RM2.ges, "logis", keepdata = T)
        distributiondata$log.1.aic[round] = logoutput$aic
        distributiondata$log.1.loc[round] = logoutput$estimate[1]
        distributiondata$log.1.scale[round] = logoutput$estimate[2]
        distributiondata$log.2.aic[round] = logoutput2$aic
        distributiondata$log.2.loc[round] = logoutput2$estimate[1]
        distributiondata$log.2.scale[round] = logoutput2$estimate[2]
        
        ##test against each other
        testdistribution = gofstat(list(normaloutput, expoutput, gammaoutput,
                             lognormoutput, betaoutput, chioutput,
                             foutput, logoutput),
                        fitnames = c("normal", "exponential", "gamma",
                                     "log normal", "beta", "chi square",
                                     "f distribution", "logistic"))
        testdistribution2 = gofstat(list(normaloutput2, expoutput2, gammaoutput2,
                                        lognormoutput2, betaoutput2, chioutput2,
                                        foutput2, logoutput2),
                                   fitnames = c("normal", "exponential", "gamma",
                                                "log normal", "beta", "chi square",
                                                "f distribution", "logistic"))
                        
        numbers = c(testdistribution$ks, testdistribution$kstest, testdistribution$cvm,
                    testdistribution$cvmtest, testdistribution$ad, testdistribution$adtest)
        
        numbers2 = c(testdistribution2$ks, testdistribution2$kstest, testdistribution2$cvm,
                    testdistribution2$cvmtest, testdistribution2$ad, testdistribution2$adtest)
        
        testoutput = rbind(testoutput, numbers)
        testoutput2 = rbind(testoutput2, numbers2)
        
      }
    }
  }
}

####pes rm1 versus bn1####
library(fitdistrplus)
round = 0
distributiondata = data.frame(N = 1:1152)
testoutput = c(1:48)
testoutput2 = c(1:48)

##loop a over N
Nloops = seq(20, 110, 6)
for (a in 1:length(Nloops)) {
  ##loops b over levels
  levelloop = 3:6 
  for (b in 1:length(levelloop)) {
    ##loop c over effect size
    SDloops = c(5, 3, 1)
    for (c in 1:length(SDloops)) {
      ##loop d over correlation
      corloops = c(0, .1, .3, .5, .7, .9)
      for (d in 1:length(corloops)) {
        
        dataset = subset(fulldata,
                         N == Nloops[a] &
                           levels == levelloop[b] &
                           stdev == SDloops[c] &
                           correl == corloops[d])
        
        round = round + 1
        distributiondata$N[round] = Nloops[a]
        distributiondata$levels[round] = levelloop[b]
        distributiondata$stdev[round] = SDloops[c]
        distributiondata$correl[round] = corloops[d]
        
        ##don't work with zeroes
        dataset2 = subset(dataset, RM1.pes > 0)
        dataset3 = subset(dataset, BN1.pes > 0)
        
        ##normal distribution
        normaloutput = fitdist(dataset2$RM1.pes, "norm", keepdata = T)
        normaloutput2 = fitdist(dataset3$BN1.pes, "norm", keepdata = T)
        distributiondata$norm.1.aic[round] = normaloutput$aic
        distributiondata$norm.1.mean[round] = normaloutput$estimate[1]
        distributiondata$norm.1.sd[round] = normaloutput$estimate[2]
        distributiondata$norm.2.aic[round] = normaloutput2$aic
        distributiondata$norm.2.mean[round] = normaloutput2$estimate[1]
        distributiondata$norm.2.stdev[round] = normaloutput2$estimate[2]
        
        ##exponential
        expoutput = fitdist(dataset2$RM1.pes, "exp", keepdata = T)
        expoutput2 = fitdist(dataset3$BN1.pes, "exp", keepdata = T)
        distributiondata$exp.1.aic[round] = expoutput$aic
        distributiondata$exp.1.rate[round] = expoutput$estimate
        distributiondata$exp.2.aic[round] = expoutput2$aic
        distributiondata$exp.2.rate[round] = expoutput2$estimate
        
        ##gamma
        gammaoutput = fitdist(dataset2$RM1.pes, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        gammaoutput2 = fitdist(dataset3$BN1.pes, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        distributiondata$gamma.1.aic[round] = gammaoutput$aic
        distributiondata$gamma.1.shape[round] = gammaoutput$estimate[1]
        distributiondata$gamma.1.rate[round] = gammaoutput$estimate[2]
        distributiondata$gamma.2.aic[round] = gammaoutput2$aic
        distributiondata$gamma.2.shape[round] = gammaoutput2$estimate[1]
        distributiondata$gamma.2.rate[round] = gammaoutput2$estimate[2]
        
        ##log normal
        lognormoutput = fitdist(dataset2$RM1.pes, "lnorm", keepdata = T)
        lognormoutput2 = fitdist(dataset3$BN1.pes, "lnorm", keepdata = T)
        distributiondata$lnorm.1.aic[round] = lognormoutput$aic
        distributiondata$lnorm.1.mean[round] = lognormoutput$estimate[1]
        distributiondata$lnorm.1.stdev[round] = lognormoutput$estimate[2]
        distributiondata$lnorm.2.aic[round] = lognormoutput2$aic
        distributiondata$lnorm.2.mean[round] = lognormoutput2$estimate[1]
        distributiondata$lnorm.2.stdev[round] = lognormoutput2$estimate[2]
        
        ##beta
        betaoutput = fitdist(dataset2$RM1.pes, "beta", keepdata = T)
        betaoutput2 = fitdist(dataset3$BN1.pes, "beta", keepdata = T)
        distributiondata$beta.1.aic[round] = betaoutput$aic
        distributiondata$beta.1.shape1[round] = betaoutput$estimate[1]
        distributiondata$beta.1.shape2[round] = betaoutput$estimate[2]
        distributiondata$beta.2.aic[round] = betaoutput2$aic
        distributiondata$beta.2.shape1[round] = betaoutput2$estimate[1]
        distributiondata$beta.2.shape2[round] = betaoutput2$estimate[2]
        
        ##chi square
        chioutput = fitdist(dataset2$RM1.pes, "chisq", start = list(df = 1))
        chioutput2 = fitdist(dataset3$BN1.pes, "chisq", start = list(df = 1))
        distributiondata$chi.1.aic[round] = chioutput$aic
        distributiondata$chi.1.df[round] = chioutput$estimate
        distributiondata$chi.2.aic[round] = chioutput2$aic
        distributiondata$chi.2.df[round] = chioutput2$estimate
        
        ##f
        foutput = fitdist(dataset2$RM1.pes, "f", keepdata = T, 
                          start = list(df1 = (dataset$RM1.dfm[1]), df2 = (dataset$RM1.dfr[1])), 
                          optim.method="SANN")
        foutput2 = fitdist(dataset3$BN1.pes, "f", keepdata = T, 
                           start = list(df1 = (dataset$BN1.dfm[1]), df2 = (dataset$BN1.dfr[1])), 
                           optim.method="SANN")
        distributiondata$f.1.aic[round] = foutput$aic
        distributiondata$f.1.df1[round] = foutput$estimate[1]
        distributiondata$f.1.df2[round] = foutput$estimate[2]
        distributiondata$f.2.aic[round] = foutput2$aic
        distributiondata$f.2.df1[round] = foutput2$estimate[1]
        distributiondata$f.2.df2[round] = foutput2$estimate[2]        
        
        ##logistic
        logoutput = fitdist(dataset2$RM1.pes, "logis", keepdata = T)
        logoutput2 = fitdist(dataset3$BN1.pes, "logis", keepdata = T)
        distributiondata$log.1.aic[round] = logoutput$aic
        distributiondata$log.1.loc[round] = logoutput$estimate[1]
        distributiondata$log.1.scale[round] = logoutput$estimate[2]
        distributiondata$log.2.aic[round] = logoutput2$aic
        distributiondata$log.2.loc[round] = logoutput2$estimate[1]
        distributiondata$log.2.scale[round] = logoutput2$estimate[2]
        
        ##test against each other
        testdistribution = gofstat(list(normaloutput, expoutput, gammaoutput,
                                        lognormoutput, betaoutput, chioutput,
                                        foutput, logoutput),
                                   fitnames = c("normal", "exponential", "gamma",
                                                "log normal", "beta", "chi square",
                                                "f distribution", "logistic"))
        testdistribution2 = gofstat(list(normaloutput2, expoutput2, gammaoutput2,
                                         lognormoutput2, betaoutput2, chioutput2,
                                         foutput2, logoutput2),
                                    fitnames = c("normal", "exponential", "gamma",
                                                 "log normal", "beta", "chi square",
                                                 "f distribution", "logistic"))
        
        numbers = c(testdistribution$ks, testdistribution$kstest, testdistribution$cvm,
                    testdistribution$cvmtest, testdistribution$ad, testdistribution$adtest)
        
        numbers2 = c(testdistribution2$ks, testdistribution2$kstest, testdistribution2$cvm,
                     testdistribution2$cvmtest, testdistribution2$ad, testdistribution2$adtest)
        
        testoutput = rbind(testoutput, numbers)
        testoutput2 = rbind(testoutput2, numbers2)
        
      }
    }
  }
}

####fes rm1 versus rm2####
library(fitdistrplus)
round = 0
distributiondata = data.frame(N = 1:1152)
testoutput = c(1:48)
testoutput2 = c(1:48)

##loop a over N
Nloops = seq(20, 110, 6)
for (a in 1:length(Nloops)) {
  ##loops b over levels
  levelloop = 3:6 
  for (b in 1:length(levelloop)) {
    ##loop c over effect size
    SDloops = c(5, 3, 1)
    for (c in 1:length(SDloops)) {
      ##loop d over correlation
      corloops = c(0, .1, .3, .5, .7, .9)
      for (d in 1:length(corloops)) {
        
        dataset = subset(fulldata,
                         N == Nloops[a] &
                           levels == levelloop[b] &
                           stdev == SDloops[c] &
                           correl == corloops[d])
        
        round = round + 1
        distributiondata$N[round] = Nloops[a]
        distributiondata$levels[round] = levelloop[b]
        distributiondata$stdev[round] = SDloops[c]
        distributiondata$correl[round] = corloops[d]
        
        ##don't work with zeroes
        dataset2 = subset(dataset, RM1.fes > 0)
        dataset3 = subset(dataset, RM2.fes > 0)
        
        ##normal distribution
        normaloutput = fitdist(dataset2$RM1.fes, "norm", keepdata = T)
        normaloutput2 = fitdist(dataset3$RM2.fes, "norm", keepdata = T)
        distributiondata$norm.1.aic[round] = normaloutput$aic
        distributiondata$norm.1.mean[round] = normaloutput$estimate[1]
        distributiondata$norm.1.sd[round] = normaloutput$estimate[2]
        distributiondata$norm.2.aic[round] = normaloutput2$aic
        distributiondata$norm.2.mean[round] = normaloutput2$estimate[1]
        distributiondata$norm.2.stdev[round] = normaloutput2$estimate[2]
        
        ##exponential
        expoutput = fitdist(dataset2$RM1.fes, "exp", keepdata = T)
        expoutput2 = fitdist(dataset3$RM2.fes, "exp", keepdata = T)
        distributiondata$exp.1.aic[round] = expoutput$aic
        distributiondata$exp.1.rate[round] = expoutput$estimate
        distributiondata$exp.2.aic[round] = expoutput2$aic
        distributiondata$exp.2.rate[round] = expoutput2$estimate
        
        ##gamma
        gammaoutput = fitdist(dataset2$RM1.fes, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        gammaoutput2 = fitdist(dataset3$RM2.fes, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        distributiondata$gamma.1.aic[round] = gammaoutput$aic
        distributiondata$gamma.1.shape[round] = gammaoutput$estimate[1]
        distributiondata$gamma.1.rate[round] = gammaoutput$estimate[2]
        distributiondata$gamma.2.aic[round] = gammaoutput2$aic
        distributiondata$gamma.2.shape[round] = gammaoutput2$estimate[1]
        distributiondata$gamma.2.rate[round] = gammaoutput2$estimate[2]
        
        ##log normal
        lognormoutput = fitdist(dataset2$RM1.fes, "lnorm", keepdata = T)
        lognormoutput2 = fitdist(dataset3$RM2.fes, "lnorm", keepdata = T)
        distributiondata$lnorm.1.aic[round] = lognormoutput$aic
        distributiondata$lnorm.1.mean[round] = lognormoutput$estimate[1]
        distributiondata$lnorm.1.stdev[round] = lognormoutput$estimate[2]
        distributiondata$lnorm.2.aic[round] = lognormoutput2$aic
        distributiondata$lnorm.2.mean[round] = lognormoutput2$estimate[1]
        distributiondata$lnorm.2.stdev[round] = lognormoutput2$estimate[2]
        
        ##beta
        betaoutput = fitdist(dataset2$RM1.fes, "beta", keepdata = T)
        betaoutput2 = fitdist(dataset3$RM2.fes, "beta", keepdata = T)
        distributiondata$beta.1.aic[round] = betaoutput$aic
        distributiondata$beta.1.shape1[round] = betaoutput$estimate[1]
        distributiondata$beta.1.shape2[round] = betaoutput$estimate[2]
        distributiondata$beta.2.aic[round] = betaoutput2$aic
        distributiondata$beta.2.shape1[round] = betaoutput2$estimate[1]
        distributiondata$beta.2.shape2[round] = betaoutput2$estimate[2]
        
        ##chi square
        chioutput = fitdist(dataset2$RM1.fes, "chisq", start = list(df = 1))
        chioutput2 = fitdist(dataset3$RM2.fes, "chisq", start = list(df = 1))
        distributiondata$chi.1.aic[round] = chioutput$aic
        distributiondata$chi.1.df[round] = chioutput$estimate
        distributiondata$chi.2.aic[round] = chioutput2$aic
        distributiondata$chi.2.df[round] = chioutput2$estimate
        
        ##f
        foutput = fitdist(dataset2$RM1.fes, "f", keepdata = T, 
                          start = list(df1 = (dataset$RM1.dfm[1]), df2 = (dataset$RM1.dfr[1])), 
                          optim.method="SANN")
        foutput2 = fitdist(dataset3$RM2.fes, "f", keepdata = T, 
                           start = list(df1 = (dataset$RM2.dfm[1]), df2 = (dataset$RM2.dfr[1])), 
                           optim.method="SANN")
        distributiondata$f.1.aic[round] = foutput$aic
        distributiondata$f.1.df1[round] = foutput$estimate[1]
        distributiondata$f.1.df2[round] = foutput$estimate[2]
        distributiondata$f.2.aic[round] = foutput2$aic
        distributiondata$f.2.df1[round] = foutput2$estimate[1]
        distributiondata$f.2.df2[round] = foutput2$estimate[2]        
        
        ##logistic
        logoutput = fitdist(dataset2$RM1.fes, "logis", keepdata = T)
        logoutput2 = fitdist(dataset3$RM2.fes, "logis", keepdata = T)
        distributiondata$log.1.aic[round] = logoutput$aic
        distributiondata$log.1.loc[round] = logoutput$estimate[1]
        distributiondata$log.1.scale[round] = logoutput$estimate[2]
        distributiondata$log.2.aic[round] = logoutput2$aic
        distributiondata$log.2.loc[round] = logoutput2$estimate[1]
        distributiondata$log.2.scale[round] = logoutput2$estimate[2]
        
        ##test against each other
        testdistribution = gofstat(list(normaloutput, expoutput, gammaoutput,
                                        lognormoutput, betaoutput, chioutput,
                                        foutput, logoutput),
                                   fitnames = c("normal", "exponential", "gamma",
                                                "log normal", "beta", "chi square",
                                                "f distribution", "logistic"))
        testdistribution2 = gofstat(list(normaloutput2, expoutput2, gammaoutput2,
                                         lognormoutput2, betaoutput2, chioutput2,
                                         foutput2, logoutput2),
                                    fitnames = c("normal", "exponential", "gamma",
                                                 "log normal", "beta", "chi square",
                                                 "f distribution", "logistic"))
        
        numbers = c(testdistribution$ks, testdistribution$kstest, testdistribution$cvm,
                    testdistribution$cvmtest, testdistribution$ad, testdistribution$adtest)
        
        numbers2 = c(testdistribution2$ks, testdistribution2$kstest, testdistribution2$cvm,
                     testdistribution2$cvmtest, testdistribution2$ad, testdistribution2$adtest)
        
        testoutput = rbind(testoutput, numbers)
        testoutput2 = rbind(testoutput2, numbers2)
        
      }
    }
  }
}

####fos rm1 versus rm2####
library(fitdistrplus)
round = 0
distributiondata = data.frame(N = 1:1152)
testoutput = c(1:48)
testoutput2 = c(1:48)

##loop a over N
Nloops = seq(20, 110, 6)
for (a in 1:length(Nloops)) {
  ##loops b over levels
  levelloop = 3:6 
  for (b in 1:length(levelloop)) {
    ##loop c over effect size
    SDloops = c(5, 3, 1)
    for (c in 1:length(SDloops)) {
      ##loop d over correlation
      corloops = c(0, .1, .3, .5, .7, .9)
      for (d in 1:length(corloops)) {
        
        dataset = subset(fulldata,
                         N == Nloops[a] &
                           levels == levelloop[b] &
                           stdev == SDloops[c] &
                           correl == corloops[d])
        
        round = round + 1
        distributiondata$N[round] = Nloops[a]
        distributiondata$levels[round] = levelloop[b]
        distributiondata$stdev[round] = SDloops[c]
        distributiondata$correl[round] = corloops[d]
        
        ##don't work with zeroes
        dataset2 = subset(dataset, RM1.fos > 0)
        dataset3 = subset(dataset, RM2.fos > 0)
        
        ##normal distribution
        normaloutput = fitdist(dataset2$RM1.fos, "norm", keepdata = T)
        normaloutput2 = fitdist(dataset3$RM2.fos, "norm", keepdata = T)
        distributiondata$norm.1.aic[round] = normaloutput$aic
        distributiondata$norm.1.mean[round] = normaloutput$estimate[1]
        distributiondata$norm.1.sd[round] = normaloutput$estimate[2]
        distributiondata$norm.2.aic[round] = normaloutput2$aic
        distributiondata$norm.2.mean[round] = normaloutput2$estimate[1]
        distributiondata$norm.2.stdev[round] = normaloutput2$estimate[2]
        
        ##exponential
        expoutput = fitdist(dataset2$RM1.fos, "exp", keepdata = T)
        expoutput2 = fitdist(dataset3$RM2.fos, "exp", keepdata = T)
        distributiondata$exp.1.aic[round] = expoutput$aic
        distributiondata$exp.1.rate[round] = expoutput$estimate
        distributiondata$exp.2.aic[round] = expoutput2$aic
        distributiondata$exp.2.rate[round] = expoutput2$estimate
        
        ##gamma
        gammaoutput = fitdist(dataset2$RM1.fos, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        gammaoutput2 = fitdist(dataset3$RM2.fos, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        distributiondata$gamma.1.aic[round] = gammaoutput$aic
        distributiondata$gamma.1.shape[round] = gammaoutput$estimate[1]
        distributiondata$gamma.1.rate[round] = gammaoutput$estimate[2]
        distributiondata$gamma.2.aic[round] = gammaoutput2$aic
        distributiondata$gamma.2.shape[round] = gammaoutput2$estimate[1]
        distributiondata$gamma.2.rate[round] = gammaoutput2$estimate[2]
        
        ##log normal
        lognormoutput = fitdist(dataset2$RM1.fos, "lnorm", keepdata = T)
        lognormoutput2 = fitdist(dataset3$RM2.fos, "lnorm", keepdata = T)
        distributiondata$lnorm.1.aic[round] = lognormoutput$aic
        distributiondata$lnorm.1.mean[round] = lognormoutput$estimate[1]
        distributiondata$lnorm.1.stdev[round] = lognormoutput$estimate[2]
        distributiondata$lnorm.2.aic[round] = lognormoutput2$aic
        distributiondata$lnorm.2.mean[round] = lognormoutput2$estimate[1]
        distributiondata$lnorm.2.stdev[round] = lognormoutput2$estimate[2]
        
        ##beta
        betaoutput = fitdist(dataset2$RM1.fos, "beta", keepdata = T)
        betaoutput2 = fitdist(dataset3$RM2.fos, "beta", keepdata = T)
        distributiondata$beta.1.aic[round] = betaoutput$aic
        distributiondata$beta.1.shape1[round] = betaoutput$estimate[1]
        distributiondata$beta.1.shape2[round] = betaoutput$estimate[2]
        distributiondata$beta.2.aic[round] = betaoutput2$aic
        distributiondata$beta.2.shape1[round] = betaoutput2$estimate[1]
        distributiondata$beta.2.shape2[round] = betaoutput2$estimate[2]
        
        ##chi square
        chioutput = fitdist(dataset2$RM1.fos, "chisq", start = list(df = 1))
        chioutput2 = fitdist(dataset3$RM2.fos, "chisq", start = list(df = 1))
        distributiondata$chi.1.aic[round] = chioutput$aic
        distributiondata$chi.1.df[round] = chioutput$estimate
        distributiondata$chi.2.aic[round] = chioutput2$aic
        distributiondata$chi.2.df[round] = chioutput2$estimate
        
        ##f
        foutput = fitdist(dataset2$RM1.fos, "f", keepdata = T, 
                          start = list(df1 = (dataset$RM1.dfm[1]), df2 = (dataset$RM1.dfr[1])), 
                          optim.method="SANN")
        foutput2 = fitdist(dataset3$RM2.fos, "f", keepdata = T, 
                           start = list(df1 = (dataset$RM2.dfm[1]), df2 = (dataset$RM2.dfr[1])), 
                           optim.method="SANN")
        distributiondata$f.1.aic[round] = foutput$aic
        distributiondata$f.1.df1[round] = foutput$estimate[1]
        distributiondata$f.1.df2[round] = foutput$estimate[2]
        distributiondata$f.2.aic[round] = foutput2$aic
        distributiondata$f.2.df1[round] = foutput2$estimate[1]
        distributiondata$f.2.df2[round] = foutput2$estimate[2]        
        
        ##logistic
        logoutput = fitdist(dataset2$RM1.fos, "logis", keepdata = T)
        logoutput2 = fitdist(dataset3$RM2.fos, "logis", keepdata = T)
        distributiondata$log.1.aic[round] = logoutput$aic
        distributiondata$log.1.loc[round] = logoutput$estimate[1]
        distributiondata$log.1.scale[round] = logoutput$estimate[2]
        distributiondata$log.2.aic[round] = logoutput2$aic
        distributiondata$log.2.loc[round] = logoutput2$estimate[1]
        distributiondata$log.2.scale[round] = logoutput2$estimate[2]
        
        ##test against each other
        testdistribution = gofstat(list(normaloutput, expoutput, gammaoutput,
                                        lognormoutput, betaoutput, chioutput,
                                        foutput, logoutput),
                                   fitnames = c("normal", "exponential", "gamma",
                                                "log normal", "beta", "chi square",
                                                "f distribution", "logistic"))
        testdistribution2 = gofstat(list(normaloutput2, expoutput2, gammaoutput2,
                                         lognormoutput2, betaoutput2, chioutput2,
                                         foutput2, logoutput2),
                                    fitnames = c("normal", "exponential", "gamma",
                                                 "log normal", "beta", "chi square",
                                                 "f distribution", "logistic"))
        
        numbers = c(testdistribution$ks, testdistribution$kstest, testdistribution$cvm,
                    testdistribution$cvmtest, testdistribution$ad, testdistribution$adtest)
        
        numbers2 = c(testdistribution2$ks, testdistribution2$kstest, testdistribution2$cvm,
                     testdistribution2$cvmtest, testdistribution2$ad, testdistribution2$adtest)
        
        testoutput = rbind(testoutput, numbers)
        testoutput2 = rbind(testoutput2, numbers2)
        
      }
    }
  }
}

####POS rm2, bn2, mix####
library(fitdistrplus)
round = 0
distributiondata = data.frame(N = 1:1152)
#testoutput = c(1:48)
testoutput2 = c(1:48)
testoutput3 = c(1:48)

##loop a over N
Nloops = seq(20, 110, 6)
for (a in 1:length(Nloops)) {
  ##loops b over levels
  levelloop = 3:6 
  for (b in 1:length(levelloop)) {
    ##loop c over effect size
    SDloops = c(5, 3, 1)
    for (c in 1:length(SDloops)) {
      ##loop d over correlation
      corloops = c(0, .1, .3, .5, .7, .9)
      for (d in 1:length(corloops)) {
        
        dataset = subset(fulldata,
                         N == Nloops[a] &
                           levels == levelloop[b] &
                           stdev == SDloops[c] &
                           correl == corloops[d])
        
        round = round + 1
        distributiondata$N[round] = Nloops[a]
        distributiondata$levels[round] = levelloop[b]
        distributiondata$stdev[round] = SDloops[c]
        distributiondata$correl[round] = corloops[d]
        
        ##don't work with zeroes
        #dataset2 = subset(dataset, RM2.pos > 0)
        dataset3 = subset(dataset, BN2.pos > 0)
        dataset4 = subset(dataset, MIX.pos > 0)
        
        ##normal distribution
        #normaloutput = fitdist(dataset2$RM2.pos, "norm", optim.method="Nelder-Mead", keepdata = T)
        normaloutput2 = fitdist(dataset3$BN2.pos, "norm", keepdata = T)
        normaloutput3 = fitdist(dataset4$MIX.pos, "norm", keepdata = T)
        #distributiondata$norm.1.aic[round] = normaloutput$aic
        #distributiondata$norm.1.mean[round] = normaloutput$estimate[1]
        #distributiondata$norm.1.sd[round] = normaloutput$estimate[2]
        distributiondata$norm.2.aic[round] = normaloutput2$aic
        distributiondata$norm.2.mean[round] = normaloutput2$estimate[1]
        distributiondata$norm.2.stdev[round] = normaloutput2$estimate[2]
        distributiondata$norm.3.aic[round] = normaloutput3$aic
        distributiondata$norm.3.mean[round] = normaloutput3$estimate[1]
        distributiondata$norm.3.stdev[round] = normaloutput3$estimate[2]
        
        ##exponential
        #expoutput = fitdist(dataset2$RM2.pos, "exp", keepdata = T)
        expoutput2 = fitdist(dataset3$BN2.pos, "exp", keepdata = T)
        expoutput3 = fitdist(dataset4$MIX.pos, "exp", keepdata = T)
        #distributiondata$exp.1.aic[round] = expoutput$aic
        #distributiondata$exp.1.rate[round] = expoutput$estimate
        distributiondata$exp.2.aic[round] = expoutput2$aic
        distributiondata$exp.2.rate[round] = expoutput2$estimate
        distributiondata$exp.3.aic[round] = expoutput3$aic
        distributiondata$exp.3.rate[round] = expoutput3$estimate
        
        ##gamma
        #gammaoutput = fitdist(dataset2$RM2.pos, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        gammaoutput2 = fitdist(dataset3$BN2.pos, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        gammaoutput3 = fitdist(dataset4$MIX.pos, "gamma", lower = c(0,0), start = list(shape = 1.0, rate = 5), keepdata = T)
        #distributiondata$gamma.1.aic[round] = gammaoutput$aic
        #distributiondata$gamma.1.shape[round] = gammaoutput$estimate[1]
        #distributiondata$gamma.1.rate[round] = gammaoutput$estimate[2]
        distributiondata$gamma.2.aic[round] = gammaoutput2$aic
        distributiondata$gamma.2.shape[round] = gammaoutput2$estimate[1]
        distributiondata$gamma.2.rate[round] = gammaoutput2$estimate[2]
        distributiondata$gamma.3.aic[round] = gammaoutput3$aic
        distributiondata$gamma.3.shape[round] = gammaoutput3$estimate[1]
        distributiondata$gamma.3.rate[round] = gammaoutput3$estimate[2]
        
        ##log normal
        #lognormoutput = fitdist(dataset2$RM2.pos, "lnorm", keepdata = T)
        lognormoutput2 = fitdist(dataset3$BN2.pos, "lnorm", keepdata = T)
        lognormoutput3 = fitdist(dataset4$MIX.pos, "lnorm", keepdata = T)
        #distributiondata$lnorm.1.aic[round] = lognormoutput$aic
        #distributiondata$lnorm.1.mean[round] = lognormoutput$estimate[1]
        #distributiondata$lnorm.1.stdev[round] = lognormoutput$estimate[2]
        distributiondata$lnorm.2.aic[round] = lognormoutput2$aic
        distributiondata$lnorm.2.mean[round] = lognormoutput2$estimate[1]
        distributiondata$lnorm.2.stdev[round] = lognormoutput2$estimate[2]
        distributiondata$lnorm.3.aic[round] = lognormoutput3$aic
        distributiondata$lnorm.3.mean[round] = lognormoutput3$estimate[1]
        distributiondata$lnorm.3.stdev[round] = lognormoutput3$estimate[2]
        
        ##beta
        #betaoutput = fitdist(dataset2$RM2.pos, "beta", keepdata = T)
        betaoutput2 = fitdist(dataset3$BN2.pos, "beta", keepdata = T)
        betaoutput3 = fitdist(dataset4$MIX.pos, "beta", keepdata = T)
        #distributiondata$beta.1.aic[round] = betaoutput$aic
        #distributiondata$beta.1.shape1[round] = betaoutput$estimate[1]
        #distributiondata$beta.1.shape2[round] = betaoutput$estimate[2]
        distributiondata$beta.2.aic[round] = betaoutput2$aic
        distributiondata$beta.2.shape1[round] = betaoutput2$estimate[1]
        distributiondata$beta.2.shape2[round] = betaoutput2$estimate[2]
        distributiondata$beta.3.aic[round] = betaoutput3$aic
        distributiondata$beta.3.shape1[round] = betaoutput3$estimate[1]
        distributiondata$beta.3.shape2[round] = betaoutput3$estimate[2]
        
        ##chi square
        #chioutput = fitdist(dataset2$RM2.pos, "chisq", start = list(df = 1))
        chioutput2 = fitdist(dataset3$BN2.pos, "chisq", start = list(df = 1))
        chioutput3 = fitdist(dataset4$MIX.pos, "chisq", start = list(df = 1))
        #distributiondata$chi.1.aic[round] = chioutput$aic
        #distributiondata$chi.1.df[round] = chioutput$estimate
        distributiondata$chi.2.aic[round] = chioutput2$aic
        distributiondata$chi.2.df[round] = chioutput2$estimate
        distributiondata$chi.3.aic[round] = chioutput3$aic
        distributiondata$chi.3.df[round] = chioutput3$estimate
        
        ##f
        #foutput = fitdist(dataset2$RM2.pos, "f", keepdata = T, 
        #                  start = list(df1 = (dataset$RM2.dfm[1]), df2 = (dataset$RM2.dfr[1])), 
        #                  optim.method="SANN")
        foutput2 = fitdist(dataset3$BN2.pos, "f", keepdata = T, 
                           start = list(df1 = (dataset$BN2.dfm[1]), df2 = (dataset$BN2.dfr[1])), 
                           optim.method="SANN")
        foutput3 = fitdist(dataset4$MIX.pos, "f", keepdata = T, 
                           start = list(df1 = (dataset$MIX.dfm[1]), df2 = (dataset$MIX.dfr[1])), 
                           optim.method="SANN")
        #distributiondata$f.1.aic[round] = foutput$aic
        #distributiondata$f.1.df1[round] = foutput$estimate[1]
        #distributiondata$f.1.df2[round] = foutput$estimate[2]
        distributiondata$f.2.aic[round] = foutput2$aic
        distributiondata$f.2.df1[round] = foutput2$estimate[1]
        distributiondata$f.2.df2[round] = foutput2$estimate[2]        
        distributiondata$f.3.aic[round] = foutput3$aic
        distributiondata$f.3.df1[round] = foutput3$estimate[1]
        distributiondata$f.3.df2[round] = foutput3$estimate[2]
        
        ##logistic
        #logoutput = fitdist(dataset2$RM2.pos, "logis", keepdata = T)
        #logoutput2 = fitdist(dataset3$BN2.pos, "logis", keepdata = T)
        #logoutput3 = fitdist(dataset4$MIX.pos, "logis", keepdata = T)
        #distributiondata$log.1.aic[round] = logoutput$aic
        #distributiondata$log.1.loc[round] = logoutput$estimate[1]
        #distributiondata$log.1.scale[round] = logoutput$estimate[2]
        #distributiondata$log.2.aic[round] = logoutput2$aic
        #distributiondata$log.2.loc[round] = logoutput2$estimate[1]
        #distributiondata$log.2.scale[round] = logoutput2$estimate[2]
        #distributiondata$log.3.aic[round] = logoutput3$aic
        #distributiondata$log.3.loc[round] = logoutput3$estimate[1]
        #distributiondata$log.3.scale[round] = logoutput3$estimate[2]
        
        ##test against each other
        #testdistribution = gofstat(list(normaloutput, expoutput, gammaoutput,
        #                                lognormoutput, betaoutput, chioutput,
        #                                foutput, logoutput),
        #                           fitnames = c("normal", "exponential", "gamma",
        #                                        "log normal", "beta", "chi square",
        #                                        "f distribution", "logistic")) 
        testdistribution2 = gofstat(list(normaloutput2, expoutput2, gammaoutput2,
                                         lognormoutput2, betaoutput2, chioutput2,
                                         foutput2),
                                    fitnames = c("normal", "exponential", "gamma",
                                                 "log normal", "beta", "chi square",
                                                 "f distribution"))
        testdistribution3 = gofstat(list(normaloutput3, expoutput3, gammaoutput3,
                                         lognormoutput3, betaoutput3, chioutput3,
                                         foutput3),
                                    fitnames = c("normal", "exponential", "gamma",
                                                 "log normal", "beta", "chi square",
                                                 "f distribution"))
        
        #numbers = c(testdistribution$ks, testdistribution$kstest, testdistribution$cvm,
        #            testdistribution$cvmtest, testdistribution$ad, testdistribution$adtest)
        
        numbers2 = c(testdistribution2$ks, testdistribution2$kstest, testdistribution2$cvm,
                     testdistribution2$cvmtest, testdistribution2$ad, testdistribution2$adtest)

        numbers3 = c(testdistribution3$ks, testdistribution3$kstest, testdistribution3$cvm,
                     testdistribution3$cvmtest, testdistribution3$ad, testdistribution3$adtest)
        
        #testoutput = rbind(testoutput, numbers)
        testoutput2 = rbind(testoutput2, numbers2)
        testoutput3 = rbind(testoutput3, numbers3)
        
      }
    }
  }
}





