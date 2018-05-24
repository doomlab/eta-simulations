setwd("C:/Users/eri2005/Desktop")

####simulations for data####
library(mvtnorm)
library(ez)
library(reshape)
##rmvnorm(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
##        method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE)

####things to simulate####
##rotate around N values start at 20 in each level, then add 5 as you go
##keep means steady 2.5, 3, 3.5, etc.
##rotate around the number of levels 3-6
##rotate around SDs making eta small medium large
##rotate around correlated error 0, .1, 3., .5, .7, .9

####create blank data from for data####
totalsims = 1224*1000
mydata = data.frame(N = 1:totalsims)

####keep track of the simulation rounds####
round = 0

####loop over N values here####
##loop a
Nloops = seq(20, 100, 6)
for (a in 1:length(Nloops)) {
  
simulate = 0

####loop over M and levels values here####
##this loop will give you different numbers of means for the different number of levels
##loops b
levels = 3:6 

for (b in 1:length(levels)) {

topmean = c(3.5, 4.0, 4.5, 5)  
Means = seq(2.5, topmean[b], .5)


####loop over SD values here####
##loop c
SDloops = c(5, 3, 1)

for (c in 1:length(SDloops)) {

####loop over correlations here####
##loop d
corloops = c(0, .1, .3, .5, .7, .9)

for (d in 1:length(corloops)) {

####simulate 1000 rounds of data####

for (e in 1:1000) {
      ####make the data here####
      ##here we are going to want to make the cor / SD matrix pattern
      ##but that will depends on the number of levels 
      ##might have to do it by if statements? not very elegant

      if(levels[b] == 3) {
      sigma = matrix(c(SDloops[c],corloops[d],corloops[d],
                       corloops[d],SDloops[c],corloops[d],
                       corloops[d],corloops[d],SDloops[c]), nrow = 3, ncol = 3)
      }

      if(levels[b] == 4) {
      sigma = matrix(c(SDloops[c],corloops[d],corloops[d],corloops[d],
                   corloops[d],SDloops[c],corloops[d],corloops[d],
                   corloops[d],corloops[d],SDloops[c],corloops[d],
                   corloops[d],corloops[d],corloops[d],SDloops[c]), nrow = 4, ncol = 4)
      }

      if(levels[b] == 5) {
      sigma = matrix(c(SDloops[c],corloops[d],corloops[d],corloops[d],corloops[d],
                   corloops[d],SDloops[c],corloops[d],corloops[d],corloops[d],
                   corloops[d],corloops[d],SDloops[c],corloops[d],corloops[d],
                   corloops[d],corloops[d],corloops[d],SDloops[c],corloops[d],
                   corloops[d],corloops[d],corloops[d],corloops[d],SDloops[c]), nrow = 5, ncol = 5)
      }

      if(levels[b] == 6) {
      sigma = matrix(c(SDloops[c],corloops[d],corloops[d],corloops[d],corloops[d],corloops[d],
                   corloops[d],SDloops[c],corloops[d],corloops[d],corloops[d],corloops[d],
                   corloops[d],corloops[d],SDloops[c],corloops[d],corloops[d],corloops[d],
                   corloops[d],corloops[d],corloops[d],SDloops[c],corloops[d],corloops[d],
                   corloops[d],corloops[d],corloops[d],corloops[d],SDloops[c],corloops[d],
                   corloops[d],corloops[d],corloops[d],corloops[d],corloops[d],SDloops[c]), nrow = 6, ncol = 6)
      }
    
      dataset = rmvnorm(Nloops[a], Means, sigma)
      
      ##here we are simulating 1-7 Likert type data
      ##take off the digits
      ##take out the out of range values
      dataset = round(dataset, digits = 0)
      dataset[ dataset < 1 ] = 1
      dataset[ dataset > 7 ] = 7
      
      ####put in the basic statistics here####
      round = round + 1
      simulate = simulate + 1
      mydata$N[round] = Nloops[a]
      mydata$levels[round] = levels[b]
      mydata$stdev[round] = SDloops[c]
      mydata$correl[round] = corloops[d]
      mydata$simnum[round] = simulate
    
      ####begin RM ANOVA one way####
      dataset = as.data.frame(dataset)
      dataset$partno = as.factor(1:nrow(dataset))
      longdataset = melt(dataset,
                         id = "partno",
                         measured = colnames(longdataset[1:(ncol(longdataset)-1), ]))
      
      rmoutput = ezANOVA(data = longdataset,
                         wid = partno,
                         within = variable,
                         dv = value,
                         type = 3, 
                         detailed = T)
      
      mydata$RM1.dfm[round] = rmoutput$ANOVA$DFn[2]
      mydata$RM1.dfr[round] = rmoutput$ANOVA$DFd[2]
      mydata$RM1.ssm.p[round] = rmoutput$ANOVA$SSn[1]
      mydata$RM1.ssm.main[round] = rmoutput$ANOVA$SSn[2]
      mydata$RM1.ssr.p[round] = rmoutput$ANOVA$SSd[1]
      mydata$RM1.ssr.main[round] = rmoutput$ANOVA$SSd[2]
      mydata$RM1.F[round] = rmoutput$ANOVA$F[2]
      mydata$RM1.ges[round] = rmoutput$ANOVA$ges[2]
            
      ####begin RM ANOVA two way####
      tempstuff = longdataset
      randomvalues = rnorm(Nloops[a], mean(Means), 1)
      tempstuff$value = tempstuff$value + randomvalues
      
      tempstuff$value = round(tempstuff$value, digits = 0)
      tempstuff$value[  tempstuff$value < 1 ] = 1
      tempstuff$value[  tempstuff$value > 7 ] = 7
      
      doublermdata = rbind(longdataset, tempstuff)
      
      level1 = as.numeric(gl(2, Nloops[a]/2, nrow(longdataset), labels = c("1", "2")))
      level2 = 3 - as.numeric(gl(2, Nloops[a]/2, nrow(longdataset), labels = c("2", "1")))
      
      doublermdata$rmlevel2 = as.factor(c(level1,level2))
      
      rmoutput2 = ezANOVA(data = doublermdata,
                            wid = partno,
                            within = .(variable, rmlevel2),
                            dv = value,
                            type = 3,
                            detailed = T)
      
      mydata$RM2.dfm[round] = rmoutput2$ANOVA$DFn[2]
      mydata$RM2.dfr[round] = rmoutput2$ANOVA$DFd[2]
      mydata$RM2.ssm.p[round] = rmoutput2$ANOVA$SSn[1]
      mydata$RM2.ssm.main[round] = rmoutput2$ANOVA$SSn[2]
      mydata$RM2.ssm.other[round] = rmoutput2$ANOVA$SSn[3]
      mydata$RM2.ssm.interact[round] = rmoutput2$ANOVA$SSn[4]
      mydata$RM2.ssr.p[round] = rmoutput2$ANOVA$SSd[1]
      mydata$RM2.ssr.main[round] = rmoutput2$ANOVA$SSd[2]
      mydata$RM2.ssr.other[round] = rmoutput2$ANOVA$SSd[3]
      mydata$RM2.ssr.interact[round] = rmoutput2$ANOVA$SSd[4]
      mydata$RM2.F[round] = rmoutput2$ANOVA$F[2]
      mydata$RM2.ges[round] = rmoutput2$ANOVA$ges[2]
      
      ####begin MIXED ANOVA two way####
      longdataset$level2 = gl(2, Nloops[a]/2, nrow(longdataset), labels = c("level 1", "level 2"))
      
      mixedoutput = ezANOVA(data = longdataset,
                          wid = partno,
                          within = variable,
                          between = level2,
                          dv = value,
                          type = 3,
                          detailed = T)
      
      mydata$MIX.dfm[round] = mixedoutput$ANOVA$DFn[3]
      mydata$MIX.dfr[round] = mixedoutput$ANOVA$DFd[3]
      mydata$MIX.ssm.p[round] = mixedoutput$ANOVA$SSn[1]
      mydata$MIX.ssm.other[round] = mixedoutput$ANOVA$SSn[2]
      mydata$MIX.ssm.main[round] = mixedoutput$ANOVA$SSn[3]
      mydata$MIX.ssm.interact[round] = mixedoutput$ANOVA$SSn[4]
      mydata$MIX.ssr.p[round] = mixedoutput$ANOVA$SSd[1]
      mydata$MIX.ssr.other[round] = mixedoutput$ANOVA$SSd[2]
      mydata$MIX.ssr.main[round] = mixedoutput$ANOVA$SSd[3]
      mydata$MIX.ssr.interact[round] = mixedoutput$ANOVA$SSd[4]
      mydata$MIX.F[round] = mixedoutput$ANOVA$F[3]
      mydata$MIX.ges[round] = mixedoutput$ANOVA$ges[3]
      
      ####begin BN ANOVA one way####
      longdataset$bnpartno = as.factor(1:nrow(longdataset))
      bnoutput = ezANOVA(data = longdataset,
                         wid = bnpartno,
                         between = variable,
                         dv = value, 
                         type = 3, 
                         detailed = T)
      
      mydata$BN1.dfm[round] = bnoutput$ANOVA$DFn[2]
      mydata$BN1.dfr[round] = bnoutput$ANOVA$DFd[2]
      mydata$BN1.ssm.p[round] = bnoutput$ANOVA$SSn[1]
      mydata$BN1.ssm.main[round] = bnoutput$ANOVA$SSn[2]
      mydata$BN1.ssr.p[round] = bnoutput$ANOVA$SSd[1]
      mydata$BN1.ssr.main[round] = bnoutput$ANOVA$SSd[2]
      mydata$BN1.F[round] = bnoutput$ANOVA$F[2]
      mydata$BN1.ges[round] = bnoutput$ANOVA$ges[2]
       
      ####begin BN ANOVA two way####
      bnoutput2 = ezANOVA(data = longdataset,
                            wid = bnpartno,
                            between = .(variable, level2),
                            dv = value,
                            type = 3,
                            detailed = T)

      mydata$BN2.dfm[round] = bnoutput2$ANOVA$DFn[2]
      mydata$BN2.dfr[round] = bnoutput2$ANOVA$DFd[2]
      mydata$BN2.ssm.p[round] = bnoutput2$ANOVA$SSn[1]
      mydata$BN2.ssm.main[round] = bnoutput2$ANOVA$SSn[2]
      mydata$BN2.ssm.other[round] = bnoutput2$ANOVA$SSn[3]
      mydata$BN2.ssm.interact[round] = bnoutput2$ANOVA$SSn[4]
      mydata$BN2.ssr.all[round] = bnoutput2$ANOVA$SSd[1]
      mydata$BN2.F[round] = bnoutput2$ANOVA$F[2]
      mydata$BN2.ges[round] = bnoutput2$ANOVA$ges[2]
      
} ##end sim loop

  
  filename = paste(round, ".csv", sep = "")
  datalines = (abs(round-999):round)
  write.csv(mydata[ datalines, ], file = filename)
  
      
} ##end N loop

} ##end levels loop

} ##end SD loop

} ##end cor loop

