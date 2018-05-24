
library(mvtnorm)
library(ez)
library(reshape)
library(data.table)

totalsims = 30*5000
mydata = data.table(N = 1:totalsims)

round = 0
Means = c(3.5, 4.0, 4.5)  

##loop a
Nloops = seq(10, 100, 10)

for (a in 1:length(Nloops)) {
  simulate = 0
  
  SDloops = c(5, 3, 1)
  for (b in 1:length(SDloops)) {
    
    ####simulate 5,000 rounds of data####
    for (c in 1:5000) {
      sigma = matrix(c(SDloops[b],0,0,
                       0,SDloops[b],0,
                       0,0,SDloops[b]), nrow = 3, ncol = 3)
      
      dataset = as.data.table(rmvnorm(Nloops[a], Means, sigma))
      dataset = round(dataset, digits = 0)
      dataset[ dataset < 1 ] = 1
      dataset[ dataset > 7 ] = 7
      round = round + 1
      simulate = simulate + 1
      
      mydata$N[round] = Nloops[a]
      mydata$stdev[round] = SDloops[b]
      mydata$simnum[round] = simulate
      
      ####begin RM ANOVA one way####
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
      mydata$RM1.ges[round] = rmoutput$ANOVA$ges[2]
      mydata$pvalue[round] = rmoutput$ANOVA$p[2]
      mydata$Fvalue[round] = rmoutput$ANOVA$F[2]
      mydata$SSn[round] = rmoutput$ANOVA$SSn[2]
      mydata$SSd[round] = rmoutput$ANOVA$SSd[2]
      mydata$DFn[round] = rmoutput$ANOVA$DFn[2]
      mydata$DFd[round] = rmoutput$ANOVA$DFd[2]
      
    } # end sim
    
  } # end SD
  
} # end N


#working directory
write.csv(mydata, "mydata.csv")
