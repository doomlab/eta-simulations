setwd("C:/Users/eri2005/Desktop")

####simulations for data####
library(mvtnorm)
library(ez)
library(reshape)
##rmvnorm(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
##        method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE)

####create blank data from for data####
totalsims = 3*4*3*3*1000
mydata = data.frame(N = 1:totalsims)

####keep track of the simulation rounds####
round_sim = 0

####loop over N values here####
##loop a
Nloops = c(25, 50, 100)
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
corloops = c(0, .5, .9)

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
      round_sim = round_sim + 1
      simulate = simulate + 1
      mydata$N[round_sim] = Nloops[a]
      mydata$levels[round_sim] = levels[b]
      mydata$stdev[round_sim] = SDloops[c]
      mydata$correl[round_sim] = corloops[d]
      mydata$simnum[round_sim] = simulate
    
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
      
      mydata$RM1.dfm[round_sim] = rmoutput$ANOVA$DFn[2]
      mydata$RM1.dfr[round_sim] = rmoutput$ANOVA$DFd[2]
      mydata$RM1.ssm.p[round_sim] = rmoutput$ANOVA$SSn[1]
      mydata$RM1.ssm.main[round_sim] = rmoutput$ANOVA$SSn[2]
      mydata$RM1.ssr.p[round_sim] = rmoutput$ANOVA$SSd[1]
      mydata$RM1.ssr.main[round_sim] = rmoutput$ANOVA$SSd[2]
      mydata$RM1.F[round_sim] = rmoutput$ANOVA$F[2]
      mydata$RM1.ges[round_sim] = rmoutput$ANOVA$ges[2]
      mydata$RM1.gg[round_sim] = rmoutput$`Sphericity Corrections`$GGe
      mydata$RM1.hf[round_sim] = rmoutput$`Sphericity Corrections`$HFe
      mydata$RM1.W[round_sim] = rmoutput$`Mauchly's Test for Sphericity`$W
      mydata$RM1.Wp[round_sim] = rmoutput$`Mauchly's Test for Sphericity`$p
      
} ##end sim loop

  
  filename = paste(round, ".csv", sep = "")
  datalines = (abs(round-999):round)
  write.csv(mydata[ datalines, ], file = filename)
  
      
} ##end N loop

} ##end levels loop

} ##end SD loop

} ##end cor loop

