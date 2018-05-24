
##load sim data
library(readr)
library(data.table)
library(mvtnorm)
library(ez)
library(reshape)
library(readr)
data <- read_csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/ES Variance Comparison Project/mydata.csv", 
                 col_types = cols(DFd = col_number(), 
                                  DFn = col_number(), Fvalue = col_number(), 
                                  N = col_number(), RM1.ges = col_number(), 
                                  SSd = col_number(), SSn = col_number(), 
                                  X1 = col_number(), pvalue = col_number(), 
                                  simnum = col_number(), stdev = col_number()))
data = as.data.frame(data)

options(scipen=999)
#recalculate p values to see if it matches earlier output
data$pvalue.new = pf(data$Fvalue, data$DFn, data$DFd, lower.tail = F)

smalldata = subset(data, stdev==5)
mediumdata = subset(data, stdev==3)
largedata = subset(data, stdev==1)



################################################################ Variance functions
## simulating function
SamplingVariance = function(Size, N){
  Size = as.numeric(Size)
  N = as.numeric(N)
  if((N%%2)!=0){
    N = N-1
  }
  numsim = 1000 #check this later
  mydata = as.data.table(matrix(ncol=2, nrow=numsim))
  colnames(mydata)=c("dfr","ES")
  Sizex = 4.623546 + ((-9.882827)*Size)
  if(Sizex == 0){
    Sizex = 0.01
  } else if(Sizex < 0){
    Sizex = 0.01
  }
  simx=0
  for (i in 1:numsim){
    simx=simx+1
    #make datasets
    sigma = matrix(c(Sizex,0,0,
                     0,Sizex,0,
                     0,0,Sizex), nrow = 3, ncol = 3)
    Means = c(3.5,4,4.5)
    dataset = as.data.table(rmvnorm(N, Means, sigma))
    dataset = round(dataset, digits = 0)
    dataset[ dataset < 1 ] = 1
    dataset[ dataset > 7 ] = 7
    dataset$partno = 1:nrow(dataset)
    dataset$partno = factor(dataset$partno)
    long = melt(dataset,
                id = "partno",
                measured = colnames(long[1:(ncol(long)-1), ]))
    colnames(long)=c("partno","variable","value")
    #### end make datasets
    #### RM1 stats
    rmoutput = ezANOVA(data = long,
                       wid = partno,
                       within = variable,
                       dv = value,
                       type = 3, 
                       detailed = T)
    mydata$dfr[simx] = rmoutput$ANOVA$DFd[2]
    mydata$ES[simx] = rmoutput$ANOVA$ges[2]
  } #end for loop
  var1 = sum((mydata$ES-mean(mydata$ES))^2)/(N-1)
  var2 = sum((mydata$ES-mean(mydata$ES))^2)/mydata$dfr[1]
  if(var1<var2){
    varlo = var2
  } else{
    varlo = var1
  }
  return(varlo)
} #end function

## eta to d variance function
EtaToD = function(Size, N){
  Size = as.numeric(Size)
  N = as.numeric(N)
  r = sqrt(Size)
  d = (2*r)/sqrt(1-Size)
  Vr = ((1-Size)^2)/(N-1)
  Vd = (4*Vr)/((1-Size)^3)
  return(Vd)
}

## d to eta function effect size conversion
DBacktoEta = function(d){
  d = as.numeric(d)
  EtA = (d/sqrt((d^2)+4))^2
  return(EtA)
}

## eta to r to z variance function
EtaToZ = function(Size, N){
  Size = as.numeric(Size)
  N = as.numeric(N)
  r = sqrt(Size)
  z = 0.5*log((1+r)/(1-r))
  Vz = 1/(N-3)
  return(Vz)
}

## eta to z effect size translation
EtaToZEffect = function(Size){
  Size = as.numeric(Size)
  r = sqrt(Size)
  z = 0.5*log((1+r)/(1-r))
  return(z)
}

#translating z back to eta effect size conversion
z2e = function(z){
  z = as.numeric(z)
  e = ((exp(z/0.5)-1)/(exp(z/0.5)+1))^2
  return(e)
}

## eta to d effect size conversion
EtaToDEffect = function(Size){
  Size = as.numeric(Size)
  r = sqrt(Size)
  d = (2*r)/sqrt(1-Size)
  return(d)
}
################################################################ Variance functions





############################################################### Comparing variance techniques with average example
eta = unname(tapply(smalldata$RM1.ges, list(smalldata$N), mean))
avg_data = as.data.frame(eta)
popvar = unname(tapply(smalldata$RM1.ges, list(smalldata$N), var))
avg_data$popvar = popvar
avg_data$size = "Small"
avg_data$N = seq(10,100,10)

eta = unname(tapply(mediumdata$RM1.ges, list(mediumdata$N), mean))
temp = as.data.frame(eta)
popvar = unname(tapply(mediumdata$RM1.ges, list(mediumdata$N), var))
temp$popvar = popvar
temp$size = "Medium"
temp$N = seq(10,100,10)
avg_data = rbind(avg_data, temp)

eta = unname(tapply(largedata$RM1.ges, list(largedata$N), mean))
temp = as.data.frame(eta)
popvar = unname(tapply(largedata$RM1.ges, list(largedata$N), var))
temp$popvar = popvar
temp$size = "Large"
temp$N = seq(10,100,10)
avg_data = rbind(avg_data, temp)
avg_data = avg_data[c(3,4,1,2)]

## 1 way RM ANOVA 3 levels

avg_data$E2Dvar = "NA"
avg_data$E2Zvar = "NA"
avg_data$Esimvar = "NA"
round = 0
nsim = as.numeric(nrow(avg_data))

for(i in 1:nsim){
  round = round+1
  x_size = avg_data$eta[round]
  x_N = avg_data$N[round]
  
  avg_data$E2Dvar[round] = EtaToD(Size = x_size, N = x_N)
  avg_data$E2Zvar[round] = EtaToZ(Size = x_size, N = x_N)
  avg_data$Esimvar[round] = SamplingVariance(Size = x_size, N = x_N)
  
}
############################################################### Comparing variance techniques with average example

setwd("C:/Users/John/Desktop")
write.csv(avg_data, "avg_data.csv")


avg_data <- read_csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/ES Variance Comparison Project/avg_data.csv", 
                     col_types = cols(E2Dvar = col_number(), 
                                      E2Zvar = col_number(), Esimvar = col_number(), 
                                      eta = col_number(), popvar = col_number()))


#set working directory

# Start the clock!
ptm <- proc.time()

############################################################## 50 small meta no pub bias
mean(smalldata$RM1.ges) # .05

totsims = 50
meta_small_nobias = data.table(sim = 1:totsims,
                               eta_d = 1:totsims,
                               eta_z = 1:totsims,
                               eta_sim = 1:totsims)
biground=0

#start loop
for(i in 1:totsims){
  meta_pull = data.frame(N = 1:50, RM1.ges = 1:50, pvalue = 1:50)
  x = sample(5001:50000,50, replace = F)
  biground=biground+1
  round = 0
  pullsim= 50
  
  for(i in 1:pullsim){
    round=round+1
    meta_pull[round, 1] = as.numeric(smalldata[x[round], 2])
    meta_pull[round, 2] = as.numeric(smalldata[x[round], 5])
    meta_pull[round, 3] = as.numeric(smalldata[x[round], 12])
  }
  
  ## eta to d
  meta_pull$d = "NA"
  round = 0
  nsim = as.numeric(nrow(meta_pull))
  for(i in 1:nsim){
    round=round+1
    meta_pull$d[round] = EtaToDEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$d = as.numeric(meta_pull$d)
  ## eta to d variance
  meta_pull$dVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$dVarWeight[round] = 1/(EtaToD(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$dVarWeight = as.numeric(meta_pull$dVarWeight)
  #d times inverse variance
  meta_pull$DVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$DVarprod[round] = meta_pull$d[round]*meta_pull$dVarWeight[round]
  }
  meta_pull$DVarprod = as.numeric(meta_pull$DVarprod)
  
  ##eta to z
  meta_pull$z = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$z[round] = EtaToZEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$z = as.numeric(meta_pull$z)
  ## eta to z variance
  meta_pull$zVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$zVarWeight[round] = 1/(EtaToZ(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$zVarWeight = as.numeric(meta_pull$zVarWeight)
  #z times inverse variance
  meta_pull$ZVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$ZVarprod[round] = meta_pull$z[round]*meta_pull$zVarWeight[round]
  }
  meta_pull$ZVarprod = as.numeric(meta_pull$ZVarprod)
  
  # sims var weight
  meta_pull$simvarweight = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$simvarweight[round] = 1/ SamplingVariance(meta_pull$RM1.ges[round], meta_pull$N[round])
  }
  meta_pull$simvarweight = as.numeric(meta_pull$simvarweight)
  #eta times inverse variance
  meta_pull$simVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$simVarprod[round] = meta_pull$RM1.ges[round]*meta_pull$simvarweight[round]
  }
  meta_pull$simVarprod = as.numeric(meta_pull$simVarprod)
  
  #meta on d
  dmet = sum(meta_pull$DVarprod)/sum(meta_pull$dVarWeight)
  meta_small_nobias$eta_d[biground] = DBacktoEta(dmet)
  
  #meta on z
  zmet = sum(meta_pull$ZVarprod)/sum(meta_pull$zVarWeight)
  meta_small_nobias$eta_z[biground] = z2e(zmet)
  
  #meta on eta sim
  simmet = sum(meta_pull$simVarprod)/sum(meta_pull$simvarweight)
  meta_small_nobias$eta_sim[biground] = simmet
  
} # end loop

write.csv(meta_small_nobias, "meta_small_nobias.csv")
############################################################## 50 small meta no pub bias



############################################################# 50 small meta with pub bias
totsims = 50
meta_small_pubbias = data.table(sim = 1:totsims,
                               eta_d = 1:totsims,
                               eta_z = 1:totsims,
                               eta_sim = 1:totsims)
biground=0

#start loop
for(i in 1:totsims){
  meta_pull = data.frame(N = 1:50, RM1.ges = 1:50, pvalue = 1:50)
  smallpubdata = subset(smalldata, pvalue < .05)
  smallpubdata.2 = subset(smallpubdata, N > 10)
  x = sample(smallpubdata.2$X1, 50, replace = F)
  biground=biground+1
  round = 0
  pullsim= 50
  
  for(i in 1:pullsim){
    round=round+1
    meta_pull[round, 1] = as.numeric(smallpubdata.2[which(smallpubdata.2$X1 == x[round]), 2])
    meta_pull[round, 2] = as.numeric(smallpubdata.2[which(smallpubdata.2$X1 == x[round]), 5])
    meta_pull[round, 3] = as.numeric(smallpubdata.2[which(smallpubdata.2$X1 == x[round]), 12])
  }
  
  ## eta to d
  meta_pull$d = "NA"
  round = 0
  nsim = as.numeric(nrow(meta_pull))
  for(i in 1:nsim){
    round=round+1
    meta_pull$d[round] = EtaToDEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$d = as.numeric(meta_pull$d)
  ## eta to d variance
  meta_pull$dVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$dVarWeight[round] = 1/(EtaToD(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$dVarWeight = as.numeric(meta_pull$dVarWeight)
  #d times inverse variance
  meta_pull$DVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$DVarprod[round] = meta_pull$d[round]*meta_pull$dVarWeight[round]
  }
  meta_pull$DVarprod = as.numeric(meta_pull$DVarprod)
  
  ##eta to z
  meta_pull$z = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$z[round] = EtaToZEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$z = as.numeric(meta_pull$z)
  ## eta to z variance
  meta_pull$zVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$zVarWeight[round] = 1/(EtaToZ(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$zVarWeight = as.numeric(meta_pull$zVarWeight)
  #z times inverse variance
  meta_pull$ZVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$ZVarprod[round] = meta_pull$z[round]*meta_pull$zVarWeight[round]
  }
  meta_pull$ZVarprod = as.numeric(meta_pull$ZVarprod)
  
  # sims var weight
  meta_pull$simvarweight = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$simvarweight[round] = 1/ SamplingVariance(meta_pull$RM1.ges[round], meta_pull$N[round])
  }
  meta_pull$simvarweight = as.numeric(meta_pull$simvarweight)
  #eta times inverse variance
  meta_pull$simVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$simVarprod[round] = meta_pull$RM1.ges[round]*meta_pull$simvarweight[round]
  }
  meta_pull$simVarprod = as.numeric(meta_pull$simVarprod)
  
  #meta on d
  dmet = sum(meta_pull$DVarprod)/sum(meta_pull$dVarWeight)
  meta_small_pubbias$eta_d[biground] = DBacktoEta(dmet)
  
  #meta on z
  zmet = sum(meta_pull$ZVarprod)/sum(meta_pull$zVarWeight)
  meta_small_pubbias$eta_z[biground] = z2e(zmet)
  
  #meta on eta sim
  simmet = sum(meta_pull$simVarprod)/sum(meta_pull$simvarweight)
  meta_small_pubbias$eta_sim[biground] = simmet
  
} # end loop

write.csv(meta_small_pubbias, "meta_small_pubbias.csv")
############################################################# 50 small meta with pub bias




############################################################## 50 medium meta no pub bias
mean(mediumdata$RM1.ges) # .07

totsims = 50
meta_medium_nobias = data.table(sim = 1:totsims,
                               eta_d = 1:totsims,
                               eta_z = 1:totsims,
                               eta_sim = 1:totsims)
biground=0

#start loop
for(i in 1:totsims){
  meta_pull = data.frame(N = 1:50, RM1.ges = 1:50, pvalue = 1:50)
  x = sample(5001:50000,50, replace = F)
  biground=biground+1
  round = 0
  pullsim= 50
  
  for(i in 1:pullsim){
    round=round+1
    meta_pull[round, 1] = as.numeric(mediumdata[x[round], 2])
    meta_pull[round, 2] = as.numeric(mediumdata[x[round], 5])
    meta_pull[round, 3] = as.numeric(mediumdata[x[round], 12])
  }
  
  ## eta to d
  meta_pull$d = "NA"
  round = 0
  nsim = as.numeric(nrow(meta_pull))
  for(i in 1:nsim){
    round=round+1
    meta_pull$d[round] = EtaToDEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$d = as.numeric(meta_pull$d)
  ## eta to d variance
  meta_pull$dVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$dVarWeight[round] = 1/(EtaToD(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$dVarWeight = as.numeric(meta_pull$dVarWeight)
  #d times inverse variance
  meta_pull$DVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$DVarprod[round] = meta_pull$d[round]*meta_pull$dVarWeight[round]
  }
  meta_pull$DVarprod = as.numeric(meta_pull$DVarprod)
  
  ##eta to z
  meta_pull$z = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$z[round] = EtaToZEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$z = as.numeric(meta_pull$z)
  ## eta to z variance
  meta_pull$zVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$zVarWeight[round] = 1/(EtaToZ(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$zVarWeight = as.numeric(meta_pull$zVarWeight)
  #z times inverse variance
  meta_pull$ZVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$ZVarprod[round] = meta_pull$z[round]*meta_pull$zVarWeight[round]
  }
  meta_pull$ZVarprod = as.numeric(meta_pull$ZVarprod)
  
  # sims var weight
  meta_pull$simvarweight = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$simvarweight[round] = 1/ SamplingVariance(meta_pull$RM1.ges[round], meta_pull$N[round])
  }
  meta_pull$simvarweight = as.numeric(meta_pull$simvarweight)
  #eta times inverse variance
  meta_pull$simVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$simVarprod[round] = meta_pull$RM1.ges[round]*meta_pull$simvarweight[round]
  }
  meta_pull$simVarprod = as.numeric(meta_pull$simVarprod)
  
  #meta on d
  dmet = sum(meta_pull$DVarprod)/sum(meta_pull$dVarWeight)
  meta_medium_nobias$eta_d[biground] = DBacktoEta(dmet)
  
  #meta on z
  zmet = sum(meta_pull$ZVarprod)/sum(meta_pull$zVarWeight)
  meta_medium_nobias$eta_z[biground] = z2e(zmet)
  
  #meta on eta sim
  simmet = sum(meta_pull$simVarprod)/sum(meta_pull$simvarweight)
  meta_medium_nobias$eta_sim[biground] = simmet
  
} # end loop

write.csv(meta_medium_nobias, "meta_medium_nobias.csv")
############################################################## 50 medium meta no pub bias



############################################################# 50 medium meta with pub bias
totsims = 50
meta_medium_pubbias = data.table(sim = 1:totsims,
                                eta_d = 1:totsims,
                                eta_z = 1:totsims,
                                eta_sim = 1:totsims)
biground=0

#start loop
for(i in 1:totsims){
  meta_pull = data.frame(N = 1:50, RM1.ges = 1:50, pvalue = 1:50)
  mediumpubdata = subset(mediumdata, pvalue < .05)
  mediumpubdata.2 = subset(mediumpubdata, N > 10)
  x = sample(mediumpubdata.2$X1, 50, replace = F)
  biground=biground+1
  round = 0
  pullsim= 50
  
  for(i in 1:pullsim){
    round=round+1
    meta_pull[round, 1] = as.numeric(mediumpubdata.2[which(mediumpubdata.2$X1 == x[round]), 2])
    meta_pull[round, 2] = as.numeric(mediumpubdata.2[which(mediumpubdata.2$X1 == x[round]), 5])
    meta_pull[round, 3] = as.numeric(mediumpubdata.2[which(mediumpubdata.2$X1 == x[round]), 12])
  }
  
  ## eta to d
  meta_pull$d = "NA"
  round = 0
  nsim = as.numeric(nrow(meta_pull))
  for(i in 1:nsim){
    round=round+1
    meta_pull$d[round] = EtaToDEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$d = as.numeric(meta_pull$d)
  ## eta to d variance
  meta_pull$dVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$dVarWeight[round] = 1/(EtaToD(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$dVarWeight = as.numeric(meta_pull$dVarWeight)
  #d times inverse variance
  meta_pull$DVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$DVarprod[round] = meta_pull$d[round]*meta_pull$dVarWeight[round]
  }
  meta_pull$DVarprod = as.numeric(meta_pull$DVarprod)
  
  ##eta to z
  meta_pull$z = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$z[round] = EtaToZEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$z = as.numeric(meta_pull$z)
  ## eta to z variance
  meta_pull$zVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$zVarWeight[round] = 1/(EtaToZ(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$zVarWeight = as.numeric(meta_pull$zVarWeight)
  #z times inverse variance
  meta_pull$ZVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$ZVarprod[round] = meta_pull$z[round]*meta_pull$zVarWeight[round]
  }
  meta_pull$ZVarprod = as.numeric(meta_pull$ZVarprod)
  
  # sims var weight
  meta_pull$simvarweight = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$simvarweight[round] = 1/ SamplingVariance(meta_pull$RM1.ges[round], meta_pull$N[round])
  }
  meta_pull$simvarweight = as.numeric(meta_pull$simvarweight)
  #eta times inverse variance
  meta_pull$simVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$simVarprod[round] = meta_pull$RM1.ges[round]*meta_pull$simvarweight[round]
  }
  meta_pull$simVarprod = as.numeric(meta_pull$simVarprod)
  
  #meta on d
  dmet = sum(meta_pull$DVarprod)/sum(meta_pull$dVarWeight)
  meta_medium_pubbias$eta_d[biground] = DBacktoEta(dmet)
  
  #meta on z
  zmet = sum(meta_pull$ZVarprod)/sum(meta_pull$zVarWeight)
  meta_medium_pubbias$eta_z[biground] = z2e(zmet)
  
  #meta on eta sim
  simmet = sum(meta_pull$simVarprod)/sum(meta_pull$simvarweight)
  meta_medium_pubbias$eta_sim[biground] = simmet
  
} # end loop

write.csv(meta_medium_pubbias, "meta_medium_pubbias.csv")
############################################################# 50 medium meta with pub bias




############################################################## 50 large meta no pub bias
mean(largedata$RM1.ges) # .15

totsims = 50
meta_large_nobias = data.table(sim = 1:totsims,
                               eta_d = 1:totsims,
                               eta_z = 1:totsims,
                               eta_sim = 1:totsims)
biground=0

#start loop
for(i in 1:totsims){
  meta_pull = data.frame(N = 1:50, RM1.ges = 1:50, pvalue = 1:50)
  x = sample(5001:50000,50, replace = F)
  biground=biground+1
  round = 0
  pullsim= 50
  
  for(i in 1:pullsim){
    round=round+1
    meta_pull[round, 1] = as.numeric(largedata[x[round], 2])
    meta_pull[round, 2] = as.numeric(largedata[x[round], 5])
    meta_pull[round, 3] = as.numeric(largedata[x[round], 12])
  }
  
  ## eta to d
  meta_pull$d = "NA"
  round = 0
  nsim = as.numeric(nrow(meta_pull))
  for(i in 1:nsim){
    round=round+1
    meta_pull$d[round] = EtaToDEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$d = as.numeric(meta_pull$d)
  ## eta to d variance
  meta_pull$dVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$dVarWeight[round] = 1/(EtaToD(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$dVarWeight = as.numeric(meta_pull$dVarWeight)
  #d times inverse variance
  meta_pull$DVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$DVarprod[round] = meta_pull$d[round]*meta_pull$dVarWeight[round]
  }
  meta_pull$DVarprod = as.numeric(meta_pull$DVarprod)
  
  ##eta to z
  meta_pull$z = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$z[round] = EtaToZEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$z = as.numeric(meta_pull$z)
  ## eta to z variance
  meta_pull$zVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$zVarWeight[round] = 1/(EtaToZ(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$zVarWeight = as.numeric(meta_pull$zVarWeight)
  #z times inverse variance
  meta_pull$ZVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$ZVarprod[round] = meta_pull$z[round]*meta_pull$zVarWeight[round]
  }
  meta_pull$ZVarprod = as.numeric(meta_pull$ZVarprod)
  
  # sims var weight
  meta_pull$simvarweight = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$simvarweight[round] = 1/ SamplingVariance(meta_pull$RM1.ges[round], meta_pull$N[round])
  }
  meta_pull$simvarweight = as.numeric(meta_pull$simvarweight)
  #eta times inverse variance
  meta_pull$simVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$simVarprod[round] = meta_pull$RM1.ges[round]*meta_pull$simvarweight[round]
  }
  meta_pull$simVarprod = as.numeric(meta_pull$simVarprod)
  
  #meta on d
  dmet = sum(meta_pull$DVarprod)/sum(meta_pull$dVarWeight)
  meta_large_nobias$eta_d[biground] = DBacktoEta(dmet)
  
  #meta on z
  zmet = sum(meta_pull$ZVarprod)/sum(meta_pull$zVarWeight)
  meta_large_nobias$eta_z[biground] = z2e(zmet)
  
  #meta on eta sim
  simmet = sum(meta_pull$simVarprod)/sum(meta_pull$simvarweight)
  meta_large_nobias$eta_sim[biground] = simmet
  
} # end loop

write.csv(meta_large_nobias, "meta_large_nobias.csv")
############################################################## 50 large meta no pub bias



############################################################# 50 large meta with pub bias
totsims = 50
meta_large_pubbias = data.table(sim = 1:totsims,
                                eta_d = 1:totsims,
                                eta_z = 1:totsims,
                                eta_sim = 1:totsims)
biground=0

#start loop
for(i in 1:totsims){
  meta_pull = data.frame(N = 1:50, RM1.ges = 1:50, pvalue = 1:50)
  largepubdata = subset(largedata, pvalue < .05)
  largepubdata.2 = subset(largepubdata, N > 10)
  x = sample(largepubdata.2$X1, 50, replace = F)
  biground=biground+1
  round = 0
  pullsim= 50
  
  for(i in 1:pullsim){
    round=round+1
    meta_pull[round, 1] = as.numeric(largepubdata.2[which(largepubdata.2$X1 == x[round]), 2])
    meta_pull[round, 2] = as.numeric(largepubdata.2[which(largepubdata.2$X1 == x[round]), 5])
    meta_pull[round, 3] = as.numeric(largepubdata.2[which(largepubdata.2$X1 == x[round]), 12])
  }
  
  ## eta to d
  meta_pull$d = "NA"
  round = 0
  nsim = as.numeric(nrow(meta_pull))
  for(i in 1:nsim){
    round=round+1
    meta_pull$d[round] = EtaToDEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$d = as.numeric(meta_pull$d)
  ## eta to d variance
  meta_pull$dVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$dVarWeight[round] = 1/(EtaToD(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$dVarWeight = as.numeric(meta_pull$dVarWeight)
  #d times inverse variance
  meta_pull$DVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$DVarprod[round] = meta_pull$d[round]*meta_pull$dVarWeight[round]
  }
  meta_pull$DVarprod = as.numeric(meta_pull$DVarprod)
  
  ##eta to z
  meta_pull$z = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$z[round] = EtaToZEffect(meta_pull$RM1.ges[round])
  }
  meta_pull$z = as.numeric(meta_pull$z)
  ## eta to z variance
  meta_pull$zVarWeight = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$zVarWeight[round] = 1/(EtaToZ(meta_pull$RM1.ges[round],meta_pull$N[round]))
  }
  meta_pull$zVarWeight = as.numeric(meta_pull$zVarWeight)
  #z times inverse variance
  meta_pull$ZVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$ZVarprod[round] = meta_pull$z[round]*meta_pull$zVarWeight[round]
  }
  meta_pull$ZVarprod = as.numeric(meta_pull$ZVarprod)
  
  # sims var weight
  meta_pull$simvarweight = "NA"
  round = 0
  for(i in 1:nsim){
    round=round+1
    meta_pull$simvarweight[round] = 1/ SamplingVariance(meta_pull$RM1.ges[round], meta_pull$N[round])
  }
  meta_pull$simvarweight = as.numeric(meta_pull$simvarweight)
  #eta times inverse variance
  meta_pull$simVarprod = "NA"
  round = 0
  for(i in 1:nsim){
    round = round+1
    meta_pull$simVarprod[round] = meta_pull$RM1.ges[round]*meta_pull$simvarweight[round]
  }
  meta_pull$simVarprod = as.numeric(meta_pull$simVarprod)
  
  #meta on d
  dmet = sum(meta_pull$DVarprod)/sum(meta_pull$dVarWeight)
  meta_large_pubbias$eta_d[biground] = DBacktoEta(dmet)
  
  #meta on z
  zmet = sum(meta_pull$ZVarprod)/sum(meta_pull$zVarWeight)
  meta_large_pubbias$eta_z[biground] = z2e(zmet)
  
  #meta on eta sim
  simmet = sum(meta_pull$simVarprod)/sum(meta_pull$simvarweight)
  meta_large_pubbias$eta_sim[biground] = simmet
  
} # end loop

write.csv(meta_large_pubbias, "meta_large_pubbias.csv")
############################################################# 50 large meta with pub bias











# Stop the clock
proc.time() - ptm

