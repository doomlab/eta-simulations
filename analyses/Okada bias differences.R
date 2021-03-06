#Simulate bias in eta-squared, omega-squared, and epsilon-squared.
#R script by Kensuke Okada from: Okada, K. (2013). Is omega squared less biased? A comparison of three major effect size indices in one-way ANOVA. Behaviormetrika, 40(2), 129-147.

####okada's code run 1K times####
##code edited to only calculate eta2
muvec <- c(0.00,0.40,0.80,1.20) ##medium variability
meanmu <- mean(muvec)
sigb <- sum((muvec-meanmu)^2)/4
eta2p <- sigb/(sigb+1)
k <- length(muvec)
nsim <- 1000
njs <- c(10,20,30,40,50,60,70,80,90,100)
BIASmat <- matrix(NA,nrow=length(njs),ncol=1)
rownames(BIASmat) <- njs
colnames(BIASmat) <- c("eta2")
RMSEmat <- matrix(NA,nrow=length(njs),ncol=1)
rownames(RMSEmat) <- njs
colnames(RMSEmat) <- c("eta2")
SDmat <- matrix(NA,nrow=length(njs),ncol=1)
rownames(SDmat) <- njs
colnames(SDmat) <- c("eta2")
niter <- 1

for (nj in njs){
  x <- matrix(NA,nrow=nj,ncol=4)
  eta2 <- rep(NA,nsim)
  for (ii in 1: nsim){
    y <- c(rnorm(n=nj,mean=muvec[1],sd=1),
           rnorm(n=nj,mean=muvec[2] ,sd=1),
           rnorm(n=nj,mean=muvec[3] ,sd=1),
           rnorm(n=nj,mean=muvec[4] ,sd=1))
    x <- as.factor(c(rep("mu1",nj),rep("mu2",nj),
                     rep("mu3",nj),rep("mu4",nj)))
    res <- anova(aov(y~x))
    res <- as.matrix(res)
    SSb <- res[1,2]
    SSt <- res[1,2] + res[2,2]
    MSw <- res[2,3]
    eta2[ii] <- SSb/SSt
  }  
  BIASmat[niter,1] <- mean(eta2) - eta2p
  RMSEmat[niter,1] <- sqrt(sum((eta2-eta2p)^2)/nsim)
  SDmat[niter,1] <- sqrt(sum((eta2-mean(eta2))^2)/nsim)
  niter <- niter+1
}

####okada's code run 1K times, our means####
##code edited to only calculate eta2
muvec <- c(2.50, 3.00, 3.50, 4.00) 
meanmu <- mean(muvec)
sigb <- sum((muvec-meanmu)^2)/4
eta2p.2 <- sigb/(sigb+1)
k <- length(muvec)
nsim <- 1000
njs <- c(10,20,30,40,50,60,70,80,90,100)
BIASmat.2 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(BIASmat.2) <- njs
colnames(BIASmat.2) <- c("eta2")
RMSEmat.2 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(RMSEmat.2) <- njs
colnames(RMSEmat.2) <- c("eta2")
SDmat/2 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(SDmat.2) <- njs
colnames(SDmat.2) <- c("eta2")
niter <- 1

for (nj in njs){
  x <- matrix(NA,nrow=nj,ncol=4)
  eta2.2 <- rep(NA,nsim)
  for (ii in 1: nsim){
    y <- c(rnorm(n=nj,mean=muvec[1],sd=1),
           rnorm(n=nj,mean=muvec[2] ,sd=1),
           rnorm(n=nj,mean=muvec[3] ,sd=1),
           rnorm(n=nj,mean=muvec[4] ,sd=1))
    x <- as.factor(c(rep("mu1",nj),rep("mu2",nj),
                     rep("mu3",nj),rep("mu4",nj)))
    res <- anova(aov(y~x))
    res <- as.matrix(res)
    SSb <- res[1,2]
    SSt <- res[1,2] + res[2,2]
    MSw <- res[2,3]
    eta2.2[ii] <- SSb/SSt
  }  
  BIASmat.2[niter,1] <- mean(eta2.2) - eta2p.2
  RMSEmat.2[niter,1] <- sqrt(sum((eta2.2-eta2p.2)^2)/nsim)
  SDmat.2[niter,1] <- sqrt(sum((eta2.2-mean(eta2.2))^2)/nsim)
  niter <- niter+1
}

####okada's code run 1K times, our data generation####
##code edited to only calculate eta2
library(mvtnorm)
library(reshape)
muvec <- c(0.00,0.40,0.80,1.20) ##medium variability
meanmu <- mean(muvec)
sigb <- sum((muvec-meanmu)^2)/4
eta2p.3 <- sigb/(sigb+1)
k <- length(muvec)
nsim <- 1000
njs <- c(10,20,30,40,50,60,70,80,90,100)
BIASmat.3 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(BIASmat.3) <- njs
colnames(BIASmat.3) <- c("eta2")
RMSEmat.3 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(RMSEmat.3) <- njs
colnames(RMSEmat.3) <- c("eta2")
SDmat.3 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(SDmat.3) <- njs
colnames(SDmat.3) <- c("eta2")
niter <- 1

for (nj in njs){
  x <- matrix(NA,nrow=nj,ncol=4)
  eta2.3 <- rep(NA,nsim)
  for (ii in 1: nsim){
    
    sigma = matrix(c(1,0,0,0,
                     0,1,0,0,
                     0,0,1,0,
                     0,0,0,1), nrow = 4, ncol = 4)
    y = melt(rmvnorm(n = nj, muvec, sigma))$value
    
    x <- as.factor(c(rep("mu1",nj),rep("mu2",nj),
                     rep("mu3",nj),rep("mu4",nj)))
    res <- anova(aov(y~x))
    res <- as.matrix(res)
    SSb <- res[1,2]
    SSt <- res[1,2] + res[2,2]
    MSw <- res[2,3]
    eta2.3[ii] <- SSb/SSt
  }  
  BIASmat.3[niter,1] <- mean(eta2.3) - eta2p.3
  RMSEmat.3[niter,1] <- sqrt(sum((eta2.3-eta2p.3)^2)/nsim)
  SDmat.3[niter,1] <- sqrt(sum((eta2.3-mean(eta2.3))^2)/nsim)
  niter <- niter+1
}

####okada's code run 1K times, our eta estimate####
##code edited to only calculate eta2
muvec <- c(0.00,0.40,0.80,1.20) ##medium variability
gm = mean(muvec)
ss = sum((means - gm)^2)/length(means)
eta2p.4 = ss/(ss + 1)
k <- length(muvec)
nsim <- 1000
njs <- c(10,20,30,40,50,60,70,80,90,100)
BIASmat.4 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(BIASmat.4) <- njs
colnames(BIASmat.4) <- c("eta2")
RMSEmat.4 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(RMSEmat.4) <- njs
colnames(RMSEmat.4) <- c("eta2")
SDmat.4 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(SDmat.4) <- njs
colnames(SDmat.4) <- c("eta2")
niter <- 1

for (nj in njs){
  x <- matrix(NA,nrow=nj,ncol=4)
  eta2.4 <- rep(NA,nsim)
  for (ii in 1: nsim){
    y <- c(rnorm(n=nj,mean=muvec[1],sd=1),
           rnorm(n=nj,mean=muvec[2] ,sd=1),
           rnorm(n=nj,mean=muvec[3] ,sd=1),
           rnorm(n=nj,mean=muvec[4] ,sd=1))
    x <- as.factor(c(rep("mu1",nj),rep("mu2",nj),
                     rep("mu3",nj),rep("mu4",nj)))
    res <- anova(aov(y~x))
    res <- as.matrix(res)
    SSb <- res[1,2]
    SSt <- res[1,2] + res[2,2]
    MSw <- res[2,3]
    eta2.4[ii] <- SSb/SSt
  }  
  BIASmat.4[niter,1] <- mean(eta2.4) - eta2p.4
  RMSEmat.4[niter,1] <- sqrt(sum((eta2.4-eta2p.4)^2)/nsim)
  SDmat.4[niter,1] <- sqrt(sum((eta2.4-mean(eta2.4))^2)/nsim)
  niter <- niter+1
}

library(ez)
####okada's code run 1K times, ez anova####
##code edited to only calculate eta2
muvec <- c(0.00,0.40,0.80,1.20) ##medium variability
meanmu <- mean(muvec)
sigb <- sum((muvec-meanmu)^2)/4
eta2p.5 <- sigb/(sigb+1)
k <- length(muvec)
nsim <- 1000
njs <- c(10,20,30,40,50,60,70,80,90,100)
BIASmat.5 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(BIASmat.5) <- njs
colnames(BIASmat.5) <- c("eta2")
RMSEmat.5 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(RMSEmat.5) <- njs
colnames(RMSEmat.5) <- c("eta2")
SDmat.5 <- matrix(NA,nrow=length(njs),ncol=1)
rownames(SDmat.5) <- njs
colnames(SDmat.5) <- c("eta2")
niter <- 1

for (nj in njs){
  x <- matrix(NA,nrow=nj,ncol=4)
  eta2 <- rep(NA,nsim)
  for (ii in 1: nsim){
    y <- c(rnorm(n=nj,mean=muvec[1],sd=1),
           rnorm(n=nj,mean=muvec[2] ,sd=1),
           rnorm(n=nj,mean=muvec[3] ,sd=1),
           rnorm(n=nj,mean=muvec[4] ,sd=1))
    x <- as.factor(c(rep("mu1",nj),rep("mu2",nj),
                     rep("mu3",nj),rep("mu4",nj)))
    
    dataez = as.data.frame(y)
    dataez$x = x
    dataez$partno = as.factor(1:length(y))
    output = ezANOVA(data = dataez,
                     wid = partno,
                     between = x,
                     dv = y, 
                     detailed = T)
    
    SSb = output$ANOVA$SSn
    SSt = output$ANOVA$SSn + output$ANOVA$SSd
    eta2.5[ii] <- SSb/SSt
  }  
  BIASmat.5[niter,1] <- mean(eta2.5) - eta2p.5
  RMSEmat.5[niter,1] <- sqrt(sum((eta2.5-eta2p.5)^2)/nsim)
  SDmat.5[niter,1] <- sqrt(sum((eta2.5-mean(eta2.5))^2)/nsim)
  niter <- niter+1
}

####graphs come back to later####
#Create graph plotting bias.
#library("reshape2")
#library("ggplot2")

#BIAS_long <- melt(BIASmat, value.name = "Bias")  # convert to long format
#colnames(BIAS_long)[1] <- c("SampleSize")
#colnames(BIAS_long)[2] <- c("EffectSize")

#ggplot(data=BIAS_long,
#       aes(x=SampleSize, y=Bias, group = EffectSize, colour=EffectSize)) +
#  geom_line(size=1) + theme_bw(base_size = 16)

