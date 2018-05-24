#Simulate bias in eta-squared, omega-squared, and epsilon-squared.
#R script by Kensuke Okada from: Okada, K. (2013). Is omega squared less biased? A comparison of three major effect size indices in one-way ANOVA. Behaviormetrika, 40(2), 129-147.
library(ez)
library(reshape)
library(mvtnorm)

muvec <- c(2.5, 3.0, 3.5, 4.0) ###I changed this here
meanmu <- mean(muvec)
sigb <- sum((muvec-meanmu)^2)/4
eta2p <- sigb/(sigb+1)
k <- length(muvec)
nsim <- 1000 #Bias should decrease as sample size increases. Set simulations to 1000000 for best results
njs <- c(10,20,30,40,50,60,70,80,90,100)
BIASmat <- matrix(NA,nrow=length(njs),ncol=3)
rownames(BIASmat) <- njs
colnames(BIASmat) <- c("eta2","epsilon2","omega2")
RMSEmat <- matrix(NA,nrow=length(njs),ncol=3)
rownames(RMSEmat) <- njs
colnames(RMSEmat) <- c("eta2","epsilon2","omega2")
SDmat <- matrix(NA,nrow=length(njs),ncol=3)
rownames(SDmat) <- njs
colnames(SDmat) <- c("eta2","epsilon2","omega2")
niter <- 1

for (nj in njs){
  x <- matrix(NA,nrow=nj,ncol=4)
  eta2 <- rep(NA,nsim)
  epsilon2 <- rep(NA,nsim)
  omega2 <- rep(NA,nsim)
  for (ii in 1: nsim){
    
    ##testing with rmvnorm package
    sigma = matrix(c(5,0,0,0,
                     0,5,0,0,
                     0,0,5,0,
                     0,0,0,5), nrow = 4, ncol = 4)
    y = melt(rmvnorm(n = nj, muvec, sigma))$value
    
    ##old okada normal
    #y <- c(rnorm(n=nj,mean=muvec[1],sd=1),
    #       rnorm(n=nj,mean=muvec[2] ,sd=1),
    #       rnorm(n=nj,mean=muvec[3] ,sd=1),
    #       rnorm(n=nj,mean=muvec[4] ,sd=1))
    x <- as.factor(c(rep("mu1",nj),rep("mu2",nj),
                     rep("mu3",nj),rep("mu4",nj)))
    #res <- anova(aov(y~x))
    #res <- as.matrix(res)
    
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
    MSw = output$ANOVA$SSd / output$ANOVA$DFd
    #SSb <- res[1,2]
    #SSt <- res[1,2] + res[2,2]
    #MSw <- res[2,3]
    eta2[ii] <- SSb/SSt
    epsilon2[ii] <- (SSb - 3*MSw)/SSt
    omega2[ii] <- (SSb - 3*MSw)/(SSt+MSw)
  }  
  BIASmat[niter,1] <- mean(eta2) - eta2p
  BIASmat[niter,2] <- mean(epsilon2) - eta2p
  BIASmat[niter,3] <- mean(omega2) - eta2p
  RMSEmat[niter,1] <- sqrt(sum((eta2-eta2p)^2)/nsim)
  RMSEmat[niter,2] <- sqrt(sum((epsilon2-eta2p)^2)/nsim)
  RMSEmat[niter,3] <- sqrt(sum((omega2-eta2p)^2)/nsim)
  SDmat[niter,1] <- sqrt(sum((eta2-mean(eta2))^2)/nsim)
  SDmat[niter,2] <- sqrt(sum((epsilon2-mean(epsilon2))^2)
                         /nsim)
  SDmat[niter,3] <- sqrt(sum((omega2-mean(omega2))^2)
                         /nsim)
  niter <- niter+1
}

#Create graph plotting bias.
library("reshape2")
library("ggplot2")

BIAS_long <- melt(BIASmat, value.name = "Bias")  # convert to long format
colnames(BIAS_long)[1] <- c("SampleSize")
colnames(BIAS_long)[2] <- c("EffectSize")
colnames(BIAS_long)[3] <- c("Bias")

ggplot(data=BIAS_long,
       aes(x=SampleSize, y=Bias, group = EffectSize, colour=EffectSize)) +
  geom_line(size=1) + theme_bw(base_size = 16)

#BiasMatrix
BIASmat

#True effect size
eta2p

