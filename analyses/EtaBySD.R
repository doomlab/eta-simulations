
##load data
n20total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n20total.csv")
n26total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n26total.csv")
n32total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n32total.csv")
n38total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n38total.csv")
n44total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n44total.csv")
n50total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n50total.csv")
n56total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n56total.csv")
n62total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n62total.csv")
n68total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n68total.csv")
n74total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n74total.csv")
n80total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n80total.csv")
n86total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n86total.csv")
n92total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n92total.csv")
n98total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n98total.csv")
n104total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n104total.csv")
n110total <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/Eta Distribution Report/data/n110total.csv")
data = rbind(n20total, n26total, n32total, n38total, n44total,n50total,
             n56total, n62total, n68total, n74total, n80total, n86total,
             n92total, n98total, n104total, n110total)
library(fitdistrplus)

#calculate partial eta squared
data$RM1.pes = data$RM1.ssm.main / (data$RM1.ssm.main + data$RM1.ssr.main)
data$RM2.pes = data$RM2.ssm.main / (data$RM2.ssm.main + data$RM2.ssr.main)
data$MIX.pes = data$MIX.ssm.main / (data$MIX.ssm.main + data$MIX.ssr.main)
data$BN1.pes = data$BN1.ssm.main / (data$BN1.ssm.main + data$BN1.ssr.main)
data$BN2.pes = data$BN2.ssm.main / (data$BN2.ssm.main + data$BN2.ssr.all)

#split up by SD
SDfive = subset(data, stdev == 5)
SDthree = subset(data, stdev == 3)
SDone = subset(data, stdev == 1)


############################################Genearlized Eta Squared

#######################RM1 ges
##RM 1 SDfive ges
hist(SDfive$RM1.ges)
SDfive$RM1.ges[SDfive$RM1.ges == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$RM1.ges, discrete = FALSE)
RM1SD5ges.norm <- fitdist(SDfive$RM1.ges, "norm")
RM1SD5ges.w = fitdist(SDfive$RM1.ges, "weibull")   ##these won't run unless you remove zeros
RM1SD5ges.uniform <- fitdist(SDfive$RM1.ges, "unif")
RM1SD5ges.beta = fitdist(SDfive$RM1.ges, "beta")  ##these won't run unless you remove zeros
RM1SD5ges.exp=fitdist(SDfive$RM1.ges, "exp")
RM1SD5ges.gamma=fitdist(SDfive$RM1.ges, "gamma")
#warning plots are slow
plot(RM1SD5ges.norm)  
plot(RM1SD5ges.uniform)
plot(RM1SD5ges.beta)
plot(RM1SD5ges.w)
plot(RM1SD5ges.exp)
RM1SD5ges.norm$aic
RM1SD5ges.uniform$aic
RM1SD5ges.beta$aic
RM1SD5ges.w$aic
RM1SD5ges.exp$aic
RM1SD5ges.gamma$aic


##RM 1 SDthree ges
hist(SDthree$RM1.ges)
SDthree$RM1.ges[SDthree$RM1.ges == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$RM1.ges, discrete = FALSE)
RM1SD3ges.norm <- fitdist(SDthree$RM1.ges, "norm")
RM1SD3ges.w = fitdist(SDthree$RM1.ges, "weibull")   ##these won't run unless you remove zeros
RM1SD3ges.uniform <- fitdist(SDthree$RM1.ges, "unif")
RM1SD3ges.beta = fitdist(SDthree$RM1.ges, "beta")  ##these won't run unless you remove zeros
RM1SD3ges.exp=fitdist(SDthree$RM1.ges, "exp")
#warning plots are slow
plot(RM1SD3ges.norm)  
plot(RM1SD3ges.uniform)
plot(RM1SD3ges.beta)
plot(RM1SD3ges.w)
plot(RM1SD3ges.exp)
RM1SD3ges.norm$aic
RM1SD3ges.uniform$aic
RM1SD3ges.beta$aic
RM1SD3ges.w$aic
RM1SD3ges.exp$aic

##RM 1 SDone ges
hist(SDone$RM1.ges)
SDone$RM1.ges[SDone$RM1.ges == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$RM1.ges, discrete = FALSE)
RM1SD1ges.norm <- fitdist(SDone$RM1.ges, "norm")
RM1SD1ges.w = fitdist(SDone$RM1.ges, "weibull")   ##these won't run unless you remove zeros
RM1SD1ges.uniform <- fitdist(SDone$RM1.ges, "unif")
RM1SD1ges.beta = fitdist(SDone$RM1.ges, "beta")  ##these won't run unless you remove zeros
RM1SD1ges.exp=fitdist(SDone$RM1.ges, "exp")
RM1SD1ges.gamma=fitdist(SDone$RM1.ges, "gamma")
#warning plots are slow
plot(RM1SD1ges.norm)  
plot(RM1SD1ges.uniform)
plot(RM1SD1ges.beta)
plot(RM1SD1ges.w)
plot(RM1SD1ges.exp)
RM1SD1ges.norm$aic
RM1SD1ges.uniform$aic
RM1SD1ges.beta$aic
RM1SD1ges.w$aic
RM1SD1ges.exp$aic
RM1SD1ges.gamma$aic

#######################RM2 ges
##RM 2 SDfive ges
hist(SDfive$RM2.ges)
SDfive$RM2.ges[SDfive$RM2.ges == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$RM2.ges, discrete = FALSE)
RM2SD5ges.norm <- fitdist(SDfive$RM2.ges, "norm")
RM2SD5ges.w = fitdist(SDfive$RM2.ges, "weibull")   ##these won't run unless you remove zeros
RM2SD5ges.uniform <- fitdist(SDfive$RM2.ges, "unif")
RM2SD5ges.beta = fitdist(SDfive$RM2.ges, "beta")  ##these won't run unless you remove zeros
RM2SD5ges.exp=fitdist(SDfive$RM2.ges, "exp")
#warning plots are slow
plot(RM2SD5ges.norm)  
plot(RM2SD5ges.uniform)
plot(RM2SD5ges.beta)
plot(RM2SD5ges.w)
plot(RM2SD5ges.exp)
RM2SD5ges.norm$aic
RM2SD5ges.uniform$aic
RM2SD5ges.beta$aic
RM2SD5ges.w$aic
RM2SD5ges.exp$aic

##RM 2 SDthree ges
hist(SDthree$RM2.ges)
SDthree$RM2.ges[SDthree$RM2.ges == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$RM2.ges, discrete = FALSE)
RM2SD3ges.norm <- fitdist(SDthree$RM2.ges, "norm")
RM2SD3ges.w = fitdist(SDthree$RM2.ges, "weibull")   ##these won't run unless you remove zeros
RM2SD3ges.uniform <- fitdist(SDthree$RM2.ges, "unif")
RM2SD3ges.beta = fitdist(SDthree$RM2.ges, "beta")  ##these won't run unless you remove zeros
RM2SD3ges.exp=fitdist(SDthree$RM2.ges, "exp")
#warning plots are slow
plot(RM2SD3ges.norm)  
plot(RM2SD3ges.uniform)
plot(RM2SD3ges.beta)
plot(RM2SD3ges.w)
plot(RM2SD3ges.exp)
RM2SD3ges.norm$aic
RM2SD3ges.uniform$aic
RM2SD3ges.beta$aic
RM2SD3ges.w$aic
RM2SD3ges.exp$aic

##RM 2 SDone ges
hist(SDone$RM2.ges)
SDone$RM2.ges[SDone$RM2.ges == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$RM2.ges, discrete = FALSE)
RM2SD1ges.norm <- fitdist(SDone$RM2.ges, "norm")
RM2SD1ges.w = fitdist(SDone$RM2.ges, "weibull")   ##these won't run unless you remove zeros
RM2SD1ges.uniform <- fitdist(SDone$RM2.ges, "unif")
RM2SD1ges.beta = fitdist(SDone$RM2.ges, "beta")  ##these won't run unless you remove zeros
RM2SD1ges.exp=fitdist(SDone$RM2.ges, "exp")
#warning plots are slow
plot(RM2SD1ges.norm)  
plot(RM2SD1ges.uniform)
plot(RM2SD1ges.beta)
plot(RM2SD1ges.w)
plot(RM2SD1ges.exp)
RM2SD1ges.norm$aic
RM2SD1ges.uniform$aic
RM2SD1ges.beta$aic
RM2SD1ges.w$aic
RM2SD1ges.exp$aic

#######################MIX ges
##MIX SDfive ges
hist(SDfive$MIX.ges)
SDfive$MIX.ges[SDfive$MIX.ges == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$MIX.ges, discrete = FALSE)
MIXSD5ges.norm <- fitdist(SDfive$MIX.ges, "norm")
MIXSD5ges.w = fitdist(SDfive$MIX.ges, "weibull")   ##these won't run unless you remove zeros
MIXSD5ges.uniform <- fitdist(SDfive$MIX.ges, "unif")
MIXSD5ges.beta = fitdist(SDfive$MIX.ges, "beta")  ##these won't run unless you remove zeros
MIXSD5ges.exp=fitdist(SDfive$MIX.ges, "exp")
#warning plots are slow
plot(MIXSD5ges.norm)  
plot(MIXSD5ges.uniform)
plot(MIXSD5ges.beta)
plot(MIXSD5ges.w)
plot(MIXSD5ges.exp)
MIXSD5ges.norm$aic
MIXSD5ges.uniform$aic
MIXSD5ges.beta$aic
MIXSD5ges.w$aic
MIXSD5ges.exp$aic

##MIX SDthree ges
hist(SDthree$MIX.ges)
SDthree$MIX.ges[SDthree$MIX.ges == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$MIX.ges, discrete = FALSE)
MIXSD3ges.norm <- fitdist(SDthree$MIX.ges, "norm")
MIXSD3ges.w = fitdist(SDthree$MIX.ges, "weibull")   ##these won't run unless you remove zeros
MIXSD3ges.uniform <- fitdist(SDthree$MIX.ges, "unif")
MIXSD3ges.beta = fitdist(SDthree$MIX.ges, "beta")  ##these won't run unless you remove zeros
MIXSD3ges.exp=fitdist(SDthree$MIX.ges, "exp")
#warning plots are slow
plot(MIXSD3ges.norm)  
plot(MIXSD3ges.uniform)
plot(MIXSD3ges.beta)
plot(MIXSD3ges.w)
plot(MIXSD3ges.exp)
MIXSD3ges.norm$aic
MIXSD3ges.uniform$aic
MIXSD3ges.beta$aic
MIXSD3ges.w$aic
MIXSD3ges.exp$aic

##MIX SDone ges
hist(SDone$MIX.ges)
SDone$MIX.ges[SDone$MIX.ges == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$MIX.ges, discrete = FALSE)
MIXSD1ges.norm <- fitdist(SDone$MIX.ges, "norm")
MIXSD1ges.w = fitdist(SDone$MIX.ges, "weibull")   ##these won't run unless you remove zeros
MIXSD1ges.uniform <- fitdist(SDone$MIX.ges, "unif")
MIXSD1ges.beta = fitdist(SDone$MIX.ges, "beta")  ##these won't run unless you remove zeros
MIXSD1ges.exp=fitdist(SDone$MIX.ges, "exp")
#warning plots are slow
plot(MIXSD1ges.norm)  
plot(MIXSD1ges.uniform)
plot(MIXSD1ges.beta)
plot(MIXSD1ges.w)
plot(MIXSD1ges.exp)
MIXSD1ges.norm$aic
MIXSD1ges.uniform$aic
MIXSD1ges.beta$aic
MIXSD1ges.w$aic
MIXSD1ges.exp$aic

#######################BN1 ges
##BN1 SDfive ges
hist(SDfive$BN1.ges)
SDfive$BN1.ges[SDfive$BN1.ges == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$BN1.ges, discrete = FALSE)
BN1SD5ges.norm <- fitdist(SDfive$BN1.ges, "norm")
BN1SD5ges.w = fitdist(SDfive$BN1.ges, "weibull")   ##these won't run unless you remove zeros
BN1SD5ges.uniform <- fitdist(SDfive$BN1.ges, "unif")
BN1SD5ges.beta = fitdist(SDfive$BN1.ges, "beta")  ##these won't run unless you remove zeros
BN1SD5ges.exp=fitdist(SDfive$BN1.ges, "exp")
#warning plots are slow
plot(BN1SD5ges.norm)  
plot(BN1SD5ges.uniform)
plot(BN1SD5ges.beta)
plot(BN1SD5ges.w)
plot(BN1SD5ges.exp)
BN1SD5ges.norm$aic
BN1SD5ges.uniform$aic
BN1SD5ges.beta$aic
BN1SD5ges.w$aic
BN1SD5ges.exp$aic

##BN1 SDthree ges
hist(SDthree$BN1.ges)
SDthree$BN1.ges[SDthree$BN1.ges == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$BN1.ges, discrete = FALSE)
BN1SD3ges.norm <- fitdist(SDthree$BN1.ges, "norm")
BN1SD3ges.w = fitdist(SDthree$BN1.ges, "weibull")   ##these won't run unless you remove zeros
BN1SD3ges.uniform <- fitdist(SDthree$BN1.ges, "unif")
BN1SD3ges.beta = fitdist(SDthree$BN1.ges, "beta")  ##these won't run unless you remove zeros
BN1SD3ges.exp=fitdist(SDthree$BN1.ges, "exp")
#warning plots are slow
plot(BN1SD3ges.norm)  
plot(BN1SD3ges.uniform)
plot(BN1SD3ges.beta)
plot(BN1SD3ges.w)
plot(BN1SD3ges.exp)
BN1SD3ges.norm$aic
BN1SD3ges.uniform$aic
BN1SD3ges.beta$aic
BN1SD3ges.w$aic
BN1SD3ges.exp$aic

##BN1 SDone ges
hist(SDone$BN1.ges)
SDone$BN1.ges[SDone$BN1.ges == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$BN1.ges, discrete = FALSE)
BN1SD1ges.norm <- fitdist(SDone$BN1.ges, "norm")
BN1SD1ges.w = fitdist(SDone$BN1.ges, "weibull")   ##these won't run unless you remove zeros
BN1SD1ges.uniform <- fitdist(SDone$BN1.ges, "unif")
BN1SD1ges.beta = fitdist(SDone$BN1.ges, "beta")  ##these won't run unless you remove zeros
BN1SD1ges.exp=fitdist(SDone$BN1.ges, "exp")
#warning plots are slow
plot(BN1SD1ges.norm)  
plot(BN1SD1ges.uniform)
plot(BN1SD1ges.beta)
plot(BN1SD1ges.w)
plot(BN1SD1ges.exp)
BN1SD1ges.norm$aic
BN1SD1ges.uniform$aic
BN1SD1ges.beta$aic
BN1SD1ges.w$aic
BN1SD1ges.exp$aic

#######################BN2 ges
##BN2 SDfive ges
hist(SDfive$BN2.ges)
SDfive$BN2.ges[SDfive$BN2.ges == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$BN2.ges, discrete = FALSE)
BN2SD5ges.norm <- fitdist(SDfive$BN2.ges, "norm")
BN2SD5ges.w = fitdist(SDfive$BN2.ges, "weibull")   ##these won't run unless you remove zeros
BN2SD5ges.uniform <- fitdist(SDfive$BN2.ges, "unif")
BN2SD5ges.beta = fitdist(SDfive$BN2.ges, "beta")  ##these won't run unless you remove zeros
BN2SD5ges.exp=fitdist(SDfive$BN2.ges, "exp")
#warning plots are slow
plot(BN2SD5ges.norm)  
plot(BN2SD5ges.uniform)
plot(BN2SD5ges.beta)
plot(BN2SD5ges.w)
plot(BN2SD5ges.exp)
BN2SD5ges.norm$aic
BN2SD5ges.uniform$aic
BN2SD5ges.beta$aic
BN2SD5ges.w$aic
BN2SD5ges.exp$aic

##BN2 SDthree ges
hist(SDthree$BN2.ges)
SDthree$BN2.ges[SDthree$BN2.ges == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$BN2.ges, discrete = FALSE)
BN2SD3ges.norm <- fitdist(SDthree$BN2.ges, "norm")
BN2SD3ges.w = fitdist(SDthree$BN2.ges, "weibull")   ##these won't run unless you remove zeros
BN2SD3ges.uniform <- fitdist(SDthree$BN2.ges, "unif")
BN2SD3ges.beta = fitdist(SDthree$BN2.ges, "beta")  ##these won't run unless you remove zeros
BN2SD3ges.exp=fitdist(SDthree$BN2.ges, "exp")
#warning plots are slow
plot(BN2SD3ges.norm)  
plot(BN2SD3ges.uniform)
plot(BN2SD3ges.beta)
plot(BN2SD3ges.w)
plot(BN2SD3ges.exp)
BN2SD3ges.norm$aic
BN2SD3ges.uniform$aic
BN2SD3ges.beta$aic
BN2SD3ges.w$aic
BN2SD3ges.exp$aic

##BN2 SDone ges
hist(SDone$BN2.ges)
SDone$BN2.ges[SDone$BN2.ges == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$BN2.ges, discrete = FALSE)
BN2SD1ges.norm <- fitdist(SDone$BN2.ges, "norm")
BN2SD1ges.w = fitdist(SDone$BN2.ges, "weibull")   ##these won't run unless you remove zeros
BN2SD1ges.uniform <- fitdist(SDone$BN2.ges, "unif")
BN2SD1ges.beta = fitdist(SDone$BN2.ges, "beta")  ##these won't run unless you remove zeros
BN2SD1ges.exp=fitdist(SDone$BN2.ges, "exp")
#warning plots are slow
plot(BN2SD1ges.norm)  
plot(BN2SD1ges.uniform)
plot(BN2SD1ges.beta)
plot(BN2SD1ges.w)
plot(BN2SD1ges.exp)
BN2SD1ges.norm$aic
BN2SD1ges.uniform$aic
BN2SD1ges.beta$aic
BN2SD1ges.w$aic
BN2SD1ges.exp$aic


############################################Partial Eta Squared

#######################RM1 pes
##RM 1 SDfive pes
hist(SDfive$RM1.pes)
SDfive$RM1.pes[SDfive$RM1.pes == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$RM1.pes, discrete = FALSE)
RM1SD5pes.norm <- fitdist(SDfive$RM1.pes, "norm")
RM1SD5pes.w = fitdist(SDfive$RM1.pes, "weibull")   ##these won't run unless you remove zeros
RM1SD5pes.uniform <- fitdist(SDfive$RM1.pes, "unif")
RM1SD5pes.beta = fitdist(SDfive$RM1.pes, "beta")  ##these won't run unless you remove zeros
RM1SD5pes.exp=fitdist(SDfive$RM1.pes, "exp")
#warning plots are slow
plot(RM1SD5pes.norm)  
plot(RM1SD5pes.uniform)
plot(RM1SD5pes.beta)
plot(RM1SD5pes.w)
plot(RM1SD5pes.exp)
RM1SD5pes.norm$aic
RM1SD5pes.uniform$aic
RM1SD5pes.beta$aic
RM1SD5pes.w$aic
RM1SD5pes.exp$aic

##RM 1 SDthree pes
hist(SDthree$RM1.pes)
SDthree$RM1.pes[SDthree$RM1.pes == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$RM1.pes, discrete = FALSE)
RM1SD3pes.norm <- fitdist(SDthree$RM1.pes, "norm")
RM1SD3pes.w = fitdist(SDthree$RM1.pes, "weibull")   ##these won't run unless you remove zeros
RM1SD3pes.uniform <- fitdist(SDthree$RM1.pes, "unif")
RM1SD3pes.beta = fitdist(SDthree$RM1.pes, "beta")  ##these won't run unless you remove zeros
RM1SD3pes.exp=fitdist(SDthree$RM1.pes, "exp")
#warning plots are slow
plot(RM1SD3pes.norm)  
plot(RM1SD3pes.uniform)
plot(RM1SD3pes.beta)
plot(RM1SD3pes.w)
plot(RM1SD3pes.exp)
RM1SD3pes.norm$aic
RM1SD3pes.uniform$aic
RM1SD3pes.beta$aic
RM1SD3pes.w$aic
RM1SD3pes.exp$aic

##RM 1 SDone pes
hist(SDone$RM1.pes)
SDone$RM1.pes[SDone$RM1.pes == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$RM1.pes, discrete = FALSE)
RM1SD1pes.norm <- fitdist(SDone$RM1.pes, "norm")
RM1SD1pes.w = fitdist(SDone$RM1.pes, "weibull")   ##these won't run unless you remove zeros
RM1SD1pes.uniform <- fitdist(SDone$RM1.pes, "unif")
RM1SD1pes.beta = fitdist(SDone$RM1.pes, "beta")  ##these won't run unless you remove zeros
RM1SD1pes.exp=fitdist(SDone$RM1.pes, "exp")
#warning plots are slow
plot(RM1SD1pes.norm)  
plot(RM1SD1pes.uniform)
plot(RM1SD1pes.beta)
plot(RM1SD1pes.w)
plot(RM1SD1pes.exp)
RM1SD1pes.norm$aic
RM1SD1pes.uniform$aic
RM1SD1pes.beta$aic
RM1SD1pes.w$aic
RM1SD1pes.exp$aic

#######################RM2 pes
##RM 2 SDfive pes
hist(SDfive$RM2.pes)
SDfive$RM2.pes[SDfive$RM2.pes == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$RM2.pes, discrete = FALSE)
RM2SD5pes.norm <- fitdist(SDfive$RM2.pes, "norm")
RM2SD5pes.w = fitdist(SDfive$RM2.pes, "weibull")   ##these won't run unless you remove zeros
RM2SD5pes.uniform <- fitdist(SDfive$RM2.pes, "unif")
RM2SD5pes.beta = fitdist(SDfive$RM2.pes, "beta")  ##these won't run unless you remove zeros
RM2SD5pes.exp=fitdist(SDfive$RM2.pes, "exp")
#warning plots are slow
plot(RM2SD5pes.norm)  
plot(RM2SD5pes.uniform)
plot(RM2SD5pes.beta)
plot(RM2SD5pes.w)
plot(RM2SD5pes.exp)
RM2SD5pes.norm$aic
RM2SD5pes.uniform$aic
RM2SD5pes.beta$aic
RM2SD5pes.w$aic
RM2SD5pes.exp$aic

##RM 2 SDthree pes
hist(SDthree$RM2.pes)
SDthree$RM2.pes[SDthree$RM2.pes == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$RM2.pes, discrete = FALSE)
RM2SD3pes.norm <- fitdist(SDthree$RM2.pes, "norm")
RM2SD3pes.w = fitdist(SDthree$RM2.pes, "weibull")   ##these won't run unless you remove zeros
RM2SD3pes.uniform <- fitdist(SDthree$RM2.pes, "unif")
RM2SD3pes.beta = fitdist(SDthree$RM2.pes, "beta")  ##these won't run unless you remove zeros
RM2SD3pes.exp=fitdist(SDthree$RM2.pes, "exp")
#warning plots are slow
plot(RM2SD3pes.norm)  
plot(RM2SD3pes.uniform)
plot(RM2SD3pes.beta)
plot(RM2SD3pes.w)
plot(RM2SD3pes.exp)
RM2SD3pes.norm$aic
RM2SD3pes.uniform$aic
RM2SD3pes.beta$aic
RM2SD3pes.w$aic
RM2SD3pes.exp$aic

##RM 2 SDone pes
hist(SDone$RM2.pes)
SDone$RM2.pes[SDone$RM2.pes == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$RM2.pes, discrete = FALSE)
RM2SD1pes.norm <- fitdist(SDone$RM2.pes, "norm")
RM2SD1pes.w = fitdist(SDone$RM2.pes, "weibull")   ##these won't run unless you remove zeros
RM2SD1pes.uniform <- fitdist(SDone$RM2.pes, "unif")
RM2SD1pes.beta = fitdist(SDone$RM2.pes, "beta")  ##these won't run unless you remove zeros
RM2SD1pes.exp=fitdist(SDone$RM2.pes, "exp")
#warning plots are slow
plot(RM2SD1pes.norm)  
plot(RM2SD1pes.uniform)
plot(RM2SD1pes.beta)
plot(RM2SD1pes.w)
plot(RM2SD1pes.exp)
RM2SD1pes.norm$aic
RM2SD1pes.uniform$aic
RM2SD1pes.beta$aic
RM2SD1pes.w$aic
RM2SD1pes.exp$aic

#######################MIX pes
##MIX SDfive pes
hist(SDfive$MIX.pes)
SDfive$MIX.pes[SDfive$MIX.pes == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$MIX.pes, discrete = FALSE)
MIXSD5pes.norm <- fitdist(SDfive$MIX.pes, "norm")
MIXSD5pes.w = fitdist(SDfive$MIX.pes, "weibull")   ##these won't run unless you remove zeros
MIXSD5pes.uniform <- fitdist(SDfive$MIX.pes, "unif")
MIXSD5pes.beta = fitdist(SDfive$MIX.pes, "beta")  ##these won't run unless you remove zeros
MIXSD5pes.exp=fitdist(SDfive$MIX.pes, "exp")
#warning plots are slow
plot(MIXSD5pes.norm)  
plot(MIXSD5pes.uniform)
plot(MIXSD5pes.beta)
plot(MIXSD5pes.w)
plot(MIXSD5pes.exp)
MIXSD5pes.norm$aic
MIXSD5pes.uniform$aic
MIXSD5pes.beta$aic
MIXSD5pes.w$aic
MIXSD5pes.exp$aic

##MIX SDthree pes
hist(SDthree$MIX.pes)
SDthree$MIX.pes[SDthree$MIX.pes == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$MIX.pes, discrete = FALSE)
MIXSD3pes.norm <- fitdist(SDthree$MIX.pes, "norm")
MIXSD3pes.w = fitdist(SDthree$MIX.pes, "weibull")   ##these won't run unless you remove zeros
MIXSD3pes.uniform <- fitdist(SDthree$MIX.pes, "unif")
MIXSD3pes.beta = fitdist(SDthree$MIX.pes, "beta")  ##these won't run unless you remove zeros
MIXSD3pes.exp=fitdist(SDthree$MIX.pes, "exp")
#warning plots are slow
plot(MIXSD3pes.norm)  
plot(MIXSD3pes.uniform)
plot(MIXSD3pes.beta)
plot(MIXSD3pes.w)
plot(MIXSD3pes.exp)
MIXSD3pes.norm$aic
MIXSD3pes.uniform$aic
MIXSD3pes.beta$aic
MIXSD3pes.w$aic
MIXSD3pes.exp$aic

##MIX SDone pes
hist(SDone$MIX.pes)
SDone$MIX.pes[SDone$MIX.pes == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$MIX.pes, discrete = FALSE)
MIXSD1pes.norm <- fitdist(SDone$MIX.pes, "norm")
MIXSD1pes.w = fitdist(SDone$MIX.pes, "weibull")   ##these won't run unless you remove zeros
MIXSD1pes.uniform <- fitdist(SDone$MIX.pes, "unif")
MIXSD1pes.beta = fitdist(SDone$MIX.pes, "beta")  ##these won't run unless you remove zeros
MIXSD1pes.exp=fitdist(SDone$MIX.pes, "exp")
#warning plots are slow
plot(MIXSD1pes.norm)  
plot(MIXSD1pes.uniform)
plot(MIXSD1pes.beta)
plot(MIXSD1pes.w)
plot(MIXSD1pes.exp)
MIXSD1pes.norm$aic
MIXSD1pes.uniform$aic
MIXSD1pes.beta$aic
MIXSD1pes.w$aic
MIXSD1pes.exp$aic

#######################BN1 pes
##BN1 SDfive pes
hist(SDfive$BN1.pes)
SDfive$BN1.pes[SDfive$BN1.pes == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$BN1.pes, discrete = FALSE)
BN1SD5pes.norm <- fitdist(SDfive$BN1.pes, "norm")
BN1SD5pes.w = fitdist(SDfive$BN1.pes, "weibull")   ##these won't run unless you remove zeros
BN1SD5pes.uniform <- fitdist(SDfive$BN1.pes, "unif")
BN1SD5pes.beta = fitdist(SDfive$BN1.pes, "beta")  ##these won't run unless you remove zeros
BN1SD5pes.exp=fitdist(SDfive$BN1.pes, "exp")
#warning plots are slow
plot(BN1SD5pes.norm)  
plot(BN1SD5pes.uniform)
plot(BN1SD5pes.beta)
plot(BN1SD5pes.w)
plot(BN1SD5pes.exp)
BN1SD5pes.norm$aic
BN1SD5pes.uniform$aic
BN1SD5pes.beta$aic
BN1SD5pes.w$aic
BN1SD5pes.exp$aic

##BN1 SDthree pes
hist(SDthree$BN1.pes)
SDthree$BN1.pes[SDthree$BN1.pes == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$BN1.pes, discrete = FALSE)
BN1SD3pes.norm <- fitdist(SDthree$BN1.pes, "norm")
BN1SD3pes.w = fitdist(SDthree$BN1.pes, "weibull")   ##these won't run unless you remove zeros
BN1SD3pes.uniform <- fitdist(SDthree$BN1.pes, "unif")
BN1SD3pes.beta = fitdist(SDthree$BN1.pes, "beta")  ##these won't run unless you remove zeros
BN1SD3pes.exp=fitdist(SDthree$BN1.pes, "exp")
#warning plots are slow
plot(BN1SD3pes.norm)  
plot(BN1SD3pes.uniform)
plot(BN1SD3pes.beta)
plot(BN1SD3pes.w)
plot(BN1SD3pes.exp)
BN1SD3pes.norm$aic
BN1SD3pes.uniform$aic
BN1SD3pes.beta$aic
BN1SD3pes.w$aic
BN1SD3pes.exp$aic

##BN1 SDone pes
hist(SDone$BN1.pes)
SDone$BN1.pes[SDone$BN1.pes == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$BN1.pes, discrete = FALSE)
BN1SD1pes.norm <- fitdist(SDone$BN1.pes, "norm")
BN1SD1pes.w = fitdist(SDone$BN1.pes, "weibull")   ##these won't run unless you remove zeros
BN1SD1pes.uniform <- fitdist(SDone$BN1.pes, "unif")
BN1SD1pes.beta = fitdist(SDone$BN1.pes, "beta")  ##these won't run unless you remove zeros
BN1SD1pes.exp=fitdist(SDone$BN1.pes, "exp")
#warning plots are slow
plot(BN1SD1pes.norm)  
plot(BN1SD1pes.uniform)
plot(BN1SD1pes.beta)
plot(BN1SD1pes.w)
plot(BN1SD1pes.exp)
BN1SD1pes.norm$aic
BN1SD1pes.uniform$aic
BN1SD1pes.beta$aic
BN1SD1pes.w$aic
BN1SD1pes.exp$aic

#######################BN2 pes
##BN2 SDfive pes
hist(SDfive$BN2.pes)
SDfive$BN2.pes[SDfive$BN2.pes == 0] <- NA #for weibull and beta
SDfive = na.omit(SDfive)                  #for weibull and beta
descdist(SDfive$BN2.pes, discrete = FALSE)
BN2SD5pes.norm <- fitdist(SDfive$BN2.pes, "norm")
BN2SD5pes.w = fitdist(SDfive$BN2.pes, "weibull")   ##these won't run unless you remove zeros
BN2SD5pes.uniform <- fitdist(SDfive$BN2.pes, "unif")
BN2SD5pes.beta = fitdist(SDfive$BN2.pes, "beta")  ##these won't run unless you remove zeros
BN2SD5pes.exp=fitdist(SDfive$BN2.pes, "exp")
#warning plots are slow
plot(BN2SD5pes.norm)  
plot(BN2SD5pes.uniform)
plot(BN2SD5pes.beta)
plot(BN2SD5pes.w)
plot(BN2SD5pes.exp)
BN2SD5pes.norm$aic
BN2SD5pes.uniform$aic
BN2SD5pes.beta$aic
BN2SD5pes.w$aic
BN2SD5pes.exp$aic

##BN2 SDthree pes
hist(SDthree$BN2.pes)
SDthree$BN2.pes[SDthree$BN2.pes == 0] <- NA #for weibull and beta
SDthree = na.omit(SDthree)                  #for weibull and beta
descdist(SDthree$BN2.pes, discrete = FALSE)
BN2SD3pes.norm <- fitdist(SDthree$BN2.pes, "norm")
BN2SD3pes.w = fitdist(SDthree$BN2.pes, "weibull")   ##these won't run unless you remove zeros
BN2SD3pes.uniform <- fitdist(SDthree$BN2.pes, "unif")
BN2SD3pes.beta = fitdist(SDthree$BN2.pes, "beta")  ##these won't run unless you remove zeros
BN2SD3pes.exp=fitdist(SDthree$BN2.pes, "exp")
#warning plots are slow
plot(BN2SD3pes.norm)  
plot(BN2SD3pes.uniform)
plot(BN2SD3pes.beta)
plot(BN2SD3pes.w)
plot(BN2SD3pes.exp)
BN2SD3pes.norm$aic
BN2SD3pes.uniform$aic
BN2SD3pes.beta$aic
BN2SD3pes.w$aic
BN2SD3pes.exp$aic

##BN2 SDone pes
hist(SDone$BN2.pes)
SDone$BN2.pes[SDone$BN2.pes == 0] <- NA #for weibull and beta
SDone = na.omit(SDone)                  #for weibull and beta
descdist(SDone$BN2.pes, discrete = FALSE)
BN2SD1pes.norm <- fitdist(SDone$BN2.pes, "norm")
BN2SD1pes.w = fitdist(SDone$BN2.pes, "weibull")   ##these won't run unless you remove zeros
BN2SD1pes.uniform <- fitdist(SDone$BN2.pes, "unif")
BN2SD1pes.beta = fitdist(SDone$BN2.pes, "beta")  ##these won't run unless you remove zeros
BN2SD1pes.exp=fitdist(SDone$BN2.pes, "exp")
#warning plots are slow
plot(BN2SD1pes.norm)  
plot(BN2SD1pes.uniform)
plot(BN2SD1pes.beta)
plot(BN2SD1pes.w)
plot(BN2SD1pes.exp)
BN2SD1pes.norm$aic
BN2SD1pes.uniform$aic
BN2SD1pes.beta$aic
BN2SD1pes.w$aic
BN2SD1pes.exp$aic