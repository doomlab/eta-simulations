
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


########################Generalized Eta Squared
#RM1 ges
mean(data$RM1.ges)
sd(data$RM1.ges)
var(data$RM1.ges)
length(data$RM1.ges)
#RM2 ges
mean(data$RM2.ges)
sd(data$RM2.ges)
var(data$RM2.ges)
length(data$RM2.ges)
#MIX ges
mean(data$MIX.ges)
sd(data$MIX.ges)
var(data$MIX.ges)
length(data$MIX.ges)
#BN1 ges
mean(data$BN1.ges)
sd(data$BN1.ges)
var(data$BN1.ges)
length(data$BN1.ges)
#BN2 ges
mean(data$BN2.ges)
sd(data$BN2.ges)
var(data$BN2.ges)
length(data$BN2.ges)


##RM 1 ges
hist(data$RM1.ges)
data$RM1.ges[data$RM1.ges == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$RM1.ges, discrete = FALSE)
RM1ges.norm <- fitdist(data$RM1.ges, "norm")
RM1ges.w = fitdist(data$RM1.ges, "weibull")   ##these won't run unless you remove zeros
RM1ges.uniform <- fitdist(data$RM1.ges, "unif")
RM1ges.beta = fitdist(data$RM1.ges, "beta")  ##these won't run unless you remove zeros
RM1ges.exp=fitdist(data$RM1.ges, "exp")
#warning plots are slow
plot(RM1ges.norm)  
plot(RM1ges.uniform)
plot(RM1ges.beta)
plot(RM1ges.w)
plot(RM1ges.exp)
RM1ges.norm$aic
RM1ges.uniform$aic
RM1ges.beta$aic
RM1ges.w$aic
RM1ges.exp$aic


##RM 2 ges
data$RM2.ges[data$RM2.ges == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$RM2.ges, discrete = FALSE)
RM2ges.norm <- fitdist(data$RM2.ges, "norm")
RM2ges.w = fitdist(data$RM2.ges, "weibull") 
RM2ges.uniform <- fitdist(data$RM2.ges, "unif")
RM2ges.beta = fitdist(data$RM2.ges, "beta")  
RM2ges.exp=fitdist(data$RM2.ges, "exp")
#warning plots are slow
plot(RM2ges.norm)
plot(RM2ges.uniform)
plot(RM2ges.beta)
plot(RM2ges.w)
plot(RM2ges.exp)
RM2ges.norm$aic
RM2ges.uniform$aic
RM2ges.beta$aic
RM2ges.w$aic
RM2ges.exp$aic

##MIX ges
data$MIX.ges[data$MIX.ges == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$MIX.ges, discrete = FALSE)
MIXges.norm <- fitdist(data$MIX.ges, "norm")
MIXges.w = fitdist(data$MIX.ges, "weibull")
MIXges.uniform <- fitdist(data$MIX.ges, "unif")
MIXges.beta = fitdist(data$MIX.ges, "beta")
MIXges.exp=fitdist(data$MIX.ges, "exp")
plot(MIXges.norm)
plot(MIXges.uniform)
plot(MIXges.beta)
plot(MIXges.w)
plot(MIXges.exp)
MIX.norm$aic
MIXges.uniform$aic
MIXges.beta$aic
MIXges.w$aic
MIXges.exp$aic

##BN1 ges
data$BN1.ges[data$BN1.ges == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$BN1.ges, discrete = FALSE)
BN1ges.norm <- fitdist(data$BN1.ges, "norm")
BN1ges.w = fitdist(data$BN1.ges, "weibull")
BN1ges.uniform <- fitdist(data$BN1.ges, "unif")
BN1ges.beta = fitdist(data$BN1.ges, "beta")
BN1ges.exp=fitdist(data$BN1.ges, "exp")
#plots are slow
plot(BN1ges.norm)
plot(BN1ges.uniform)
plot(BN1ges.beta)
plot(BN1ges.w)
plot(BN1ges.exp)
BN1ges.norm$aic
BN1ges.uniform$aic
BN1ges.beta$aic
BN1ges.w$aic
BN1ges.exp$aic

##BN2 ges
data$BN2.ges[data$BN2.ges == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$BN2.ges, discrete = FALSE)
BN2ges.norm <- fitdist(data$BN2.ges, "norm")
BN2ges.w = fitdist(data$BN2.ges, "weibull")
BN2ges.uniform <- fitdist(data$BN2.ges, "unif")
BN2ges.beta = fitdist(data$BN2.ges, "beta")
BN2ges.exp=fitdist(data$BN2.ges, "exp")
#warning plots are slow
plot(BN2ges.norm)
plot(BN2ges.uniform)
plot(BN2ges.beta)
plot(BN2ges.w)
plot(BN2ges.exp)
BN2ges.norm$aic
BN2ges.uniform$aic
BN2ges.beta$aic
BN2ges.w$aic
BN2ges.exp$aic


###########################Parital Eta Squared
#RM1 pes
mean(data$RM1.pes)
sd(data$RM1.pes)
var(data$RM1.pes)
length(data$RM1.pes)
#RM2 pes
mean(data$RM2.pes)
sd(data$RM2.pes)
var(data$RM2.pes)
length(data$RM2.pes)
#MIX pes
mean(data$MIX.pes)
sd(data$MIX.pes)
var(data$MIX.pes)
length(data$MIX.pes)
#BN1 pes
mean(data$BN1.pes)
sd(data$BN1.pes)
var(data$BN1.pes)
length(data$BN1.pes)
#BN2 pes
mean(data$BN2.pes)
sd(data$BN2.pes)
var(data$BN2.pes)
length(data$BN2.pes)


##RM 1 pes
hist(data$RM1.pes)
data$RM1.pes[data$RM1.pes == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$RM1.pes, discrete = FALSE)
RM1pes.norm <- fitdist(data$RM1.pes, "norm")
RM1pes.w = fitdist(data$RM1.pes, "weibull")   ##these won't run unless you remove zeros
RM1pes.uniform <- fitdist(data$RM1.pes, "unif")
RM1pes.beta = fitdist(data$RM1.pes, "beta")  ##these won't run unless you remove zeros
RM1pes.exp=fitdist(data$RM1.pes, "exp")
#warning plots are slow
plot(RM1pes.norm)  
plot(RM1pes.uniform)
plot(RM1pes.beta)
plot(RM1pes.w)
plot(RM1pes.exp)
RM1pes.norm$aic
RM1pes.uniform$aic
RM1pes.beta$aic
RM1pes.w$aic
RM1pes.exp$aic


##RM 2 pes
data$RM2.pes[data$RM2.pes == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$RM2.pes, discrete = FALSE)
RM2pes.norm <- fitdist(data$RM2.pes, "norm")
RM2pes.w = fitdist(data$RM2.pes, "weibull") 
RM2pes.uniform <- fitdist(data$RM2.pes, "unif")
RM2pes.beta = fitdist(data$RM2.pes, "beta")  
RM2pes.exp=fitdist(data$RM2.pes, "exp")
#warning plots are slow
plot(RM2pes.norm)
plot(RM2pes.uniform)
plot(RM2pes.beta)
plot(RM2pes.w)
plot(RM2pes.exp)
RM2pes.norm$aic
RM2pes.uniform$aic
RM2pes.beta$aic
RM2pes.w$aic
RM2pes.exp$aic

##MIX pes
data$MIX.pes[data$MIX.pes == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$MIX.pes, discrete = FALSE)
MIXpes.norm <- fitdist(data$MIX.pes, "norm")
MIXpes.w = fitdist(data$MIX.pes, "weibull")
MIXpes.uniform <- fitdist(data$MIX.pes, "unif")
MIXpes.beta = fitdist(data$MIX.pes, "beta")
MIXpes.exp=fitdist(data$MIX.pes, "exp")
plot(MIXpes.norm)
plot(MIXpes.uniform)
plot(MIXpes.beta)
plot(MIXpes.w)
plot(MIXpes.exp)
MIX.norm$aic
MIXpes.uniform$aic
MIXpes.beta$aic
MIXpes.w$aic
MIXpes.exp$aic

##BN1 pes
data$BN1.pes[data$BN1.pes == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$BN1.pes, discrete = FALSE)
BN1pes.norm <- fitdist(data$BN1.pes, "norm")
BN1pes.w = fitdist(data$BN1.pes, "weibull")
BN1pes.uniform <- fitdist(data$BN1.pes, "unif")
BN1pes.beta = fitdist(data$BN1.pes, "beta")
BN1pes.exp=fitdist(data$BN1.pes, "exp")
#plots are slow
plot(BN1pes.norm)
plot(BN1pes.uniform)
plot(BN1pes.beta)
plot(BN1pes.w)
plot(BN1pes.exp)
BN1pes.norm$aic
BN1pes.uniform$aic
BN1pes.beta$aic
BN1pes.w$aic
BN1pes.exp$aic

##BN2 pes
data$BN2.pes[data$BN2.pes == 0] <- NA #for weibull and beta
data = na.omit(data)                  #for weibull and beta
descdist(data$BN2.pes, discrete = FALSE)
BN2pes.norm <- fitdist(data$BN2.pes, "norm")
BN2pes.w = fitdist(data$BN2.pes, "weibull")
BN2pes.uniform <- fitdist(data$BN2.pes, "unif")
BN2pes.beta = fitdist(data$BN2.pes, "beta")
BN2pes.exp=fitdist(data$BN2.pes, "exp")
#warning plots are slow
plot(BN2pes.norm)
plot(BN2pes.uniform)
plot(BN2pes.beta)
plot(BN2pes.w)
plot(BN2pes.exp)
BN2pes.norm$aic
BN2pes.uniform$aic
BN2pes.beta$aic
BN2pes.w$aic
BN2pes.exp$aic
