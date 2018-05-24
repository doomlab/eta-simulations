
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
fulldata=data
rm(n20total,n26total,n32total,n38total,n44total,n50total,n56total,n62total,n68total,n74total,n80total,
   n86total,n92total,n98total,n104total,n110total,data)

#calculate partial eta squared
fulldata$RM1.pes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main)
fulldata$RM2.pes = fulldata$RM2.ssm.main / (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main)
fulldata$MIX.pes = fulldata$MIX.ssm.main / (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main)
fulldata$BN1.pes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.pes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all)

#calculate full eta squared
fulldata$RM1.fes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main + fulldata$RM1.ssr.p)
fulldata$RM2.fes = fulldata$RM2.ssm.main / 
  (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main + fulldata$RM2.ssm.other + fulldata$RM2.ssm.interact + fulldata$RM2.ssr.p + fulldata$RM2.ssr.other + fulldata$RM2.ssr.interact)
fulldata$MIX.fes = fulldata$MIX.ssm.main / 
  (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main + fulldata$MIX.ssm.other + fulldata$MIX.ssm.interact + fulldata$MIX.ssr.p + fulldata$MIX.ssr.other + fulldata$MIX.ssr.interact)
fulldata$BN1.fes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.fes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all + fulldata$BN2.ssm.other + fulldata$BN2.ssm.interact)

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

#calculate partial omega squared
#RM1 NA
#BN1 NA
fulldata$RM2.pos = (fulldata$RM2.dfm*((fulldata$RM2.ssm.main/fulldata$RM2.dfm)-(fulldata$RM2.ssr.main/fulldata$RM2.dfr)))/
  (fulldata$RM2.ssm.main+fulldata$RM2.ssr.main+fulldata$RM2.ssm.p+(fulldata$RM2.ssm.p/(fulldata$RM2.dfr/fulldata$RM2.dfm)))
fulldata$BN2.pos = (fulldata$BN2.dfm*((fulldata$BN2.ssm.main/fulldata$BN2.dfm)-(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))/
  (fulldata$BN2.ssm.main+(((fulldata$N*fulldata$levels)-fulldata$BN2.dfm)*(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))
fulldata$MIX.pos = (fulldata$MIX.dfm*((fulldata$MIX.ssm.main/fulldata$MIX.dfm)-(fulldata$MIX.ssr.main/fulldata$MIX.dfr)))/
  (fulldata$MIX.ssm.main+fulldata$MIX.ssr.main+fulldata$MIX.ssm.p+(fulldata$MIX.ssm.p/(fulldata$MIX.dfr/fulldata$MIX.dfm)))

###############################################################################
############# Bias Testing: Developed in part from Okada (2013) ###############
###############################################################################

N = c(20,26,32,38,44,50,56,62,68,74,80,86,92,98,104,110)
stdev = c(5,3,1)
ES = c("ges","pes","fes","pos","fos")
Levels = 3:6
Corr = c(0.0,0.1,0.3,0.5,0.7,0.9)
biasdata = expand.grid(Corr,stdev,Levels,N)
colnames(biasdata) = c("Corr", "stdev", "Levels","N")
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
n.sim = 1152
round = 0
start = 1
finish = 1000
dat3 = c(2.5,3.0,3.5)
dat4 = c(2.5,3.0,3.5,4.0)
dat5 = c(2.5,3.0,3.5,4.0,4.5)
dat6 = c(2.5,3.0,3.5,4.0,4.5,5.0)
meandat3 = mean(dat3)
meandat4 = mean(dat4)
meandat5 = mean(dat5)
meandat6 = mean(dat6)
sig3 = sum((dat3-meandat3)^2)/length(dat3)
sig4 = sum((dat4-meandat4)^2)/length(dat4)
sig5 = sum((dat5-meandat5)^2)/length(dat5)
sig6 = sum((dat6-meandat6)^2)/length(dat6)
eta2p3 = sig3/(sig3+1)
eta2p4 = sig4/(sig4+1)
eta2p5 = sig5/(sig5+1)
eta2p6 = sig6/(sig6+1)

sigsd1 = .21
sigsd2 = .13
sigsd3 = .11

biasdata$eta2p = rep((c(rep(eta2p3,18),rep(eta2p4,18),rep(eta2p5,18),rep(eta2p6,18))),16)

####RM1####
##RM1 GES
for(i in 1:n.sim){
  round = round+1
  RM1.GES = fulldata$RM1.ges[start:finish]
  biasdata$effect[round] = mean(RM1.GES)
  biasdata$Bias[round] = mean(RM1.GES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM1.GES - biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM1.GES - mean(RM1.GES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM1.GES.Data = biasdata
RM1.GES.Data$ES = "ges"
##RM1 PES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
for(i in 1:n.sim){
  round = round+1
  RM1.PES = fulldata$RM1.pes[start:finish]
  biasdata$effect[round] = mean(RM1.PES)
  biasdata$Bias[round] = mean(RM1.PES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM1.PES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM1.PES-mean(RM1.PES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM1.PES.Data = biasdata
RM1.PES.Data$ES = "pes"
#RM1 FES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  RM1.FES = fulldata$RM1.fes[start:finish]
  biasdata$effect[round] = mean(RM1.FES)
  biasdata$Bias[round] = mean(RM1.FES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM1.FES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM1.FES-mean(RM1.FES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM1.FES.Data = biasdata
RM1.FES.Data$ES = "fes"
#RM1 FOS
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  RM1.FOS = fulldata$RM1.fos[start:finish]
  biasdata$effect[round] = mean(RM1.FOS)
  biasdata$Bias[round] = mean(RM1.FOS) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM1.FOS-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM1.FOS-mean(RM1.FOS))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM1.FOS.Data = biasdata
RM1.FOS.Data$ES = "fos"

RM1.bias.data = rbind(RM1.GES.Data,RM1.FES.Data,RM1.PES.Data,RM1.FOS.Data)
rm(RM1.GES.Data,RM1.FES.Data,RM1.PES.Data,RM1.FOS.Data)
rm(RM1.FES,RM1.FOS,RM1.GES,RM1.PES)
####end RM1####

####RM2####
##RM2 GES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  RM2.GES = fulldata$RM2.ges[start:finish]
  biasdata$effect[round] = mean(RM2.GES)
  biasdata$Bias[round] = mean(RM2.GES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM2.GES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM2.GES-mean(RM2.GES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM2.GES.Data = biasdata
RM2.GES.Data$ES = "ges"
##RM2 PES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  RM2.PES = fulldata$RM2.pes[start:finish]
  biasdata$effect[round] = mean(RM2.PES)
  biasdata$Bias[round] = mean(RM2.PES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM2.PES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM2.PES-mean(RM2.PES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM2.PES.Data = biasdata
RM2.PES.Data$ES = "pes"
#RM2 FES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  RM2.FES = fulldata$RM2.fes[start:finish]
  biasdata$effect[round] = mean(RM2.FES)
  biasdata$Bias[round] = mean(RM2.FES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM2.FES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM2.FES-mean(RM2.FES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM2.FES.Data = biasdata
RM2.FES.Data$ES = "fes"
#RM2 FOS
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  RM2.FOS = fulldata$RM2.fos[start:finish]
  biasdata$effect[round] = mean(RM2.FOS)
  biasdata$Bias[round] = mean(RM2.FOS) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM2.FOS-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM2.FOS-mean(RM2.FOS))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM2.FOS.Data = biasdata
RM2.FOS.Data$ES = "fos"
#RM2 POS
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  RM2.POS = fulldata$RM2.pos[start:finish]
  biasdata$effect[round] = mean(RM2.POS)
  biasdata$Bias[round] = mean(RM2.POS) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((RM2.POS-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((RM2.POS-mean(RM2.POS))^2)/1000)
  start = start+1000
  finish = finish+1000
}
RM2.POS.Data = biasdata
RM2.POS.Data$ES = "pos"

RM2.bias.data = rbind(RM2.GES.Data,RM2.FES.Data,RM2.PES.Data,RM2.FOS.Data,RM2.POS.Data)
rm(RM2.GES.Data,RM2.FES.Data,RM2.PES.Data,RM2.FOS.Data,RM2.POS.Data)
rm(RM2.GES,RM2.FES,RM2.PES,RM2.FOS,RM2.POS)
####end RM2####

####BN1####
##BN1 GES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN1.GES = fulldata$BN1.ges[start:finish]
  biasdata$effect[round] = mean(BN1.GES)
  biasdata$Bias[round] = mean(BN1.GES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN1.GES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN1.GES-mean(BN1.GES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN1.GES.Data = biasdata
BN1.GES.Data$ES = "ges"
##BN1 PES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN1.PES = fulldata$BN1.pes[start:finish]
  biasdata$effect[round] = mean(BN1.PES)
  biasdata$Bias[round] = mean(BN1.PES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN1.PES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN1.PES-mean(BN1.PES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN1.PES.Data = biasdata
BN1.PES.Data$ES = "pes"
#BN1 FES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN1.FES = fulldata$BN1.fes[start:finish]
  biasdata$effect[round] = mean(BN1.FES)
  biasdata$Bias[round] = mean(BN1.FES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN1.FES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN1.FES-mean(BN1.FES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN1.FES.Data = biasdata
BN1.FES.Data$ES = "fes"
#BN1 FOS
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN1.FOS = fulldata$BN1.fos[start:finish]
  biasdata$effect[round] = mean(BN1.FOS)
  biasdata$Bias[round] = mean(BN1.FOS) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN1.FOS-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN1.FOS-mean(BN1.FOS))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN1.FOS.Data = biasdata
BN1.FOS.Data$ES = "fos"

BN1.bias.data = rbind(BN1.GES.Data,BN1.FES.Data,BN1.PES.Data,BN1.FOS.Data)
rm(BN1.GES.Data,BN1.FES.Data,BN1.PES.Data,BN1.FOS.Data)
rm(BN1.GES,BN1.FES,BN1.PES,BN1.FOS)
####end BN1####

####BN2####
##BN2 GES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN2.GES = fulldata$BN2.ges[start:finish]
  biasdata$effect[round] = mean(BN2.GES)
  biasdata$Bias[round] = mean(BN2.GES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN2.GES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN2.GES-mean(BN2.GES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN2.GES.Data = biasdata
BN2.GES.Data$ES = "ges"
##BN2 PES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN2.PES = fulldata$BN2.pes[start:finish]
  biasdata$effect[round] = mean(BN2.PES)
  biasdata$Bias[round] = mean(BN2.PES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN2.PES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN2.PES-mean(BN2.PES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN2.PES.Data = biasdata
BN2.PES.Data$ES = "pes"
#BN2 FES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN2.FES = fulldata$BN2.fes[start:finish]
  biasdata$effect[round] = mean(BN2.FES)
  biasdata$Bias[round] = mean(BN2.FES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN2.FES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN2.FES-mean(BN2.FES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN2.FES.Data = biasdata
BN2.FES.Data$ES = "fes"
#BN2 FOS
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN2.FOS = fulldata$BN2.fos[start:finish]
  biasdata$effect[round] = mean(BN2.FOS)
  biasdata$Bias[round] = mean(BN2.FOS) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN2.FOS-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN2.FOS-mean(BN2.FOS))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN2.FOS.Data = biasdata
BN2.FOS.Data$ES = "fos"
#BN2 POS
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  BN2.POS = fulldata$BN2.pos[start:finish]
  biasdata$effect[round] = mean(BN2.POS)
  biasdata$Bias[round] = mean(BN2.POS) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((BN2.POS-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((BN2.POS-mean(BN2.POS))^2)/1000)
  start = start+1000
  finish = finish+1000
}
BN2.POS.Data = biasdata
BN2.POS.Data$ES = "pos"

BN2.bias.data = rbind(BN2.GES.Data,BN2.FES.Data,BN2.PES.Data,BN2.FOS.Data,BN2.POS.Data)
rm(BN2.GES.Data,BN2.FES.Data,BN2.PES.Data,BN2.FOS.Data,BN2.POS.Data)
rm(BN2.GES,BN2.PES,BN2.FES,BN2.POS,BN2.FOS)
####end BN2####

####MIX####
##MIX GES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  MIX.GES = fulldata$MIX.ges[start:finish]
  biasdata$effect[round] = mean(MIX.GES)
  biasdata$Bias[round] = mean(MIX.GES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((MIX.GES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((MIX.GES-mean(MIX.GES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
MIX.GES.Data = biasdata
MIX.GES.Data$ES = "ges"
##MIX PES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  MIX.PES = fulldata$MIX.pes[start:finish]
  biasdata$effect[round] = mean(MIX.PES)
  biasdata$Bias[round] = mean(MIX.PES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((MIX.PES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((MIX.PES-mean(MIX.PES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
MIX.PES.Data = biasdata
MIX.PES.Data$ES = "pes"
#MIX FES
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  MIX.FES = fulldata$MIX.fes[start:finish]
  biasdata$effect[round] = mean(MIX.FES)
  biasdata$Bias[round] = mean(MIX.FES) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((MIX.FES-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((MIX.FES-mean(MIX.FES))^2)/1000)
  start = start+1000
  finish = finish+1000
}
MIX.FES.Data = biasdata
MIX.FES.Data$ES = "fes"
#MIX FOS
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  MIX.FOS = fulldata$MIX.fos[start:finish]
  biasdata$effect[round] = mean(MIX.FOS)
  biasdata$Bias[round] = mean(MIX.FOS) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((MIX.FOS-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((MIX.FOS-mean(MIX.FOS))^2)/1000)
  start = start+1000
  finish = finish+1000
}
MIX.FOS.Data = biasdata
MIX.FOS.Data$ES = "fos"
#MIX POS
round = 0
start = 1
finish = 1000
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0
biasdata$effect = 0
for(i in 1:n.sim){
  round = round+1
  MIX.POS = fulldata$MIX.pos[start:finish]
  biasdata$effect[round] = mean(MIX.POS)
  biasdata$Bias[round] = mean(MIX.POS) - biasdata$eta2p[round]
  biasdata$RMSE[round] = sqrt(sum((MIX.POS-biasdata$eta2p[round])^2)/1000)
  biasdata$SD[round] = sqrt(sum((MIX.POS-mean(MIX.POS))^2)/1000)
  start = start+1000
  finish = finish+1000
}
MIX.POS.Data = biasdata
MIX.POS.Data$ES = "pos"

MIX.bias.data = rbind(MIX.GES.Data,MIX.FES.Data,MIX.PES.Data,MIX.FOS.Data,MIX.POS.Data)
rm(MIX.GES.Data,MIX.FES.Data,MIX.PES.Data,MIX.FOS.Data,MIX.POS.Data)
rm(MIX.GES,MIX.PES,MIX.FES,MIX.POS,MIX.FOS)
####end MIX####

RM1.bias.data$Design = "RM1"
RM2.bias.data$Design = "RM2"
BN1.bias.data$Design = "BN1"
BN2.bias.data$Design = "BN2"
MIX.bias.data$Design = "MIX"

Full.bias.data = rbind(RM1.bias.data,RM2.bias.data,BN1.bias.data,BN2.bias.data,MIX.bias.data)
rm(RM1.bias.data,RM2.bias.data,BN1.bias.data,BN2.bias.data,MIX.bias.data)
rm(biasdata)

options(scipen=999)
View(Full.bias.data)



tapply(Full.bias.data$Bias, list(Full.bias.data$ES), mean)
tapply(Full.bias.data$SD, list(Full.bias.data$ES), mean)
tapply(Full.bias.data$RMSE, list(Full.bias.data$ES), mean)

library(ggplot2)
library(reshape)

##does this match up with Okada (2013)???
bndata = subset(Full.bias.data, Design=="BN1")
bndata = subset(bndata, Levels==4)
bndata = subset(bndata, stdev==1)
bndata = subset(bndata, Corr==0.0)

biasmeans = as.data.frame(tapply(bndata$Bias, list(bndata$ES, bndata$N), mean))
library(reshape)  
biasmeans = melt(biasmeans)
biasmeans$ES = rep(c("fes","fos","ges","pes"),16)
colnames(biasmeans)=c("N","Bias","ES")
ggplot(biasmeans)+geom_line(aes(x=N, y=Bias, group=ES, color=ES))
ggplot(biasmeans)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = F)
ggplot(biasmeans)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T)

ggplot(Full.bias.data)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES))
ggplot(bndata)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES))

SDmeans = as.data.frame(tapply(bndata$SD, list(bndata$ES, bndata$N), mean))
SDmeans = melt(SDmeans)
SDmeans$ES = rep(c("fes","fos","ges","pes"),16)
colnames(SDmeans)=c("N","SD","ES")
ggplot(SDmeans)+geom_line(aes(x=N, y=SD, group=ES, color=ES))
ggplot(SDmeans)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = F)
ggplot(SDmeans)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T)

ggplot(Full.bias.data)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES))
ggplot(bndata)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES))

RMSEmeans = as.data.frame(tapply(bndata$RMSE, list(bndata$ES, bndata$N), mean))
RMSEmeans = melt(RMSEmeans)
RMSEmeans$ES = rep(c("fes","fos","ges","pes"),16)
colnames(RMSEmeans)=c("N","RMSE","ES")
ggplot(RMSEmeans)+geom_line(aes(x=N, y=RMSE, group=ES, color=ES))
ggplot(RMSEmeans)+geom_smooth(aes(x=N, y=RMSE, group=ES, color=ES), se = F)
ggplot(RMSEmeans)+geom_smooth(aes(x=N, y=RMSE, group=ES, color=ES), se = T)

ggplot(Full.bias.data)+geom_smooth(aes(x=N, y=RMSE, group=ES, color=ES))
ggplot(bndata)+geom_smooth(aes(x=N, y=RMSE, group=ES, color=ES))
























biasmeans = as.data.frame(tapply(Full.bias.data$Bias, list(Full.bias.data$ES, Full.bias.data$N), mean))
library(reshape)  
biasmeans = melt(biasmeans)
biasmeans$ES = rep(c("fes","fos","ges","pes","pos"),16)
colnames(biasmeans)=c("N","Bias","ES")
ggplot(biasmeans)+geom_line(aes(x=N, y=Bias, group=ES, color=ES))

SDmeans = as.data.frame(tapply(Full.bias.data$SD, list(Full.bias.data$ES, Full.bias.data$N), mean))
SDmeans = melt(SDmeans)
SDmeans$ES = rep(c("fes","fos","ges","pes","pos"),16)
colnames(SDmeans)=c("N","SD","ES")
ggplot(SDmeans)+geom_line(aes(x=N, y=SD, group=ES, color=ES))

RMSEmeans = as.data.frame(tapply(Full.bias.data$RMSE, list(Full.bias.data$ES, Full.bias.data$N), mean))
RMSEmeans = melt(RMSEmeans)
RMSEmeans$ES = rep(c("fes","fos","ges","pes","pos"),16)
colnames(RMSEmeans)=c("N","RMSE","ES")
ggplot(RMSEmeans)+geom_line(aes(x=N, y=RMSE, group=ES, color=ES))









