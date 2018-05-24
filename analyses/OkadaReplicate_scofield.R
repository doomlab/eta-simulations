
library(data.table)
mydata = as.data.table(matrix(ncol=9, nrow=50000))
colnames(mydata)=c("N","dfm","dfr","ssm.p","ssm.main","ssr.p","ssr.main","Fv","ges")
round=0
nsim=10000
for(i in 1:nsim){
  round=round+1
  means = c(2.5, 3.0, 3.5, 4.0)
  dataset20 = as.data.table(rmvnorm(20, means))
  dataset20$partno = 1:nrow(dataset20)
  long20 = melt(dataset20,
                id = "partno",
                measured = colnames(long20[1:(ncol(long20)-1), ]))
  colnames(long20)=c("partno","variable","value")
  bnoutput = ezANOVA(data = long20,
                     wid = partno,
                     between = variable,
                     dv = value, 
                     type = 3, 
                     detailed = T)
  mydata$dfm[round] = bnoutput$ANOVA$DFn[2]
  mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
  mydata$ssm.p[round] = bnoutput$ANOVA$SSn[1]
  mydata$ssm.main[round] = bnoutput$ANOVA$SSn[2]
  mydata$ssr.p[round] = bnoutput$ANOVA$SSd[1]
  mydata$ssr.main[round] = bnoutput$ANOVA$SSd[2]
  mydata$Fv[round] = bnoutput$ANOVA$F[2]
  mydata$ges[round] = bnoutput$ANOVA$ges[2]
}

for(i in 1:nsim){
  round=round+1
  means = c(2.5, 3.0, 3.5, 4.0)
  dataset35 = as.data.table(rmvnorm(35, means))
  dataset35$partno = 1:nrow(dataset35)
  long35 = melt(dataset35,
                id = "partno",
                measured = colnames(long35[1:(ncol(long35)-1), ]))
  colnames(long35)=c("partno","variable","value")
  bnoutput = ezANOVA(data = long20,
                     wid = partno,
                     between = variable,
                     dv = value, 
                     type = 3, 
                     detailed = T)
  mydata$dfm[round] = bnoutput$ANOVA$DFn[2]
  mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
  mydata$ssm.p[round] = bnoutput$ANOVA$SSn[1]
  mydata$ssm.main[round] = bnoutput$ANOVA$SSn[2]
  mydata$ssr.p[round] = bnoutput$ANOVA$SSd[1]
  mydata$ssr.main[round] = bnoutput$ANOVA$SSd[2]
  mydata$Fv[round] = bnoutput$ANOVA$F[2]
  mydata$ges[round] = bnoutput$ANOVA$ges[2]
}
for(i in 1:nsim){
  round=round+1
  means = c(2.5, 3.0, 3.5, 4.0)
  dataset50 = as.data.table(rmvnorm(50, means))
  dataset50$partno = 1:nrow(dataset50)
  long50 = melt(dataset50,
                id = "partno",
                measured = colnames(long50[1:(ncol(long50)-1), ]))
  colnames(long50)=c("partno","variable","value")
  bnoutput = ezANOVA(data = long20,
                     wid = partno,
                     between = variable,
                     dv = value, 
                     type = 3, 
                     detailed = T)
  mydata$dfm[round] = bnoutput$ANOVA$DFn[2]
  mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
  mydata$ssm.p[round] = bnoutput$ANOVA$SSn[1]
  mydata$ssm.main[round] = bnoutput$ANOVA$SSn[2]
  mydata$ssr.p[round] = bnoutput$ANOVA$SSd[1]
  mydata$ssr.main[round] = bnoutput$ANOVA$SSd[2]
  mydata$Fv[round] = bnoutput$ANOVA$F[2]
  mydata$ges[round] = bnoutput$ANOVA$ges[2]
}
for(i in 1:nsim){
  round=round+1
  means = c(2.5, 3.0, 3.5, 4.0)
  dataset75 = as.data.table(rmvnorm(75, means))
  dataset75$partno = 1:nrow(dataset75)
  long75 = melt(dataset75,
                id = "partno",
                measured = colnames(long20[1:(ncol(long75)-1), ]))
  colnames(long75)=c("partno","variable","value")
  bnoutput = ezANOVA(data = long20,
                     wid = partno,
                     between = variable,
                     dv = value, 
                     type = 3, 
                     detailed = T)
  mydata$dfm[round] = bnoutput$ANOVA$DFn[2]
  mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
  mydata$ssm.p[round] = bnoutput$ANOVA$SSn[1]
  mydata$ssm.main[round] = bnoutput$ANOVA$SSn[2]
  mydata$ssr.p[round] = bnoutput$ANOVA$SSd[1]
  mydata$ssr.main[round] = bnoutput$ANOVA$SSd[2]
  mydata$Fv[round] = bnoutput$ANOVA$F[2]
  mydata$ges[round] = bnoutput$ANOVA$ges[2]
}
for(i in 1:nsim){
  round=round+1
  means = c(2.5, 3.0, 3.5, 4.0)
  dataset100 = as.data.table(rmvnorm(100, means))
  dataset100$partno = 1:nrow(dataset100)
  long100 = melt(dataset100,
                 id = "partno",
                 measured = colnames(long100[1:(ncol(long100)-1), ]))
  colnames(long100)=c("partno","variable","value")
  bnoutput = ezANOVA(data = long20,
                     wid = partno,
                     between = variable,
                     dv = value, 
                     type = 3, 
                     detailed = T)
  mydata$dfm[round] = bnoutput$ANOVA$DFn[2]
  mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
  mydata$ssm.p[round] = bnoutput$ANOVA$SSn[1]
  mydata$ssm.main[round] = bnoutput$ANOVA$SSn[2]
  mydata$ssr.p[round] = bnoutput$ANOVA$SSd[1]
  mydata$ssr.main[round] = bnoutput$ANOVA$SSd[2]
  mydata$Fv[round] = bnoutput$ANOVA$F[2]
  mydata$ges[round] = bnoutput$ANOVA$ges[2]
}

####################################################################################
####################################################################################

write.csv(mydata, file="OkadaData.csv")
OkadaData <- read.csv("C:/Users/John/Desktop/OkadaData.csv")
fulldata = OkadaData

##calculate ES
fulldata$pes = fulldata$ssm.main / (fulldata$ssm.main + fulldata$ssr.main)
fulldata$fes = fulldata$ssm.main / (fulldata$ssm.main + fulldata$ssr.main)
fulldata$fos = (fulldata$dfm*((fulldata$ssm.main/fulldata$dfm)-(fulldata$ssr.main/fulldata$dfr)))/
  ((fulldata$ssm.main + fulldata$ssr.main)+(fulldata$ssr.main/fulldata$dfr))

##BN1 4 levels
fulldata$N = c(rep(20,1000),rep(35,1000),rep(50,1000),rep(75,1000),rep(100,1000))


N = rep(c(25,35,50,75,100),4)
ES = as.data.frame(c(rep("ges",5),rep("pes",5),rep("fes",5),rep("fos",5)))
biasdata = cbind(N,ES)
colnames(biasdata) = c("N", "ES")
biasdata$Bias = 0
biasdata$SD = 0
biasdata$RMSE = 0

dat4 = c(2.5,3.0,3.5,4.0)
meandat4 = mean(dat4)
sig4 = sum((dat4-meandat4)^2)/length(dat4)
eta2p4 = sig4/(sig4+1)
biasdata$eta2p = eta2p4

round=0
start = 1
finish=1000
nsim=5
for(i in 1:nsim){
  round=round+1
  biasdata$Bias[round] = mean(fulldata$ges[start:finish]) - eta2p4
  biasdata$SD[round] = sqrt(sum((fulldata$ges[start:finish]-mean(fulldata$ges[start:finish]))^2)/1000)
  biasdata$RMSE[round] = sqrt(sum((fulldata$ges[start:finish]-eta2p4)^2)/1000)
  start=start+1000
  finish=finish+1000
}
start = 1
finish=1000
for(i in 1:nsim){
  round=round+1
  biasdata$Bias[round] = mean(fulldata$pes[start:finish]) - eta2p4
  biasdata$SD[round] = sqrt(sum((fulldata$pes[start:finish]-mean(fulldata$pes[start:finish]))^2)/1000)
  biasdata$RMSE[round] = sqrt(sum((fulldata$pes[start:finish]-eta2p4)^2)/1000)
  start=start+1000
  finish=finish+1000
}
start = 1
finish=1000
for(i in 1:nsim){
  round=round+1
  biasdata$Bias[round] = mean(fulldata$fes[start:finish]) - eta2p4
  biasdata$SD[round] = sqrt(sum((fulldata$fes[start:finish]-mean(fulldata$fes[start:finish]))^2)/1000)
  biasdata$RMSE[round] = sqrt(sum((fulldata$fes[start:finish]-eta2p4)^2)/1000)
  start=start+1000
  finish=finish+1000
}
start = 1
finish=1000
for(i in 1:nsim){
  round=round+1
  biasdata$Bias[round] = mean(fulldata$fos[start:finish]) - eta2p4
  biasdata$SD[round] = sqrt(sum((fulldata$fos[start:finish]-mean(fulldata$fos[start:finish]))^2)/1000)
  biasdata$RMSE[round] = sqrt(sum((fulldata$fos[start:finish]-eta2p4)^2)/1000)
  start=start+1000
  finish=finish+1000
}

write.csv(biasdata,file="biasdata.csv")

library(ggplot2)
############################
biasmeans = as.data.frame(tapply(biasdata$Bias, list(biasdata$ES, biasdata$N), mean))
library(reshape)  
biasmeans = melt(biasmeans)
biasmeans$ES = rep(c("ges","pes","fes","fos"),5)
colnames(biasmeans)=c("N","Bias","ES")
ggplot(biasmeans)+geom_line(aes(x=N, y=Bias, group=ES, color=ES))
ggplot(biasmeans)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = F)

SDmeans = as.data.frame(tapply(biasdata$SD, list(biasdata$ES, biasdata$N), mean))
SDmeans = melt(SDmeans)
SDmeans$ES = rep(c("ges","pes","fes","fos"),5)
colnames(SDmeans)=c("N","SD","ES")
ggplot(SDmeans)+geom_line(aes(x=N, y=SD, group=ES, color=ES))
ggplot(SDmeans)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = F)

RMSEmeans = as.data.frame(tapply(biasdata$RMSE, list(biasdata$ES, biasdata$N), mean))
RMSEmeans = melt(RMSEmeans)
RMSEmeans$ES = rep(c("ges","pes","fes","fos"),5)
colnames(RMSEmeans)=c("N","RMSE","ES")
ggplot(RMSEmeans)+geom_line(aes(x=N, y=RMSE, group=ES, color=ES))
ggplot(RMSEmeans)+geom_smooth(aes(x=N, y=RMSE, group=ES, color=ES), se = F)
