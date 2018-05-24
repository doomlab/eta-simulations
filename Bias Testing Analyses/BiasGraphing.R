


setwd("C:/Users/John/Desktop")
source("BiasCalculate.R")

library(ggplot2)
library(reshape)
source("multi.plot.function.R")


################################################################################# Do we see Okada pattern
x = subset(Full.bias.data, Levels ==4)
xy = subset(x, Design=="BN1")
xyz = subset(xy, stdev=="1")
xyza = subset(xyz, ES=="fes")
xyzb = subset(xyz, ES=="ges")
xyzc = subset(xyz, ES=="pes")
xyzd = subset(xyz, ES=="fos")

p1 = ggplot(xyza)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) + labs(title="FES")
p2 = ggplot(xyzb)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) + labs(title="GES")
p3 = ggplot(xyzc)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) + labs(title="PES")
p4 = ggplot(xyzd)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) + labs(title="FOS")
multiplot(p1,p2,p3,p4,cols = 2)
################################################################################# Do we see Okada pattern



##################################################################################### Bias
## Full Bias across Design
barbias = ggplot(Full.bias.data, aes(Design, Bias, fill = ES))
barbias +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Design") +ylab("Bias") + 
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))


###########FES
BiasFES = subset(Full.bias.data, ES == "fes")
#BN1, BN2, RM1 together
BiasFES.BN1 = subset(BiasFES, Design =="BN1")
BiasFES.RM2 = subset(BiasFES, Design =="RM2")
BiasFES.MIX = subset(BiasFES, Design =="MIX")
bfesbn1 = ggplot(BiasFES.BN1)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="BN1, BN2, RM1")
bfesrm2 = ggplot(BiasFES.RM2)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="RM2")
bfesmix = ggplot(BiasFES.MIX)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="MIX")
multiplot(bfesbn1, bfesrm2, bfesmix, cols = 2)
###########FES

###########GES
BiasGES = subset(Full.bias.data, ES == "ges")
#BN1, BN2, RM1, MIX together
BiasGES.BN1 = subset(BiasGES, Design =="BN1")
BiasGES.RM2 = subset(BiasGES, Design =="RM2")
bgesbn1 = ggplot(BiasGES.BN1)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="BN1, BN2, RM1, MIX")
bgesrm2 = ggplot(BiasGES.RM2)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="RM2")
multiplot(bgesbn1, bgesrm2, cols = 2)
###########GES

###########PES
BiasPES = subset(Full.bias.data, ES == "pes")
#BN1, BN2 together
#RM1, RM2, MIX together
BiasPES.BN1 = subset(BiasPES, Design =="BN1")
BiasPES.RM2 = subset(BiasPES, Design =="RM2")
bpesbn1 = ggplot(BiasPES.BN1)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="BN1, BN2")
bpesrm2 = ggplot(BiasPES.RM2)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="RM1, RM2, MIX")
multiplot(bpesbn1, bpesrm2, cols = 2)
###########PES

###########FOS
BiasFOS = subset(Full.bias.data, ES == "fos")
#BN1, BN2 together
BiasFOS.BN1 = subset(BiasFOS, Design =="BN1")
BiasFOS.RM1 = subset(BiasFOS, Design =="RM1")
BiasFOS.RM2 = subset(BiasFOS, Design =="RM2")
BiasFOS.MIX = subset(BiasFOS, Design =="MIX")
bfosbn1 = ggplot(BiasFOS.BN1)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="BN1, BN2")
bfosrm1 = ggplot(BiasFOS.RM1)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="RM1")
bfosrm2 = ggplot(BiasFOS.RM2)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="RM2")
bfosmix = ggplot(BiasFOS.MIX)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="MIX")
multiplot(bfosbn1, bfosrm1, bfosrm2, bfosmix, cols = 2)
###########FOS

###########POS
BiasPOS = subset(Full.bias.data, ES == "pos")
BiasPOS.BN2 = subset(BiasPOS, Design =="BN2")
BiasPOS.RM2 = subset(BiasPOS, Design =="RM2")
BiasPOS.MIX = subset(BiasPOS, Design =="MIX")
bposbn2 = ggplot(BiasPOS.BN2)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="BN2")
bposrm2 = ggplot(BiasPOS.RM2)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="RM2")
bposmix = ggplot(BiasPOS.MIX)+geom_smooth(aes(x=N, y=Bias, group=stdev, color=stdev), se = T) + labs(title="MIX")
multiplot(bposbn2, bposrm2, bposmix, cols = 2)
###########POS

##################################################################################### Bias



##################################################################################### SD
## Full SD across Design
barsd = ggplot(Full.bias.data, aes(Design, SD, fill = ES))
barsd +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Design") +ylab("SD") + 
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))

###########FES
SDFES = subset(Full.bias.data, ES == "fes")
#BN1, BN2, RM1 together
SDFES.BN1 = subset(SDFES, Design =="BN1")
SDFES.RM2 = subset(SDFES, Design =="RM2")
SDFES.MIX = subset(SDFES, Design =="MIX")
SDFES.BN1.SD1 = subset(SDFES.BN1, stdev == 1)
SDFES.RM2.SD1 = subset(SDFES.RM2, stdev == 1)
SDFES.MIX.SD1 = subset(SDFES.MIX, stdev == 1)
sdfesbn1 = ggplot(SDFES.BN1.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="BN1, BN2, RM1")
sdfesrm2 = ggplot(SDFES.RM2.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="RM2")
sdfesmix = ggplot(SDFES.MIX.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="MIX")
multiplot(sdfesbn1, sdfesrm2, sdfesmix, cols = 2)
###########FES

###########GES
SDGES = subset(Full.bias.data, ES == "ges")
#BN1, BN2, RM1, MIX together
SDGES.BN1 = subset(SDGES, Design =="BN1")
SDGES.RM2 = subset(SDGES, Design =="RM2")
SDGES.BN1.SD1 = subset(SDGES.BN1, stdev == 1)
SDGES.RM2.SD1 = subset(SDGES.RM2, stdev == 1)
sdgesbn1 = ggplot(SDGES.BN1.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="BN1, BN2, RM1, MIX")
sdgesrm2 = ggplot(SDGES.RM2.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="RM2")
multiplot(sdgesbn1, sdgesrm2, cols = 2)
###########GES

###########PES
SDPES = subset(Full.bias.data, ES == "pes")
#BN1, BN2 together
#RM1, RM2, MIX together
SDPES.BN1 = subset(SDPES, Design =="BN1")
SDPES.RM2 = subset(SDPES, Design =="RM2")
SDPES.BN1.SD1 = subset(SDPES.BN1, stdev == 1)
SDPES.RM2.SD1 = subset(SDPES.RM2, stdev == 1)
sdpesbn1 = ggplot(SDPES.BN1.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="BN1, BN2")
sdpesrm2 = ggplot(SDPES.RM2.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="RM1, RM2, MIX")
multiplot(sdpesbn1, sdpesrm2, cols = 2)
###########PES

###########FOS
SDFOS = subset(Full.bias.data, ES == "fos")
#BN1, BN2 together
SDFOS.BN1 = subset(SDFOS, Design =="BN1")
SDFOS.RM1 = subset(SDFOS, Design =="RM1")
SDFOS.RM2 = subset(SDFOS, Design =="RM2")
SDFOS.MIX = subset(SDFOS, Design =="MIX")
SDFOS.BN1.SD1 = subset(SDFOS.BN1, stdev == 1)
SDFOS.RM1.SD1 = subset(SDFOS.RM1, stdev == 1)
SDFOS.RM2.SD1 = subset(SDFOS.RM2, stdev == 1)
SDFOS.MIX.SD1 = subset(SDFOS.MIX, stdev == 1)
sdfosbn1 = ggplot(SDFOS.BN1.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="BN1, BN2")
sdfosrm1 = ggplot(SDFOS.RM1.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="RM1")
sdfosrm2 = ggplot(SDFOS.RM2.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="RM2")
sdfosmix = ggplot(SDFOS.MIX.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="MIX")
multiplot(sdfosbn1, sdfosrm1, sdfosrm2, sdfosmix, cols = 2)
###########FOS

###########POS
SDPOS = subset(Full.bias.data, ES == "pos")
SDPOS.BN2 = subset(SDPOS, Design =="BN2")
SDPOS.RM2 = subset(SDPOS, Design =="RM2")
SDPOS.MIX = subset(SDPOS, Design =="MIX")
SDPOS.BN2.SD1 = subset(SDPOS.BN2, stdev == 1)
SDPOS.RM2.SD1 = subset(SDPOS.RM2, stdev == 1)
SDPOS.MIX.SD1 = subset(SDPOS.MIX, stdev == 1)
sdposbn2 = ggplot(SDPOS.BN2.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="BN2")
sdposrm2 = ggplot(SDPOS.RM2.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="RM2")
sdposmix = ggplot(SDPOS.MIX.SD1)+geom_smooth(aes(x=N, y=SD, group=Corr, color=Corr), se = T) + labs(title="MIX")
multiplot(sdposbn2, sdposrm2, sdposmix, cols = 2)
###########POS

##################################################################################### SD



##################################################################################### RMSE
## Full RMSE across Design
barrmse = ggplot(Full.bias.data, aes(Design, RMSE, fill = ES))
barrmse +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Design") +ylab("RMSE") + 
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))

########### FES, FOS, GES
RMSEFES = subset(Full.bias.data, ES == "fes")
#BN1, BN2, RM1, MIX together
RMSEFES.BN1 = subset(RMSEFES, Design =="BN1")
RMSEFES.RM2 = subset(RMSEFES, Design =="RM2")
rfesbn1 = ggplot(RMSEFES.BN1)+geom_smooth(aes(x=N, y=RMSE, group=stdev, color=stdev), se = T) + labs(title="BN1, BN2, RM1, MIX")
rfesrm2 = ggplot(RMSEFES.RM2)+geom_smooth(aes(x=N, y=RMSE, group=stdev, color=stdev), se = T) + labs(title="RM2")
multiplot(rfesbn1, rfesrm2, cols = 2)
########### FES, FOS, GES

########## PES
RMSEPES = subset(Full.bias.data, ES == "pes")
#BN1, BN2 together
#RM1, RM2, MIX together
RMSEPES.BN1 = subset(RMSEPES, Design =="BN1")
RMSEPES.RM2 = subset(RMSEPES, Design =="RM2")
rpesbn1 = ggplot(RMSEPES.BN1)+geom_smooth(aes(x=N, y=RMSE, group=stdev, color=stdev), se = T) + labs(title="BN1, BN2")
rpesrm2 = ggplot(RMSEPES.RM2)+geom_smooth(aes(x=N, y=RMSE, group=stdev, color=stdev), se = T) + labs(title="RM1, RM2, MIX")
multiplot(rpesbn1, rpesrm2, cols = 2)
########## PES

########## POS
RMSEPOS = subset(Full.bias.data, ES == "pos")
RMSEPOS.BN2 = subset(RMSEPOS, Design =="BN2")
RMSEPOS.RM2 = subset(RMSEPOS, Design =="RM2")
RMSEPOS.MIX = subset(RMSEPOS, Design =="MIX")
rposbn2 = ggplot(RMSEPOS.BN2)+geom_smooth(aes(x=N, y=RMSE, group=stdev, color=stdev), se = T) + labs(title="BN2")
rposrm2 = ggplot(RMSEPOS.RM2)+geom_smooth(aes(x=N, y=RMSE, group=stdev, color=stdev), se = T) + labs(title="RM2")
rposmix = ggplot(RMSEPOS.MIX)+geom_smooth(aes(x=N, y=RMSE, group=stdev, color=stdev), se = T) + labs(title="MIX")
multiplot(rposbn2, rposrm2, rposmix, cols = 2)
########## POS

##################################################################################### RMSE





