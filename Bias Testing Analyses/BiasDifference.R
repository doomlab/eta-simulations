

setwd("C:/Users/John/Desktop")
source("BiasCalculate.R")


############################################################################# Diff between ES and Design
##Bias
model.allES.Bias = lm(Bias ~ ES*Design, data = Full.bias.data)
summary.aov(model.allES.Bias)
61.0/(61.0+438.1) #ES
24.7/(24.7+438.1) #Design
33.20/(33.20+438.1) #Interaction

##ES
tapply(Full.bias.data$Bias, list(Full.bias.data$ES), mean)
tapply(Full.bias.data$Bias, list(Full.bias.data$ES), sd)
pairwise.t.test(Full.bias.data$Bias, Full.bias.data$ES, 
                paired = F,
                var.equal = T,
                p.adjust.method = "bonferroni")
##Design
tapply(Full.bias.data$Bias, list(Full.bias.data$Design), mean)
tapply(Full.bias.data$Bias, list(Full.bias.data$Design), sd)
pairwise.t.test(Full.bias.data$Bias, Full.bias.data$Design, 
                paired = F,
                var.equal = T,
                p.adjust.method = "bonferroni")
##Interaction
tapply(Full.bias.data$Bias, list(Full.bias.data$ES, Full.bias.data$Design), mean)
tapply(Full.bias.data$Bias, list(Full.bias.data$ES, Full.bias.data$Design), sd)
splitfes = subset(Full.bias.data, ES=="fes")
splitges = subset(Full.bias.data, ES=="ges")
splitpes = subset(Full.bias.data, ES=="pes")
splitpos = subset(Full.bias.data, ES=="pos")
splitfos = subset(Full.bias.data, ES=="fos")
pairwise.t.test(splitfes$Bias, splitfes$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitges$Bias, splitges$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitpes$Bias, splitpes$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitpos$Bias, splitpos$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitfos$Bias, splitfos$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
############################################################################# Diff between ES and Design



############################################################################# FES
#BN1, BN2, RM1 together
FESdata = subset(Full.bias.data, ES=="fes")
FESBN1 = subset(FESdata, Design=="BN1")
FESRM2 = subset(FESdata, Design=="RM2")
FESMIX = subset(FESdata, Design=="MIX")
##BN1 Bias
model.FES.BN1.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = FESBN1)
summary(model.FES.BN1.Bias)
#RM2 Bias
model.FES.RM2.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = FESRM2)
summary(model.FES.RM2.Bias)
#MIX Bias
model.FES.MIX.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = FESMIX)
summary(model.FES.MIX.Bias)
#RM2 Split by SD
FESRM2SD5 = subset(FESRM2, stdev == 5)
FESRM2SD3 = subset(FESRM2, stdev == 3)
FESRM2SD1 = subset(FESRM2, stdev == 1)
model.FES.RM2.Bias.SD5 = lm(Bias ~ N*Levels*Corr, data = FESRM2SD5)
summary(model.FES.RM2.Bias.SD5)
model.FES.RM2.Bias.SD3 = lm(Bias ~ N*Levels*Corr, data = FESRM2SD3)
summary(model.FES.RM2.Bias.SD3)
model.FES.RM2.Bias.SD1 = lm(Bias ~ N*Levels*Corr, data = FESRM2SD1)
summary(model.FES.RM2.Bias.SD1)
#RM2 split by SD split by levels
FESRM2SD5$Levelslo = FESRM2SD5$Levels + sd(FESRM2SD5$Levels)
FESRM2SD5$Levelshi = FESRM2SD5$Levels - sd(FESRM2SD5$Levels)
FESRM2SD3$Levelslo = FESRM2SD3$Levels + sd(FESRM2SD3$Levels)
FESRM2SD3$Levelshi = FESRM2SD3$Levels - sd(FESRM2SD3$Levels)
FESRM2SD1$Levelslo = FESRM2SD1$Levels + sd(FESRM2SD1$Levels)
FESRM2SD1$Levelshi = FESRM2SD1$Levels - sd(FESRM2SD1$Levels)
model.FES.RM2.Bias.SD5.levello = lm(Bias ~ N*Levelslo, data = FESRM2SD5)
summary(model.FES.RM2.Bias.SD5.levello)
model.FES.RM2.Bias.SD5.levelhi = lm(Bias ~ N*Levelshi, data = FESRM2SD5)
summary(model.FES.RM2.Bias.SD5.levelhi)
model.FES.RM2.Bias.SD3.levello = lm(Bias ~ N*Levelslo, data = FESRM2SD3)
summary(model.FES.RM2.Bias.SD3.levello)
model.FES.RM2.Bias.SD3.levelhi = lm(Bias ~ N*Levelshi, data = FESRM2SD3)
summary(model.FES.RM2.Bias.SD3.levelhi)
model.FES.RM2.Bias.SD1.levello = lm(Bias ~ N*Levelslo, data = FESRM2SD1)
summary(model.FES.RM2.Bias.SD1.levello)
model.FES.RM2.Bias.SD1.levelhi = lm(Bias ~ N*Levelshi, data = FESRM2SD1)
summary(model.FES.RM2.Bias.SD1.levelhi)
############################################################################# FES


############################################################################# GES
#BN1, BN2, RM1, MIX together
GESdata = subset(Full.bias.data, ES=="ges")
GESBN1 = subset(GESdata, Design=="BN1")
GESRM2 = subset(GESdata, Design=="RM2")
##BN1 Bias
model.GES.BN1.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = GESBN1)
summary(model.GES.BN1.Bias)
#RM2 Bias
model.GES.RM2.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = GESRM2)
summary(model.GES.RM2.Bias)
#RM2 Split by SD
GESRM2SD5 = subset(GESRM2, stdev == 5)
GESRM2SD3 = subset(GESRM2, stdev == 3)
GESRM2SD1 = subset(GESRM2, stdev == 1)
model.GES.RM2.Bias.SD5 = lm(Bias ~ N*Levels*Corr, data = GESRM2SD5)
summary(model.GES.RM2.Bias.SD5)
model.GES.RM2.Bias.SD3 = lm(Bias ~ N*Levels*Corr, data = GESRM2SD3)
summary(model.GES.RM2.Bias.SD3)
model.GES.RM2.Bias.SD1 = lm(Bias ~ N*Levels*Corr, data = GESRM2SD1)
summary(model.GES.RM2.Bias.SD1)
#RM2 split by SD split by levels
GESRM2SD5$Levelslo = GESRM2SD5$Levels + sd(GESRM2SD5$Levels)
GESRM2SD5$Levelshi = GESRM2SD5$Levels - sd(GESRM2SD5$Levels)
GESRM2SD3$Levelslo = GESRM2SD3$Levels + sd(GESRM2SD3$Levels)
GESRM2SD3$Levelshi = GESRM2SD3$Levels - sd(GESRM2SD3$Levels)
GESRM2SD1$Levelslo = GESRM2SD1$Levels + sd(GESRM2SD1$Levels)
GESRM2SD1$Levelshi = GESRM2SD1$Levels - sd(GESRM2SD1$Levels)
model.GES.RM2.Bias.SD5.levello = lm(Bias ~ N*Levelslo, data = GESRM2SD5)
summary(model.GES.RM2.Bias.SD5.levello)
model.GES.RM2.Bias.SD5.levelhi = lm(Bias ~ N*Levelshi, data = GESRM2SD5)
summary(model.GES.RM2.Bias.SD5.levelhi)
model.GES.RM2.Bias.SD3.levello = lm(Bias ~ N*Levelslo, data = GESRM2SD3)
summary(model.GES.RM2.Bias.SD3.levello)
model.GES.RM2.Bias.SD3.levelhi = lm(Bias ~ N*Levelshi, data = GESRM2SD3)
summary(model.GES.RM2.Bias.SD3.levelhi)
model.GES.RM2.Bias.SD1.levello = lm(Bias ~ N*Levelslo, data = GESRM2SD1)
summary(model.GES.RM2.Bias.SD1.levello)
model.GES.RM2.Bias.SD1.levelhi = lm(Bias ~ N*Levelshi, data = GESRM2SD1)
summary(model.GES.RM2.Bias.SD1.levelhi)
############################################################################# GES


############################################################################# PES
#BN1, BN2 together
#RM1, RM2, MIX together
PESdata = subset(Full.bias.data, ES=="pes")
PESBN1 = subset(PESdata, Design=="BN1")
PESRM2 = subset(PESdata, Design=="RM2")
##BN1 Bias
model.PES.BN1.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = PESBN1)
summary(model.PES.BN1.Bias)
#RM2 Bias
model.PES.RM2.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = PESRM2)
summary(model.PES.RM2.Bias)
############################################################################# PES


############################################################################# FOS
#BN1, BN2 together
FOSdata = subset(Full.bias.data, ES=="fos")
FOSBN1 = subset(FOSdata, Design=="BN1")
FOSRM1 = subset(FOSdata, Design=="RM1")
FOSRM2 = subset(FOSdata, Design=="RM2")
FOSMIX = subset(FOSdata, Design=="MIX")
##BN1 Bias
model.FOS.BN1.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = FOSBN1)
summary(model.FOS.BN1.Bias)
#RM1 Bias
model.FOS.RM1.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = FOSRM1)
summary(model.FOS.RM1.Bias)
#RM2 Bias
model.FOS.RM2.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = FOSRM2)
summary(model.FOS.RM2.Bias)
#MIX Bias
model.FOS.MIX.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = FOSMIX)
summary(model.FOS.MIX.Bias)
#RM2 Split by SD
FOSRM2SD5 = subset(FOSRM2, stdev == 5)
FOSRM2SD3 = subset(FOSRM2, stdev == 3)
FOSRM2SD1 = subset(FOSRM2, stdev == 1)
model.FOS.RM2.Bias.SD5 = lm(Bias ~ N*Levels*Corr, data = FOSRM2SD5)
summary(model.FOS.RM2.Bias.SD5)
model.FOS.RM2.Bias.SD3 = lm(Bias ~ N*Levels*Corr, data = FOSRM2SD3)
summary(model.FOS.RM2.Bias.SD3)
model.FOS.RM2.Bias.SD1 = lm(Bias ~ N*Levels*Corr, data = FOSRM2SD1)
summary(model.FOS.RM2.Bias.SD1)
#RM2 split by SD split by levels
FOSRM2SD3$Levelslo = FOSRM2SD3$Levels + sd(FOSRM2SD3$Levels)
FOSRM2SD3$Levelshi = FOSRM2SD3$Levels - sd(FOSRM2SD3$Levels)
model.FOS.RM2.Bias.SD3.levello = lm(Bias ~ N*Levelslo, data = FOSRM2SD3)
summary(model.FOS.RM2.Bias.SD3.levello)
model.FOS.RM2.Bias.SD3.levelhi = lm(Bias ~ N*Levelshi, data = FOSRM2SD3)
summary(model.FOS.RM2.Bias.SD3.levelhi)
############################################################################# FOS



############################################################################# POS
POSdata = subset(Full.bias.data, ES=="pos")
POSBN2 = subset(POSdata, Design=="BN2")
POSRM2 = subset(POSdata, Design=="RM2")
POSMIX = subset(POSdata, Design=="MIX")
#BN2 Bias
model.POS.BN2.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = POSBN2)
summary(model.POS.BN2.Bias)
#RM2 Bias
model.POS.RM2.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = POSRM2)
summary(model.POS.RM2.Bias)
#MIX Bias
model.POS.MIX.Bias = lm(Bias ~ N*Levels*Corr*stdev, data = POSMIX)
summary(model.POS.MIX.Bias)
#RM2 Split by SD
POSRM2SD5 = subset(POSRM2, stdev == 5)
POSRM2SD3 = subset(POSRM2, stdev == 3)
POSRM2SD1 = subset(POSRM2, stdev == 1)
model.POS.RM2.Bias.SD5 = lm(Bias ~ N*Levels*Corr, data = POSRM2SD5)
summary(model.POS.RM2.Bias.SD5)
model.POS.RM2.Bias.SD3 = lm(Bias ~ N*Levels*Corr, data = POSRM2SD3)
summary(model.POS.RM2.Bias.SD3)
model.POS.RM2.Bias.SD1 = lm(Bias ~ N*Levels*Corr, data = POSRM2SD1)
summary(model.POS.RM2.Bias.SD1)
#MIX Split by SD
POSMIXSD5 = subset(POSMIX, stdev == 5)
POSMIXSD3 = subset(POSMIX, stdev == 3)
POSMIXSD1 = subset(POSMIX, stdev == 1)
model.POS.MIX.Bias.SD5 = lm(Bias ~ N*Levels*Corr, data = POSMIXSD5)
summary(model.POS.MIX.Bias.SD5)
model.POS.MIX.Bias.SD3 = lm(Bias ~ N*Levels*Corr, data = POSMIXSD3)
summary(model.POS.MIX.Bias.SD3)
model.POS.MIX.Bias.SD1 = lm(Bias ~ N*Levels*Corr, data = POSMIXSD1)
summary(model.POS.MIX.Bias.SD1)
#MIX split by SD split by levels
POSRM2SD3$Levelslo = POSRM2SD3$Levels + sd(POSRM2SD3$Levels)
POSRM2SD3$Levelshi = POSRM2SD3$Levels - sd(POSRM2SD3$Levels)
POSRM2SD1$Levelslo = POSRM2SD1$Levels + sd(POSRM2SD1$Levels)
POSRM2SD1$Levelshi = POSRM2SD1$Levels - sd(POSRM2SD1$Levels)
model.POS.RM2.Bias.SD3.levello = lm(Bias ~ N*Levelslo, data = POSRM2SD3)
summary(model.POS.RM2.Bias.SD3.levello)
model.POS.RM2.Bias.SD3.levelhi = lm(Bias ~ N*Levelshi, data = POSRM2SD3)
summary(model.POS.RM2.Bias.SD3.levelhi)
model.POS.RM2.Bias.SD1.levello = lm(Bias ~ N*Levelslo, data = POSRM2SD1)
summary(model.POS.RM2.Bias.SD1.levello)
model.POS.RM2.Bias.SD1.levelhi = lm(Bias ~ N*Levelshi, data = POSRM2SD1)
summary(model.POS.RM2.Bias.SD1.levelhi)
############################################################################# POS



