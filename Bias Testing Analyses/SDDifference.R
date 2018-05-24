

setwd("C:/Users/John/Desktop")
source("BiasCalculate.R")

Full.SD.data = Full.bias.data
############################################################################# Diff in ES and Design
##SD
model.allES.SD = lm(SD ~ ES*Design, data = Full.SD.data)
summary.aov(model.allES.SD)
1.543/(1.543+2.938) #ES
1.174/(1.174+2.938) #Design
1.012/(1.012+2.938) #Interaction

##ES
tapply(Full.SD.data$SD, list(Full.SD.data$ES), mean)
tapply(Full.SD.data$SD, list(Full.SD.data$ES), sd)
pairwise.t.test(Full.SD.data$SD, Full.SD.data$ES, 
                paired = F,
                var.equal = T,
                p.adjust.method = "bonferroni")
##Design
tapply(Full.SD.data$SD, list(Full.SD.data$Design), mean)
tapply(Full.SD.data$SD, list(Full.SD.data$Design), sd)
pairwise.t.test(Full.SD.data$SD, Full.SD.data$Design, 
                paired = F,
                var.equal = T,
                p.adjust.method = "bonferroni")
##Interaction
tapply(Full.SD.data$SD, list(Full.SD.data$ES, Full.SD.data$Design), mean)
tapply(Full.SD.data$SD, list(Full.SD.data$ES, Full.SD.data$Design), sd)
splitfes = subset(Full.SD.data, ES=="fes")
splitges = subset(Full.SD.data, ES=="ges")
splitpes = subset(Full.SD.data, ES=="pes")
splitpos = subset(Full.SD.data, ES=="pos")
splitfos = subset(Full.SD.data, ES=="fos")
pairwise.t.test(splitfes$SD, splitfes$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitges$SD, splitges$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitpes$SD, splitpes$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitpos$SD, splitpos$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitfos$SD, splitfos$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
############################################################################# Diff in ES and Design



############################################################################# FES
#BN1, BN2, RM1 together
FESdata = subset(Full.SD.data, ES=="fes")
FESBN1 = subset(FESdata, Design=="BN1")
FESRM2 = subset(FESdata, Design=="RM2")
FESMIX = subset(FESdata, Design=="MIX")
##BN1 SD
model.FES.BN1.SD = lm(SD ~ N*Levels*Corr*stdev, data = FESBN1)
summary(model.FES.BN1.SD)
#RM2 SD
model.FES.RM2.SD = lm(SD ~ N*Levels*Corr*stdev, data = FESRM2)
summary(model.FES.RM2.SD)
#MIX SD
model.FES.MIX.SD = lm(SD ~ N*Levels*Corr*stdev, data = FESMIX)
summary(model.FES.MIX.SD)

#BN1 SD split by SD
FESBN1SD5 = subset(FESBN1, stdev == 5)
FESBN1SD3 = subset(FESBN1, stdev == 3)
FESBN1SD1 = subset(FESBN1, stdev == 1)
model.FES.BN1.SD.SD5 = lm(SD ~ N*Levels*Corr, data = FESBN1SD5)
summary(model.FES.BN1.SD.SD5)
model.FES.BN1.SD.SD3 = lm(SD ~ N*Levels*Corr, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3)
model.FES.BN1.SD.SD1 = lm(SD ~ N*Levels*Corr, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1)
#RM2 SD split by SD
FESRM2SD5 = subset(FESRM2, stdev == 5)
FESRM2SD3 = subset(FESRM2, stdev == 3)
FESRM2SD1 = subset(FESRM2, stdev == 1)
model.FES.RM2.SD.SD5 = lm(SD ~ N*Levels*Corr, data = FESRM2SD5)
summary(model.FES.RM2.SD.SD5)
model.FES.RM2.SD.SD3 = lm(SD ~ N*Levels*Corr, data = FESRM2SD3)
summary(model.FES.RM2.SD.SD3)
model.FES.RM2.SD.SD1 = lm(SD ~ N*Levels*Corr, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1)
#MIX SD split by SD
FESMIXSD5 = subset(FESMIX, stdev == 5)
FESMIXSD3 = subset(FESMIX, stdev == 3)
FESMIXSD1 = subset(FESMIX, stdev == 1)
model.FES.MIX.SD.SD5 = lm(SD ~ N*Levels*Corr, data = FESMIXSD5)
summary(model.FES.MIX.SD.SD5)
model.FES.MIX.SD.SD3 = lm(SD ~ N*Levels*Corr, data = FESMIXSD3)
summary(model.FES.MIX.SD.SD3)
model.FES.MIX.SD.SD1 = lm(SD ~ N*Levels*Corr, data = FESMIXSD1)
summary(model.FES.MIX.SD.SD1)

## SD3 split by levels
FESBN1SD3$Levelslo = FESBN1SD3$Levels + sd(FESBN1SD3$Levels)
FESBN1SD3$Levelshi = FESBN1SD3$Levels - sd(FESBN1SD3$Levels)
model.FES.BN1.SD.SD3.levello = lm(SD ~ N*Levelslo, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3.levello)
model.FES.BN1.SD.SD3.levelhi = lm(SD ~ N*Levelshi, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3.levelhi)

FESMIXSD3$Levelslo = FESMIXSD3$Levels + sd(FESMIXSD3$Levels)
FESMIXSD3$Levelshi = FESMIXSD3$Levels - sd(FESMIXSD3$Levels)
model.FES.MIX.SD.SD3.levello = lm(SD ~ N*Levelslo, data = FESMIXSD3)
summary(model.FES.MIX.SD.SD3.levello)
model.FES.MIX.SD.SD3.levelhi = lm(SD ~ N*Levelshi, data = FESMIXSD3)
summary(model.FES.MIX.SD.SD3.levelhi)

## SD1 split by Corr
FESBN1SD1$Corrlo = FESBN1SD1$Corr + sd(FESBN1SD1$Corr)
FESBN1SD1$Corrhi = FESBN1SD1$Corr - sd(FESBN1SD1$Corr)
model.FES.BN1.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1.Corrlo)
model.FES.BN1.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1.Corrhi)

FESRM2SD1$Corrlo = FESRM2SD1$Corr + sd(FESRM2SD1$Corr)
FESRM2SD1$Corrhi = FESRM2SD1$Corr - sd(FESRM2SD1$Corr)
model.FES.RM2.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1.Corrlo)
model.FES.RM2.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1.Corrhi)

FESMIXSD1$Corrlo = FESMIXSD1$Corr + sd(FESMIXSD1$Corr)
FESMIXSD1$Corrhi = FESMIXSD1$Corr - sd(FESMIXSD1$Corr)
model.FES.MIX.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = FESMIXSD1)
summary(model.FES.MIX.SD.SD1.Corrlo)
model.FES.MIX.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = FESMIXSD1)
summary(model.FES.MIX.SD.SD1.Corrhi)

## SD1 split by Corr split by Levels
FESBN1SD1$Levelslo = FESBN1SD1$Levels + sd(FESBN1SD1$Levels)
FESBN1SD1$Levelshi = FESBN1SD1$Levels - sd(FESBN1SD1$Levels)
model.FES.BN1.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1.Corrlo.Levelslo)
model.FES.BN1.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1.Corrlo.Levelshi)
model.FES.BN1.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1.Corrhi.Levelslo)
model.FES.BN1.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1.Corrhi.Levelshi)

FESRM2SD1$Levelslo = FESRM2SD1$Levels + sd(FESRM2SD1$Levels)
FESRM2SD1$Levelshi = FESRM2SD1$Levels - sd(FESRM2SD1$Levels)
model.FES.RM2.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1.Corrlo.Levelslo)
model.FES.RM2.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1.Corrlo.Levelshi)
model.FES.RM2.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1.Corrhi.Levelslo)
model.FES.RM2.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1.Corrhi.Levelshi)

FESMIXSD1$Levelslo = FESMIXSD1$Levels + sd(FESMIXSD1$Levels)
FESMIXSD1$Levelshi = FESMIXSD1$Levels - sd(FESMIXSD1$Levels)
model.FES.MIX.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = FESMIXSD1)
summary(model.FES.MIX.SD.SD1.Corrlo.Levelslo)
model.FES.MIX.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = FESMIXSD1)
summary(model.FES.MIX.SD.SD1.Corrlo.Levelshi)
model.FES.MIX.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = FESMIXSD1)
summary(model.FES.MIX.SD.SD1.Corrhi.Levelslo)
model.FES.MIX.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = FESMIXSD1)
summary(model.FES.MIX.SD.SD1.Corrhi.Levelshi)
############################################################################# FES


############################################################################# GES
#BN1, BN2, RM1, MIX together
GESdata = subset(Full.SD.data, ES=="ges")
GESBN1 = subset(GESdata, Design=="BN1")
GESRM2 = subset(GESdata, Design=="RM2")
##BN1 SD
model.GES.BN1.SD = lm(SD ~ N*Levels*Corr*stdev, data = GESBN1)
summary(model.GES.BN1.SD)
#RM2 SD
model.GES.RM2.SD = lm(SD ~ N*Levels*Corr*stdev, data = GESRM2)
summary(model.GES.RM2.SD)

#split by SD
GESBN1SD5 = subset(GESBN1, stdev == 5)
GESBN1SD3 = subset(GESBN1, stdev == 3)
GESBN1SD1 = subset(GESBN1, stdev == 1)
model.GES.BN1.SD.SD5 = lm(SD ~ N*Levels*Corr, data = GESBN1SD5)
summary(model.GES.BN1.SD.SD5)
model.GES.BN1.SD.SD3 = lm(SD ~ N*Levels*Corr, data = GESBN1SD3)
summary(model.GES.BN1.SD.SD3)
model.GES.BN1.SD.SD1 = lm(SD ~ N*Levels*Corr, data = GESBN1SD1)
summary(model.GES.BN1.SD.SD1)

GESRM2SD5 = subset(GESRM2, stdev == 5)
GESRM2SD3 = subset(GESRM2, stdev == 3)
GESRM2SD1 = subset(GESRM2, stdev == 1)
model.GES.RM2.SD.SD5 = lm(SD ~ N*Levels*Corr, data = GESRM2SD5)
summary(model.GES.RM2.SD.SD5)
model.GES.RM2.SD.SD3 = lm(SD ~ N*Levels*Corr, data = GESRM2SD3)
summary(model.GES.RM2.SD.SD3)
model.GES.RM2.SD.SD1 = lm(SD ~ N*Levels*Corr, data = GESRM2SD1)
summary(model.GES.RM2.SD.SD1)

## SD3 split by levels
GESBN1SD3$Levelslo = GESBN1SD3$Levels + sd(GESBN1SD3$Levels)
GESBN1SD3$Levelshi = GESBN1SD3$Levels - sd(GESBN1SD3$Levels)
model.GES.BN1.SD.SD3.levello = lm(SD ~ N*Levelslo, data = GESBN1SD3)
summary(model.GES.BN1.SD.SD3.levello)
model.GES.BN1.SD.SD3.levelhi = lm(SD ~ N*Levelshi, data = GESBN1SD3)
summary(model.GES.BN1.SD.SD3.levelhi)

## SD1 split by Corr
GESBN1SD1$Corrlo = GESBN1SD1$Corr + sd(GESBN1SD1$Corr)
GESBN1SD1$Corrhi = GESBN1SD1$Corr - sd(GESBN1SD1$Corr)
model.GES.BN1.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = GESBN1SD1)
summary(model.GES.BN1.SD.SD1.Corrlo)
model.GES.BN1.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = GESBN1SD1)
summary(model.GES.BN1.SD.SD1.Corrhi)

GESRM2SD1$Corrlo = GESRM2SD1$Corr + sd(GESRM2SD1$Corr)
GESRM2SD1$Corrhi = GESRM2SD1$Corr - sd(GESRM2SD1$Corr)
model.GES.RM2.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = GESRM2SD1)
summary(model.GES.RM2.SD.SD1.Corrlo)
model.GES.RM2.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = GESRM2SD1)
summary(model.GES.RM2.SD.SD1.Corrhi)

## SD1 split by Corr split by Levels
GESBN1SD1$Levelslo = GESBN1SD1$Levels + sd(GESBN1SD1$Levels)
GESBN1SD1$Levelshi = GESBN1SD1$Levels - sd(GESBN1SD1$Levels)
model.GES.BN1.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = GESBN1SD1)
summary(model.GES.BN1.SD.SD1.Corrlo.Levelslo)
model.GES.BN1.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = GESBN1SD1)
summary(model.GES.BN1.SD.SD1.Corrlo.Levelshi)
model.GES.BN1.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = GESBN1SD1)
summary(model.GES.BN1.SD.SD1.Corrhi.Levelslo)
model.GES.BN1.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = GESBN1SD1)
summary(model.GES.BN1.SD.SD1.Corrhi.Levelshi)

GESRM2SD1$Levelslo = GESRM2SD1$Levels + sd(GESRM2SD1$Levels)
GESRM2SD1$Levelshi = GESRM2SD1$Levels - sd(GESRM2SD1$Levels)
model.GES.RM2.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = GESRM2SD1)
summary(model.GES.RM2.SD.SD1.Corrlo.Levelslo)
model.GES.RM2.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = GESRM2SD1)
summary(model.GES.RM2.SD.SD1.Corrlo.Levelshi)
model.GES.RM2.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = GESRM2SD1)
summary(model.GES.RM2.SD.SD1.Corrhi.Levelslo)
model.GES.RM2.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = GESRM2SD1)
summary(model.GES.RM2.SD.SD1.Corrhi.Levelshi)
############################################################################# GES


############################################################################# PES
#BN1, BN2 together
#RM1, RM2, MIX together
PESdata = subset(Full.SD.data, ES=="pes")
PESBN1 = subset(PESdata, Design=="BN1")
PESRM2 = subset(PESdata, Design=="RM2")
##BN1 SD
model.PES.BN1.SD = lm(SD ~ N*Levels*Corr*stdev, data = PESBN1)
summary(model.PES.BN1.SD)
#RM2 SD
model.PES.RM2.SD = lm(SD ~ N*Levels*Corr*stdev, data = PESRM2)
summary(model.PES.RM2.SD)

#split by SD
PESBN1SD5 = subset(PESBN1, stdev == 5)
PESBN1SD3 = subset(PESBN1, stdev == 3)
PESBN1SD1 = subset(PESBN1, stdev == 1)
model.PES.BN1.SD.SD5 = lm(SD ~ N*Levels*Corr, data = PESBN1SD5)
summary(model.PES.BN1.SD.SD5)
model.PES.BN1.SD.SD3 = lm(SD ~ N*Levels*Corr, data = PESBN1SD3)
summary(model.PES.BN1.SD.SD3)
model.PES.BN1.SD.SD1 = lm(SD ~ N*Levels*Corr, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1)

PESRM2SD5 = subset(PESRM2, stdev == 5)
PESRM2SD3 = subset(PESRM2, stdev == 3)
PESRM2SD1 = subset(PESRM2, stdev == 1)
model.PES.RM2.SD.SD5 = lm(SD ~ N*Levels*Corr, data = PESRM2SD5)
summary(model.PES.RM2.SD.SD5)
model.PES.RM2.SD.SD3 = lm(SD ~ N*Levels*Corr, data = PESRM2SD3)
summary(model.PES.RM2.SD.SD3)
model.PES.RM2.SD.SD1 = lm(SD ~ N*Levels*Corr, data = PESRM2SD1)
summary(model.PES.RM2.SD.SD1)

## SD3 split by levels
PESBN1SD3$Levelslo = PESBN1SD3$Levels + sd(PESBN1SD3$Levels)
PESBN1SD3$Levelshi = PESBN1SD3$Levels - sd(PESBN1SD3$Levels)
model.PES.BN1.SD.SD3.levello = lm(SD ~ N*Levelslo, data = PESBN1SD3)
summary(model.PES.BN1.SD.SD3.levello)
model.PES.BN1.SD.SD3.levelhi = lm(SD ~ N*Levelshi, data = PESBN1SD3)
summary(model.PES.BN1.SD.SD3.levelhi)

## SD3 split by correlation
PESRM2SD3$Corrlo = PESRM2SD3$Corr + sd(PESRM2SD3$Corr)
PESRM2SD3$Corrhi = PESRM2SD3$Corr - sd(PESRM2SD3$Corr)
model.PES.RM2.SD.SD3.Corrlo = lm(SD ~ N*Corrlo, data = PESRM2SD3)
summary(model.PES.RM2.SD.SD3.Corrlo)
model.PES.RM2.SD.SD3.Corrhi = lm(SD ~ N*Corrhi, data = PESRM2SD3)
summary(model.PES.RM2.SD.SD3.Corrhi)

## SD1 split by levels
PESRM2SD1$Levelslo = PESRM2SD1$Levels + sd(PESRM2SD1$Levels)
PESRM2SD1$Levelshi = PESRM2SD1$Levels - sd(PESRM2SD1$Levels)
model.PES.RM2.SD.SD1.levello = lm(SD ~ N*Levelslo, data = PESRM2SD1)
summary(model.PES.RM2.SD.SD1.levello)
model.PES.RM2.SD.SD1.levelhi = lm(SD ~ N*Levelshi, data = PESRM2SD1)
summary(model.PES.RM2.SD.SD1.levelhi)

## SD1 split by Corr
PESBN1SD1$Corrlo = PESBN1SD1$Corr + sd(PESBN1SD1$Corr)
PESBN1SD1$Corrhi = PESBN1SD1$Corr - sd(PESBN1SD1$Corr)
model.PES.BN1.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1.Corrlo)
model.PES.BN1.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1.Corrhi)

## SD1 split by Corr split by Levels
PESBN1SD1$Levelslo = PESBN1SD1$Levels + sd(PESBN1SD1$Levels)
PESBN1SD1$Levelshi = PESBN1SD1$Levels - sd(PESBN1SD1$Levels)
model.PES.BN1.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1.Corrlo.Levelslo)
model.PES.BN1.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1.Corrlo.Levelshi)
model.PES.BN1.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1.Corrhi.Levelslo)
model.PES.BN1.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1.Corrhi.Levelshi)
############################################################################# PES


############################################################################# FOS
#BN1, BN2 together
FOSdata = subset(Full.SD.data, ES=="fos")
FOSBN1 = subset(FOSdata, Design=="BN1")
FOSRM1 = subset(FOSdata, Design=="RM1")
FOSRM2 = subset(FOSdata, Design=="RM2")
FOSMIX = subset(FOSdata, Design=="MIX")
##BN1 SD
model.FOS.BN1.SD = lm(SD ~ N*Levels*Corr*stdev, data = FOSBN1)
summary(model.FOS.BN1.SD)
#RM1 SD
model.FOS.RM1.SD = lm(SD ~ N*Levels*Corr*stdev, data = FOSRM1)
summary(model.FOS.RM1.SD)
#RM2 SD
model.FOS.RM2.SD = lm(SD ~ N*Levels*Corr*stdev, data = FOSRM2)
summary(model.FOS.RM2.SD)
#MIX SD
model.FOS.MIX.SD = lm(SD ~ N*Levels*Corr*stdev, data = FOSMIX)
summary(model.FOS.MIX.SD)

#BN1 SD split by SD
FOSBN1SD5 = subset(FOSBN1, stdev == 5)
FOSBN1SD3 = subset(FOSBN1, stdev == 3)
FOSBN1SD1 = subset(FOSBN1, stdev == 1)
model.FOS.BN1.SD.SD5 = lm(SD ~ N*Levels*Corr, data = FOSBN1SD5)
summary(model.FOS.BN1.SD.SD5)
model.FOS.BN1.SD.SD3 = lm(SD ~ N*Levels*Corr, data = FOSBN1SD3)
summary(model.FOS.BN1.SD.SD3)
model.FOS.BN1.SD.SD1 = lm(SD ~ N*Levels*Corr, data = FOSBN1SD1)
summary(model.FOS.BN1.SD.SD1)
#RM1 SD split by SD
FOSRM1SD5 = subset(FOSRM1, stdev == 5)
FOSRM1SD3 = subset(FOSRM1, stdev == 3)
FOSRM1SD1 = subset(FOSRM1, stdev == 1)
model.FOS.RM1.SD.SD5 = lm(SD ~ N*Levels*Corr, data = FOSRM1SD5)
summary(model.FOS.RM1.SD.SD5)
model.FOS.RM1.SD.SD3 = lm(SD ~ N*Levels*Corr, data = FOSRM1SD3)
summary(model.FOS.RM1.SD.SD3)
model.FOS.RM1.SD.SD1 = lm(SD ~ N*Levels*Corr, data = FOSRM1SD1)
summary(model.FOS.RM1.SD.SD1)
#RM2 SD split by SD
FOSRM2SD5 = subset(FOSRM2, stdev == 5)
FOSRM2SD3 = subset(FOSRM2, stdev == 3)
FOSRM2SD1 = subset(FOSRM2, stdev == 1)
model.FOS.RM2.SD.SD5 = lm(SD ~ N*Levels*Corr, data = FOSRM2SD5)
summary(model.FOS.RM2.SD.SD5)
model.FOS.RM2.SD.SD3 = lm(SD ~ N*Levels*Corr, data = FOSRM2SD3)
summary(model.FOS.RM2.SD.SD3)
model.FOS.RM2.SD.SD1 = lm(SD ~ N*Levels*Corr, data = FOSRM2SD1)
summary(model.FOS.RM2.SD.SD1)
#MIX SD split by SD
FOSMIXSD5 = subset(FOSMIX, stdev == 5)
FOSMIXSD3 = subset(FOSMIX, stdev == 3)
FOSMIXSD1 = subset(FOSMIX, stdev == 1)
model.FOS.MIX.SD.SD5 = lm(SD ~ N*Levels*Corr, data = FOSMIXSD5)
summary(model.FOS.MIX.SD.SD5)
model.FOS.MIX.SD.SD3 = lm(SD ~ N*Levels*Corr, data = FOSMIXSD3)
summary(model.FOS.MIX.SD.SD3)
model.FOS.MIX.SD.SD1 = lm(SD ~ N*Levels*Corr, data = FOSMIXSD1)
summary(model.FOS.MIX.SD.SD1)

## SD3 split by levels
FOSBN1SD3$Levelslo = FOSBN1SD3$Levels + sd(FOSBN1SD3$Levels)
FOSBN1SD3$Levelshi = FOSBN1SD3$Levels - sd(FOSBN1SD3$Levels)
model.FOS.BN1.SD.SD3.levello = lm(SD ~ N*Levelslo, data = FOSBN1SD3)
summary(model.FOS.BN1.SD.SD3.levello)
model.FOS.BN1.SD.SD3.levelhi = lm(SD ~ N*Levelshi, data = FOSBN1SD3)
summary(model.FOS.BN1.SD.SD3.levelhi)

FOSRM1SD3$Levelslo = FOSRM1SD3$Levels + sd(FOSRM1SD3$Levels)
FOSRM1SD3$Levelshi = FOSRM1SD3$Levels - sd(FOSRM1SD3$Levels)
model.FOS.RM1.SD.SD3.levello = lm(SD ~ N*Levelslo, data = FOSRM1SD3)
summary(model.FOS.RM1.SD.SD3.levello)
model.FOS.RM1.SD.SD3.levelhi = lm(SD ~ N*Levelshi, data = FOSRM1SD3)
summary(model.FOS.RM1.SD.SD3.levelhi)

FOSMIXSD3$Levelslo = FOSMIXSD3$Levels + sd(FOSMIXSD3$Levels)
FOSMIXSD3$Levelshi = FOSMIXSD3$Levels - sd(FOSMIXSD3$Levels)
model.FOS.MIX.SD.SD3.levello = lm(SD ~ N*Levelslo, data = FOSMIXSD3)
summary(model.FOS.MIX.SD.SD3.levello)
model.FOS.MIX.SD.SD3.levelhi = lm(SD ~ N*Levelshi, data = FOSMIXSD3)
summary(model.FOS.MIX.SD.SD3.levelhi)

## SD1 split by Corr
FOSBN1SD1$Corrlo = FOSBN1SD1$Corr + sd(FOSBN1SD1$Corr)
FOSBN1SD1$Corrhi = FOSBN1SD1$Corr - sd(FOSBN1SD1$Corr)
model.FOS.BN1.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = FOSBN1SD1)
summary(model.FOS.BN1.SD.SD1.Corrlo)
model.FOS.BN1.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = FOSBN1SD1)
summary(model.FOS.BN1.SD.SD1.Corrhi)

FOSRM1SD1$Corrlo = FOSRM1SD1$Corr + sd(FOSRM1SD1$Corr)
FOSRM1SD1$Corrhi = FOSRM1SD1$Corr - sd(FOSRM1SD1$Corr)
model.FOS.RM1.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = FOSRM1SD1)
summary(model.FOS.RM1.SD.SD1.Corrlo)
model.FOS.RM1.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = FOSRM1SD1)
summary(model.FOS.RM1.SD.SD1.Corrhi)

FOSRM2SD1$Corrlo = FOSRM2SD1$Corr + sd(FOSRM2SD1$Corr)
FOSRM2SD1$Corrhi = FOSRM2SD1$Corr - sd(FOSRM2SD1$Corr)
model.FOS.RM2.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = FOSRM2SD1)
summary(model.FOS.RM2.SD.SD1.Corrlo)
model.FOS.RM2.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = FOSRM2SD1)
summary(model.FOS.RM2.SD.SD1.Corrhi)

FOSMIXSD1$Corrlo = FOSMIXSD1$Corr + sd(FOSMIXSD1$Corr)
FOSMIXSD1$Corrhi = FOSMIXSD1$Corr - sd(FOSMIXSD1$Corr)
model.FOS.MIX.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = FOSMIXSD1)
summary(model.FOS.MIX.SD.SD1.Corrlo)
model.FOS.MIX.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = FOSMIXSD1)
summary(model.FOS.MIX.SD.SD1.Corrhi)

## SD1 split by Corr split by Levels
FOSBN1SD1$Levelslo = FOSBN1SD1$Levels + sd(FOSBN1SD1$Levels)
FOSBN1SD1$Levelshi = FOSBN1SD1$Levels - sd(FOSBN1SD1$Levels)
model.FOS.BN1.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = FOSBN1SD1)
summary(model.FOS.BN1.SD.SD1.Corrlo.Levelslo)
model.FOS.BN1.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = FOSBN1SD1)
summary(model.FOS.BN1.SD.SD1.Corrlo.Levelshi)
model.FOS.BN1.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = FOSBN1SD1)
summary(model.FOS.BN1.SD.SD1.Corrhi.Levelslo)
model.FOS.BN1.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = FOSBN1SD1)
summary(model.FOS.BN1.SD.SD1.Corrhi.Levelshi)

FOSRM1SD1$Levelslo = FOSRM1SD1$Levels + sd(FOSRM1SD1$Levels)
FOSRM1SD1$Levelshi = FOSRM1SD1$Levels - sd(FOSRM1SD1$Levels)
model.FOS.RM1.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = FOSRM1SD1)
summary(model.FOS.RM1.SD.SD1.Corrlo.Levelslo)
model.FOS.RM1.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = FOSRM1SD1)
summary(model.FOS.RM1.SD.SD1.Corrlo.Levelshi)
model.FOS.RM1.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = FOSRM1SD1)
summary(model.FOS.RM1.SD.SD1.Corrhi.Levelslo)
model.FOS.RM1.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = FOSRM1SD1)
summary(model.FOS.RM1.SD.SD1.Corrhi.Levelshi)

FOSRM2SD1$Levelslo = FOSRM2SD1$Levels + sd(FOSRM2SD1$Levels)
FOSRM2SD1$Levelshi = FOSRM2SD1$Levels - sd(FOSRM2SD1$Levels)
model.FOS.RM2.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = FOSRM2SD1)
summary(model.FOS.RM2.SD.SD1.Corrlo.Levelslo)
model.FOS.RM2.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = FOSRM2SD1)
summary(model.FOS.RM2.SD.SD1.Corrlo.Levelshi)
model.FOS.RM2.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = FOSRM2SD1)
summary(model.FOS.RM2.SD.SD1.Corrhi.Levelslo)
model.FOS.RM2.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = FOSRM2SD1)
summary(model.FOS.RM2.SD.SD1.Corrhi.Levelshi)

FOSMIXSD1$Levelslo = FOSMIXSD1$Levels + sd(FOSMIXSD1$Levels)
FOSMIXSD1$Levelshi = FOSMIXSD1$Levels - sd(FOSMIXSD1$Levels)
model.FOS.MIX.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = FOSMIXSD1)
summary(model.FOS.MIX.SD.SD1.Corrlo.Levelslo)
model.FOS.MIX.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = FOSMIXSD1)
summary(model.FOS.MIX.SD.SD1.Corrlo.Levelshi)
model.FOS.MIX.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = FOSMIXSD1)
summary(model.FOS.MIX.SD.SD1.Corrhi.Levelslo)
model.FOS.MIX.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = FOSMIXSD1)
summary(model.FOS.MIX.SD.SD1.Corrhi.Levelshi)
############################################################################# FOS



############################################################################# POS
POSdata = subset(Full.SD.data, ES=="pos")
POSBN2 = subset(POSdata, Design=="BN2")
POSRM2 = subset(POSdata, Design=="RM2")
POSMIX = subset(POSdata, Design=="MIX")
#BN2 SD
model.POS.BN2.SD = lm(SD ~ N*Levels*Corr*stdev, data = POSBN2)
summary(model.POS.BN2.SD)
#RM2 SD
model.POS.RM2.SD = lm(SD ~ N*Levels*Corr*stdev, data = POSRM2)
summary(model.POS.RM2.SD)
#MIX SD
model.POS.MIX.SD = lm(SD ~ N*Levels*Corr*stdev, data = POSMIX)
summary(model.POS.MIX.SD)

#corr:SD split by SD
POSRM2.corr.SD5 = subset(POSRM2, stdev == 5)
POSRM2.corr.SD3 = subset(POSRM2, stdev == 3)
POSRM2.corr.SD1 = subset(POSRM2, stdev == 1)
model.POS.RM2.corr.SD5 = lm(SD ~ Corr, data = POSRM2.corr.SD5)
summary(model.POS.RM2.corr.SD5)
model.POS.RM2.corr.SD3 = lm(SD ~ Corr, data = POSRM2.corr.SD3)
summary(model.POS.RM2.corr.SD3)
model.POS.RM2.corr.SD1 = lm(SD ~ Corr, data = POSRM2.corr.SD1)
summary(model.POS.RM2.corr.SD1)

POSMIX.corr.SD5 = subset(POSMIX, stdev == 5)
POSMIX.corr.SD3 = subset(POSMIX, stdev == 3)
POSMIX.corr.SD1 = subset(POSMIX, stdev == 1)
model.POS.MIX.corr.SD5 = lm(SD ~ Corr, data = POSMIX.corr.SD5)
summary(model.POS.MIX.corr.SD5)
model.POS.MIX.corr.SD3 = lm(SD ~ Corr, data = POSMIX.corr.SD3)
summary(model.POS.MIX.corr.SD3)
model.POS.MIX.corr.SD1 = lm(SD ~ Corr, data = POSMIX.corr.SD1)
summary(model.POS.MIX.corr.SD1)

#BN2 SD split by SD
POSBN2SD5 = subset(POSBN2, stdev == 5)
POSBN2SD3 = subset(POSBN2, stdev == 3)
POSBN2SD1 = subset(POSBN2, stdev == 1)
model.POS.BN2.SD.SD5 = lm(SD ~ N*Levels*Corr, data = POSBN2SD5)
summary(model.POS.BN2.SD.SD5)
model.POS.BN2.SD.SD3 = lm(SD ~ N*Levels*Corr, data = POSBN2SD3)
summary(model.POS.BN2.SD.SD3)
model.POS.BN2.SD.SD1 = lm(SD ~ N*Levels*Corr, data = POSBN2SD1)
summary(model.POS.BN2.SD.SD1)

## SD3 split by levels
POSBN2SD3$Levelslo = POSBN2SD3$Levels + sd(POSBN2SD3$Levels)
POSBN2SD3$Levelshi = POSBN2SD3$Levels - sd(POSBN2SD3$Levels)
model.POS.BN2.SD.SD3.levello = lm(SD ~ N*Levelslo, data = POSBN2SD3)
summary(model.POS.BN2.SD.SD3.levello)
model.POS.BN2.SD.SD3.levelhi = lm(SD ~ N*Levelshi, data = POSBN2SD3)
summary(model.POS.BN2.SD.SD3.levelhi)

## SD1 split by Corr
POSBN2SD1$Corrlo = POSBN2SD1$Corr + sd(POSBN2SD1$Corr)
POSBN2SD1$Corrhi = POSBN2SD1$Corr - sd(POSBN2SD1$Corr)
model.POS.BN2.SD.SD1.Corrlo = lm(SD ~ N*Levels*Corrlo, data = POSBN2SD1)
summary(model.POS.BN2.SD.SD1.Corrlo)
model.POS.BN2.SD.SD1.Corrhi = lm(SD ~ N*Levels*Corrhi, data = POSBN2SD1)
summary(model.POS.BN2.SD.SD1.Corrhi)

## SD1 split by Corr split by Levels
POSBN2SD1$Levelslo = POSBN2SD1$Levels + sd(POSBN2SD1$Levels)
POSBN2SD1$Levelshi = POSBN2SD1$Levels - sd(POSBN2SD1$Levels)
model.POS.BN2.SD.SD1.Corrlo.Levelslo = lm(SD ~ N*Levelslo*Corrlo, data = POSBN2SD1)
summary(model.POS.BN2.SD.SD1.Corrlo.Levelslo)
model.POS.BN2.SD.SD1.Corrlo.Levelshi = lm(SD ~ N*Levelshi*Corrlo, data = POSBN2SD1)
summary(model.POS.BN2.SD.SD1.Corrlo.Levelshi)
model.POS.BN2.SD.SD1.Corrhi.Levelslo = lm(SD ~ N*Levelslo*Corrhi, data = POSBN2SD1)
summary(model.POS.BN2.SD.SD1.Corrhi.Levelslo)
model.POS.BN2.SD.SD1.Corrhi.Levelshi = lm(SD ~ N*Levelshi*Corrhi, data = POSBN2SD1)
summary(model.POS.BN2.SD.SD1.Corrhi.Levelshi)
############################################################################# POS
