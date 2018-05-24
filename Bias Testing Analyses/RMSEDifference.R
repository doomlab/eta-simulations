




setwd("C:/Users/John/Desktop")
source("BiasCalculate.R")

Full.RMSE.data = Full.bias.data
############################################################################# Diff in ES and Design
##RMSE
model.allES.RMSE = lm(RMSE ~ ES*Design, data = Full.RMSE.data)
summary.aov(model.allES.RMSE)
7.08/(7.08+249.56) #ES
4.09/(4.09+249.56) #Design
4.77/(4.77+249.56) #Interaction

##ES
tapply(Full.RMSE.data$RMSE, list(Full.RMSE.data$ES), mean)
tapply(Full.RMSE.data$RMSE, list(Full.RMSE.data$ES), sd)
pairwise.t.test(Full.RMSE.data$RMSE, Full.RMSE.data$ES, 
                paired = F,
                var.equal = T,
                p.adjust.method = "bonferroni")
##Design
tapply(Full.RMSE.data$RMSE, list(Full.RMSE.data$Design), mean)
tapply(Full.RMSE.data$RMSE, list(Full.RMSE.data$Design), sd)
pairwise.t.test(Full.RMSE.data$RMSE, Full.RMSE.data$Design, 
                paired = F,
                var.equal = T,
                p.adjust.method = "bonferroni")
##Interaction
tapply(Full.RMSE.data$RMSE, list(Full.RMSE.data$ES, Full.RMSE.data$Design), mean)
tapply(Full.RMSE.data$RMSE, list(Full.RMSE.data$ES, Full.RMSE.data$Design), sd)
splitfes = subset(Full.RMSE.data, ES=="fes")
splitges = subset(Full.RMSE.data, ES=="ges")
splitpes = subset(Full.RMSE.data, ES=="pes")
splitpos = subset(Full.RMSE.data, ES=="pos")
splitfos = subset(Full.RMSE.data, ES=="fos")
pairwise.t.test(splitfes$RMSE, splitfes$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitges$RMSE, splitges$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitpes$RMSE, splitpes$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitpos$RMSE, splitpos$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(splitfos$RMSE, splitfos$Design, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
############################################################################# Diff in ES and Design



############################################################################# FES, FOS, GES
#BN1, BN2, MIX, RM1 together
FESdata = subset(Full.RMSE.data, ES=="fes")
FESBN1 = subset(FESdata, Design=="BN1")
FESRM2 = subset(FESdata, Design=="RM2")
##BN1 RMSE
model.FES.BN1.RMSE = lm(RMSE ~ N*Levels*Corr*stdev, data = FESBN1)
summary(model.FES.BN1.RMSE)
#RM2 RMSE
model.FES.RM2.RMSE = lm(RMSE ~ N*Levels*Corr*stdev, data = FESRM2)
summary(model.FES.RM2.RMSE)

#Split by SD
FESBN1SD5 = subset(FESBN1, stdev == 5)
FESBN1SD3 = subset(FESBN1, stdev == 3)
FESBN1SD1 = subset(FESBN1, stdev == 1)
model.FES.BN1.RMSE.SD5 = lm(RMSE ~ N*Levels*Corr, data = FESBN1SD5)
summary(model.FES.BN1.RMSE.SD5)
model.FES.BN1.RMSE.SD3 = lm(RMSE ~ N*Levels*Corr, data = FESBN1SD3)
summary(model.FES.BN1.RMSE.SD3)
model.FES.BN1.RMSE.SD1 = lm(RMSE ~ N*Levels*Corr, data = FESBN1SD1)
summary(model.FES.BN1.RMSE.SD1)

FESRM2SD5 = subset(FESRM2, stdev == 5)
FESRM2SD3 = subset(FESRM2, stdev == 3)
FESRM2SD1 = subset(FESRM2, stdev == 1)
model.FES.RM2.RMSE.SD5 = lm(RMSE ~ N*Levels*Corr, data = FESRM2SD5)
summary(model.FES.RM2.RMSE.SD5)
model.FES.RM2.RMSE.SD3 = lm(RMSE ~ N*Levels*Corr, data = FESRM2SD3)
summary(model.FES.RM2.RMSE.SD3)
model.FES.RM2.RMSE.SD1 = lm(RMSE ~ N*Levels*Corr, data = FESRM2SD1)
summary(model.FES.RM2.RMSE.SD1)

#SD5 RM2 split by corr
FESRM2SD5$Corrlo = FESRM2SD5$Corr + sd(FESRM2SD5$Corr)
FESRM2SD5$Corrhi = FESRM2SD5$Corr - sd(FESRM2SD5$Corr)
model.FES.RM2.SD.SD5.Corrlo = lm(SD ~ N*Corrlo, data = FESRM2SD5)
summary(model.FES.RM2.SD.SD5.Corrlo)
model.FES.RM2.SD.SD5.Corrhi = lm(SD ~ N*Corrhi, data = FESRM2SD5)
summary(model.FES.RM2.SD.SD5.Corrhi)

#SD5 RM2 split by Levels
FESRM2SD5$Levelslo = FESRM2SD5$Levels + sd(FESRM2SD5$Levels)
FESRM2SD5$Levelshi = FESRM2SD5$Levels - sd(FESRM2SD5$Levels)
model.FES.RM2.SD.SD5.Levelslo = lm(SD ~ N*Levelslo, data = FESRM2SD5)
summary(model.FES.RM2.SD.SD5.Levelslo)
model.FES.RM2.SD.SD5.Levelshi = lm(SD ~ N*Levelshi, data = FESRM2SD5)
summary(model.FES.RM2.SD.SD5.Levelshi)


#SD3 split by levels
FESBN1SD3$Levelslo = FESBN1SD3$Levels + sd(FESBN1SD3$Levels)
FESBN1SD3$Levelshi = FESBN1SD3$Levels - sd(FESBN1SD3$Levels)
model.FES.BN1.SD.SD3.Levelslo = lm(SD ~ N*Levelslo, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3.Levelslo)
model.FES.BN1.SD.SD3.Levelshi = lm(SD ~ N*Levelshi, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3.Levelshi)

FESRM2SD3$Levelslo = FESRM2SD3$Levels + sd(FESRM2SD3$Levels)
FESRM2SD3$Levelshi = FESRM2SD3$Levels - sd(FESRM2SD3$Levels)
model.FES.RM2.SD.SD3.Levelslo = lm(SD ~ N*Levelslo, data = FESRM2SD3)
summary(model.FES.RM2.SD.SD3.Levelslo)
model.FES.RM2.SD.SD3.Levelshi = lm(SD ~ N*Levelshi, data = FESRM2SD3)
summary(model.FES.RM2.SD.SD3.Levelshi)

#SD3 BN1 split by corr
FESBN1SD3$Corrlo = FESBN1SD3$Corr + sd(FESBN1SD3$Corr)
FESBN1SD3$Corrhi = FESBN1SD3$Corr - sd(FESBN1SD3$Corr)
model.FES.BN1.SD.SD3.Corrlo = lm(SD ~ N*Levels*Corrlo, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3.Corrlo)
model.FES.BN1.SD.SD3.Corrhi = lm(SD ~ N*Levels*Corrhi, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3.Corrhi)

#SD3 BN1 high corr split by levels
model.FES.BN1.SD.SD3.levelslo.Corrhi = lm(SD ~ N*Levelslo*Corrhi, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3.levelslo.Corrhi)
model.FES.BN1.SD.SD3.levelshi.Corrhi = lm(SD ~ N*Levelshi*Corrhi, data = FESBN1SD3)
summary(model.FES.BN1.SD.SD3.levelshi.Corrhi)

#SD1 split by levels
FESBN1SD1$Levelslo = FESBN1SD1$Levels + sd(FESBN1SD1$Levels)
FESBN1SD1$Levelshi = FESBN1SD1$Levels - sd(FESBN1SD1$Levels)
model.FES.BN1.SD.SD1.Levelslo = lm(SD ~ N*Levelslo, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1.Levelslo)
model.FES.BN1.SD.SD1.Levelshi = lm(SD ~ N*Levelshi, data = FESBN1SD1)
summary(model.FES.BN1.SD.SD1.Levelshi)

FESRM2SD1$Levelslo = FESRM2SD1$Levels + sd(FESRM2SD1$Levels)
FESRM2SD1$Levelshi = FESRM2SD1$Levels - sd(FESRM2SD1$Levels)
model.FES.RM2.SD.SD1.Levelslo = lm(SD ~ N*Levelslo, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1.Levelslo)
model.FES.RM2.SD.SD1.Levelshi = lm(SD ~ N*Levelshi, data = FESRM2SD1)
summary(model.FES.RM2.SD.SD1.Levelshi)
############################################################################# FES, FOS, GES



############################################################################# PES
#BN1, BN2 together
#RM1, RM2, MIX together
PESdata = subset(Full.RMSE.data, ES=="pes")
PESBN1 = subset(PESdata, Design=="BN1")
PESRM2 = subset(PESdata, Design=="RM2")
##BN1 RMSE
model.PES.BN1.RMSE = lm(RMSE ~ N*Levels*Corr*stdev, data = PESBN1)
summary(model.PES.BN1.RMSE)
#RM2 RMSE
model.PES.RM2.RMSE = lm(RMSE ~ N*Levels*Corr*stdev, data = PESRM2)
summary(model.PES.RM2.RMSE)

#Split by SD
PESBN1SD5 = subset(PESBN1, stdev == 5)
PESBN1SD3 = subset(PESBN1, stdev == 3)
PESBN1SD1 = subset(PESBN1, stdev == 1)
model.PES.BN1.RMSE.SD5 = lm(RMSE ~ N*Levels*Corr, data = PESBN1SD5)
summary(model.PES.BN1.RMSE.SD5)
model.PES.BN1.RMSE.SD3 = lm(RMSE ~ N*Levels*Corr, data = PESBN1SD3)
summary(model.PES.BN1.RMSE.SD3)
model.PES.BN1.RMSE.SD1 = lm(RMSE ~ N*Levels*Corr, data = PESBN1SD1)
summary(model.PES.BN1.RMSE.SD1)

PESRM2SD5 = subset(PESRM2, stdev == 5)
PESRM2SD3 = subset(PESRM2, stdev == 3)
PESRM2SD1 = subset(PESRM2, stdev == 1)
model.PES.RM2.RMSE.SD5 = lm(RMSE ~ N*Levels*Corr, data = PESRM2SD5)
summary(model.PES.RM2.RMSE.SD5)
model.PES.RM2.RMSE.SD3 = lm(RMSE ~ N*Levels*Corr, data = PESRM2SD3)
summary(model.PES.RM2.RMSE.SD3)
model.PES.RM2.RMSE.SD1 = lm(RMSE ~ N*Levels*Corr, data = PESRM2SD1)
summary(model.PES.RM2.RMSE.SD1)

#SD3 split by corr
PESBN1SD3$Corrlo = PESBN1SD3$Corr + sd(PESBN1SD3$Corr)
PESBN1SD3$Corrhi = PESBN1SD3$Corr - sd(PESBN1SD3$Corr)
model.PES.BN1.SD.SD3.Corrlo = lm(SD ~ N*Levels*Corrlo, data = PESBN1SD3)
summary(model.PES.BN1.SD.SD3.Corrlo)
model.PES.BN1.SD.SD3.Corrhi = lm(SD ~ N*Levels*Corrhi, data = PESBN1SD3)
summary(model.PES.BN1.SD.SD3.Corrhi)

PESRM2SD3$Corrlo = PESRM2SD3$Corr + sd(PESRM2SD3$Corr)
PESRM2SD3$Corrhi = PESRM2SD3$Corr - sd(PESRM2SD3$Corr)
model.PES.RM2.SD.SD3.Corrlo = lm(SD ~ N*Levels*Corrlo, data = PESRM2SD3)
summary(model.PES.RM2.SD.SD3.Corrlo)
model.PES.RM2.SD.SD3.Corrhi = lm(SD ~ N*Levels*Corrhi, data = PESRM2SD3)
summary(model.PES.RM2.SD.SD3.Corrhi)

#SD1 split by levels
PESBN1SD1$Levelslo = PESBN1SD1$Levels + sd(PESBN1SD1$Levels)
PESBN1SD1$Levelshi = PESBN1SD1$Levels - sd(PESBN1SD1$Levels)
model.PES.BN1.SD.SD1.Levelslo = lm(SD ~ N*Levelslo, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1.Levelslo)
model.PES.BN1.SD.SD1.Levelshi = lm(SD ~ N*Levelshi, data = PESBN1SD1)
summary(model.PES.BN1.SD.SD1.Levelshi)

PESRM2SD1$Levelslo = PESRM2SD1$Levels + sd(PESRM2SD1$Levels)
PESRM2SD1$Levelshi = PESRM2SD1$Levels - sd(PESRM2SD1$Levels)
model.PES.RM2.SD.SD1.Levelslo = lm(SD ~ N*Levelslo, data = PESRM2SD1)
summary(model.PES.RM2.SD.SD1.Levelslo)
model.PES.RM2.SD.SD1.Levelshi = lm(SD ~ N*Levelshi, data = PESRM2SD1)
summary(model.PES.RM2.SD.SD1.Levelshi)

#SD1 split by corr
PESRM2SD1$Corrlo = PESRM2SD1$Corr + sd(PESRM2SD1$Corr)
PESRM2SD1$Corrhi = PESRM2SD1$Corr - sd(PESRM2SD1$Corr)
model.PES.RM2.SD.SD1.Corrlo = lm(SD ~ N*Corrlo, data = PESRM2SD1)
summary(model.PES.RM2.SD.SD1.Corrlo)
model.PES.RM2.SD.SD1.Corrhi = lm(SD ~ N*Corrhi, data = PESRM2SD1)
summary(model.PES.RM2.SD.SD1.Corrhi)
############################################################################# PES



############################################################################# POS
POSdata = subset(Full.RMSE.data, ES=="pos")
POSBN2 = subset(POSdata, Design=="BN2")
POSRM2 = subset(POSdata, Design=="RM2")
POSMIX = subset(POSdata, Design=="MIX")
#BN2 RMSE
model.POS.BN2.RMSE = lm(RMSE ~ N*Levels*Corr*stdev, data = POSBN2)
summary(model.POS.BN2.RMSE)
#RM2 RMSE
model.POS.RM2.RMSE = lm(RMSE ~ N*Levels*Corr*stdev, data = POSRM2)
summary(model.POS.RM2.RMSE)
#MIX RMSE
model.POS.MIX.RMSE = lm(RMSE ~ N*Levels*Corr*stdev, data = POSMIX)
summary(model.POS.MIX.RMSE)

##RM2 and MIX split by SD
POSRM2SD5 = subset(POSRM2, stdev == 5)
POSRM2SD3 = subset(POSRM2, stdev == 3)
POSRM2SD1 = subset(POSRM2, stdev == 1)
model.POS.RM2.RMSE.SD5 = lm(RMSE ~ Levels, data = POSRM2SD5)
summary(model.POS.RM2.RMSE.SD5)
model.POS.RM2.RMSE.SD3 = lm(RMSE ~ Levels, data = POSRM2SD3)
summary(model.POS.RM2.RMSE.SD3)
model.POS.RM2.RMSE.SD1 = lm(RMSE ~ Levels, data = POSRM2SD1)
summary(model.POS.RM2.RMSE.SD1)

POSMIXSD5 = subset(POSMIX, stdev == 5)
POSMIXSD3 = subset(POSMIX, stdev == 3)
POSMIXSD1 = subset(POSMIX, stdev == 1)
model.POS.MIX.RMSE.SD5 = lm(RMSE ~ Levels, data = POSMIXSD5)
summary(model.POS.MIX.RMSE.SD5)
model.POS.MIX.RMSE.SD3 = lm(RMSE ~ Levels, data = POSMIXSD3)
summary(model.POS.MIX.RMSE.SD3)
model.POS.MIX.RMSE.SD1 = lm(RMSE ~ Levels, data = POSMIXSD1)
summary(model.POS.MIX.RMSE.SD1)

#Split by SD
POSBN2SD5 = subset(POSBN2, stdev == 5)
POSBN2SD3 = subset(POSBN2, stdev == 3)
POSBN2SD1 = subset(POSBN2, stdev == 1)
model.POS.BN2.RMSE.SD5 = lm(RMSE ~ N*Levels*Corr, data = POSBN2SD5)
summary(model.POS.BN2.RMSE.SD5)
model.POS.BN2.RMSE.SD3 = lm(RMSE ~ N*Levels*Corr, data = POSBN2SD3)
summary(model.POS.BN2.RMSE.SD3)
model.POS.BN2.RMSE.SD1 = lm(RMSE ~ N*Levels*Corr, data = POSBN2SD1)
summary(model.POS.BN2.RMSE.SD1)

#SD5 N:Corr
POSBN2SD5$Corrlo = POSBN2SD5$Corr + sd(POSBN2SD5$Corr)
POSBN2SD5$Corrhi = POSBN2SD5$Corr - sd(POSBN2SD5$Corr)
model.POS.BN2.SD.SD5.Corrlo = lm(SD ~ N*Corrlo, data = POSBN2SD5)
summary(model.POS.BN2.SD.SD5.Corrlo)
model.POS.BN2.SD.SD5.Corrhi = lm(SD ~ N*Corrhi, data = POSBN2SD5)
summary(model.POS.BN2.SD.SD5.Corrhi)

#SD3 Split by Corr
POSBN2SD3$Corrlo = POSBN2SD3$Corr + sd(POSBN2SD3$Corr)
POSBN2SD3$Corrhi = POSBN2SD3$Corr - sd(POSBN2SD3$Corr)
model.POS.BN2.SD.SD3.Corrlo = lm(SD ~ N*Corrlo, data = POSBN2SD3)
summary(model.POS.BN2.SD.SD3.Corrlo)
model.POS.BN2.SD.SD3.Corrhi = lm(SD ~ N*Corrhi, data = POSBN2SD3)
summary(model.POS.BN2.SD.SD3.Corrhi)
############################################################################# POS

