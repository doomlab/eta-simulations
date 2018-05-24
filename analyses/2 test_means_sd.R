##load data
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/BIAS 2")
fulldata <- read.csv("fulldata.csv")

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
  (fulldata$MIX.ssm.main+fulldata$MIX.ssr.main+fulldata$MIX.ssr.p+(fulldata$MIX.ssr.main/(fulldata$MIX.dfr/fulldata$MIX.dfm)))
fulldata$BN1.fos = (fulldata$BN1.dfm*((fulldata$BN1.ssm.main/fulldata$BN1.dfm)-(fulldata$BN1.ssr.main/fulldata$BN1.dfr)))/
  ((fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)+(fulldata$BN1.ssr.main/fulldata$BN1.dfr))
fulldata$BN2.fos = (fulldata$BN2.dfm*((fulldata$BN2.ssm.main/fulldata$BN2.dfm)-(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))/
  ((fulldata$BN2.ssm.main + fulldata$BN2.ssr.all + fulldata$BN2.ssm.other + fulldata$BN2.ssm.interact)+(fulldata$BN2.ssr.all/fulldata$BN2.dfr))

#calculate partial omega squared
#RM1 NA
#BN1 NA
fulldata$RM2.pos = (fulldata$RM2.dfm*((fulldata$RM2.ssm.main/fulldata$RM2.dfm)-(fulldata$RM2.ssr.main/fulldata$RM2.dfr)))/
  (fulldata$RM2.ssm.main+fulldata$RM2.ssr.main+fulldata$RM2.ssr.p+(fulldata$RM2.ssr.p/(fulldata$RM2.dfr/fulldata$RM2.dfm)))
fulldata$BN2.pos = (fulldata$BN2.dfm*((fulldata$BN2.ssm.main/fulldata$BN2.dfm)-(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))/
  (fulldata$BN2.ssm.main+(((fulldata$N*fulldata$levels)-fulldata$BN2.dfm)*(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))
fulldata$MIX.pos = (fulldata$MIX.dfm*((fulldata$MIX.ssm.main/fulldata$MIX.dfm)-(fulldata$MIX.ssr.main/fulldata$MIX.dfr)))/
  (fulldata$MIX.ssm.main+fulldata$MIX.ssr.main+fulldata$MIX.ssr.p+(fulldata$MIX.ssr.p/(fulldata$MIX.dfr/fulldata$MIX.dfm)))



####means####
mean_data = with(fulldata, aggregate.data.frame(RM1.ges, list(N, stdev, levels, correl), mean))
temp = with(fulldata, aggregate.data.frame(RM2.ges, list(N, stdev, levels, correl), mean))
temp2 = with(fulldata, aggregate.data.frame(BN1.ges, list(N, stdev, levels, correl), mean))
temp3 = with(fulldata, aggregate.data.frame(BN2.ges, list(N, stdev, levels, correl), mean))
temp4 = with(fulldata, aggregate.data.frame(MIX.ges, list(N, stdev, levels, correl), mean))
temp5 = with(fulldata, aggregate.data.frame(RM1.pes, list(N, stdev, levels, correl), mean))
temp6 = with(fulldata, aggregate.data.frame(RM2.pes, list(N, stdev, levels, correl), mean))
temp7 = with(fulldata, aggregate.data.frame(BN1.pes, list(N, stdev, levels, correl), mean))
temp8 = with(fulldata, aggregate.data.frame(BN2.pes, list(N, stdev, levels, correl), mean))
temp9 = with(fulldata, aggregate.data.frame(MIX.pes, list(N, stdev, levels, correl), mean))
temp10 = with(fulldata, aggregate.data.frame(RM1.fes, list(N, stdev, levels, correl), mean))
temp11 = with(fulldata, aggregate.data.frame(RM2.fes, list(N, stdev, levels, correl), mean))
temp12 = with(fulldata, aggregate.data.frame(BN1.fes, list(N, stdev, levels, correl), mean))
temp13 = with(fulldata, aggregate.data.frame(BN2.fes, list(N, stdev, levels, correl), mean))
temp14 = with(fulldata, aggregate.data.frame(MIX.fes, list(N, stdev, levels, correl), mean))
temp15 = with(fulldata, aggregate.data.frame(RM1.fos, list(N, stdev, levels, correl), mean))
temp16 = with(fulldata, aggregate.data.frame(RM2.fos, list(N, stdev, levels, correl), mean))
temp17 = with(fulldata, aggregate.data.frame(BN1.fos, list(N, stdev, levels, correl), mean))
temp18 = with(fulldata, aggregate.data.frame(BN2.fos, list(N, stdev, levels, correl), mean))
temp19 = with(fulldata, aggregate.data.frame(MIX.fos, list(N, stdev, levels, correl), mean))
temp20 = with(fulldata, aggregate.data.frame(RM2.pos, list(N, stdev, levels, correl), mean))
temp21 = with(fulldata, aggregate.data.frame(BN2.pos, list(N, stdev, levels, correl), mean))
temp22 = with(fulldata, aggregate.data.frame(MIX.pos, list(N, stdev, levels, correl), mean))
colnames(mean_data) = c("N", "stdev", "levels", "correl", "RM1.ges")
mean_data = cbind(mean_data,
                  "RM2.ges" = temp$x,
                  "BN1.ges" = temp2$x, 
                  "BN2.ges" = temp3$x,
                  "MIX.ges" = temp4$x,
                  "RM1.pes" = temp5$x,
                  "RM2.pes" = temp6$x,
                  "BN1.pes" = temp7$x,
                  "BN2.pes" = temp8$x,
                  "MIX.pes" = temp9$x,
                  "RM1.fes" = temp10$x,
                  "RM2.fes" = temp11$x,
                  "BN1.fes" = temp12$x,
                  "BN2.fes" = temp13$x,
                  "MIX.fes" = temp14$x,
                  "RM1.fos" = temp15$x,
                  "RM2.fos" = temp16$x,
                  "BN1.fos" = temp17$x,
                  "BN2.fos" = temp18$x,
                  "MIX.fos" = temp19$x,
                  "RM2.pos" = temp20$x,
                  "BN2.pos" = temp21$x,
                  "MIX.pos" = temp22$x
                  )

####stdevs####
stdev_data = with(fulldata, aggregate.data.frame(RM1.ges, list(N, stdev, levels, correl), sd))
temp = with(fulldata, aggregate.data.frame(RM2.ges, list(N, stdev, levels, correl), sd))
temp2 = with(fulldata, aggregate.data.frame(BN1.ges, list(N, stdev, levels, correl), sd))
temp3 = with(fulldata, aggregate.data.frame(BN2.ges, list(N, stdev, levels, correl), sd))
temp4 = with(fulldata, aggregate.data.frame(MIX.ges, list(N, stdev, levels, correl), sd))
temp5 = with(fulldata, aggregate.data.frame(RM1.pes, list(N, stdev, levels, correl), sd))
temp6 = with(fulldata, aggregate.data.frame(RM2.pes, list(N, stdev, levels, correl), sd))
temp7 = with(fulldata, aggregate.data.frame(BN1.pes, list(N, stdev, levels, correl), sd))
temp8 = with(fulldata, aggregate.data.frame(BN2.pes, list(N, stdev, levels, correl), sd))
temp9 = with(fulldata, aggregate.data.frame(MIX.pes, list(N, stdev, levels, correl), sd))
temp10 = with(fulldata, aggregate.data.frame(RM1.fes, list(N, stdev, levels, correl), sd))
temp11 = with(fulldata, aggregate.data.frame(RM2.fes, list(N, stdev, levels, correl), sd))
temp12 = with(fulldata, aggregate.data.frame(BN1.fes, list(N, stdev, levels, correl), sd))
temp13 = with(fulldata, aggregate.data.frame(BN2.fes, list(N, stdev, levels, correl), sd))
temp14 = with(fulldata, aggregate.data.frame(MIX.fes, list(N, stdev, levels, correl), sd))
temp15 = with(fulldata, aggregate.data.frame(RM1.fos, list(N, stdev, levels, correl), sd))
temp16 = with(fulldata, aggregate.data.frame(RM2.fos, list(N, stdev, levels, correl), sd))
temp17 = with(fulldata, aggregate.data.frame(BN1.fos, list(N, stdev, levels, correl), sd))
temp18 = with(fulldata, aggregate.data.frame(BN2.fos, list(N, stdev, levels, correl), sd))
temp19 = with(fulldata, aggregate.data.frame(MIX.fos, list(N, stdev, levels, correl), sd))
temp20 = with(fulldata, aggregate.data.frame(RM2.pos, list(N, stdev, levels, correl), sd))
temp21 = with(fulldata, aggregate.data.frame(BN2.pos, list(N, stdev, levels, correl), sd))
temp22 = with(fulldata, aggregate.data.frame(MIX.pos, list(N, stdev, levels, correl), sd))
colnames(stdev_data) = c("N", "stdev", "levels", "correl", "RM1.ges")
stdev_data = cbind(stdev_data,
                  "RM2.ges" = temp$x,
                  "BN1.ges" = temp2$x, 
                  "BN2.ges" = temp3$x,
                  "MIX.ges" = temp4$x,
                  "RM1.pes" = temp5$x,
                  "RM2.pes" = temp6$x,
                  "BN1.pes" = temp7$x,
                  "BN2.pes" = temp8$x,
                  "MIX.pes" = temp9$x,
                  "RM1.fes" = temp10$x,
                  "RM2.fes" = temp11$x,
                  "BN1.fes" = temp12$x,
                  "BN2.fes" = temp13$x,
                  "MIX.fes" = temp14$x,
                  "RM1.fos" = temp15$x,
                  "RM2.fos" = temp16$x,
                  "BN1.fos" = temp17$x,
                  "BN2.fos" = temp18$x,
                  "MIX.fos" = temp19$x,
                  "RM2.pos" = temp20$x,
                  "BN2.pos" = temp21$x,
                  "MIX.pos" = temp22$x
                  )
remove(temp)
remove(temp2)
remove(temp3)
remove(temp4)
remove(temp5)
remove(temp6)
remove(temp7)
remove(temp8)
remove(temp9)
remove(temp10)
remove(temp11)
remove(temp12)
remove(temp13)
remove(temp14)
remove(temp15)
remove(temp16)
remove(temp17)
remove(temp18)
remove(temp19)
remove(temp20)
remove(temp21)
remove(temp22)

options(scipen = 999)

####what are the differences in means####
##note that RM1.ges and BN1.ges are exactly the same
##note that BN2.ges and MIX.ges are exactly the same
##really here the question is what is RM1, BN1, BN2, Mix versus RM2
cor(mean_data)
symnum(cor(mean_data))


#means overall
model.RM1.meanover = lm(RM1.ges ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM1.meanover)
model.RM1.meanover.pes = lm(RM1.pes ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM1.meanover.pes)
model.RM1.meanover.fes = lm(RM1.fes ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM1.meanover.fes)
model.RM1.meanover.fos = lm(RM1.fos ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM1.meanover.fos)
model.BN2.meanover.pos = lm(BN2.pos ~ N*levels*correl*stdev, data = mean_data)
summary(model.BN2.meanover.pos)

model.RM2.meanover = lm(RM2.ges ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM2.meanover)
model.RM2.meanover.pes = lm(RM2.pes ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM2.meanover.pes)
model.RM2.meanover.fes = lm(RM2.fes ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM2.meanover.fes)
model.RM2.meanover.fos = lm(RM2.fos ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM2.meanover.fos)
model.RM2.meanover.pos = lm(RM2.pos ~ N*levels*correl*stdev, data = mean_data)
summary(model.RM2.meanover.pos)

#means RM1 simple slopes RM1 split by SD
SDfive = subset(mean_data, stdev == 5)
SDthree = subset(mean_data, stdev == 3)
SDone = subset(mean_data, stdev == 1)

model.RM1.sdfive = lm(RM1.ges ~ N*levels*correl, data = SDfive)
summary(model.RM1.sdfive)
model.RM1.sdthree = lm(RM1.ges ~ N*levels*correl, data = SDthree)
summary(model.RM1.sdthree)
model.RM1.sdone = lm(RM1.ges ~ N*levels*correl, data = SDone)
summary(model.RM1.sdone)

model.RM1.sdfive.pes = lm(RM1.pes ~ N*levels*correl, data = SDfive)
summary(model.RM1.sdfive.pes)
model.RM1.sdthree.pes = lm(RM1.pes ~ N*levels*correl, data = SDthree)
summary(model.RM1.sdthree.pes)
model.RM1.sdone.pes = lm(RM1.pes ~ N*levels*correl, data = SDone)
summary(model.RM1.sdone.pes)

model.RM1.sdfive.fes = lm(RM1.fes ~ N*levels*correl, data = SDfive)
summary(model.RM1.sdfive.fes)
model.RM1.sdthree.fes = lm(RM1.fes ~ N*levels*correl, data = SDthree)
summary(model.RM1.sdthree.fes)
model.RM1.sdone.fes = lm(RM1.fes ~ N*levels*correl, data = SDone)
summary(model.RM1.sdone.fes)

model.RM1.sdfive.fos = lm(RM1.fos ~ N*levels*correl, data = SDfive)
summary(model.RM1.sdfive.fos)
model.RM1.sdthree.fos = lm(RM1.fos ~ N*levels*correl, data = SDthree)
summary(model.RM1.sdthree.fos)
model.RM1.sdone.fos = lm(RM1.fos ~ N*levels*correl, data = SDone)
summary(model.RM1.sdone.fos)

model.BN2.sdfive.pos = lm(BN2.pos ~ N*levels*correl, data = SDfive)
summary(model.BN2.sdfive.pos)
model.BN2.sdthree.pos = lm(BN2.pos ~ N*levels*correl, data = SDthree)
summary(model.BN2.sdthree.pos)
model.BN2.sdone.pos = lm(BN2.pos ~ N*levels*correl, data = SDone)
summary(model.BN2.sdone.pos)

#means RM1 split by SD split by Cor
SDfive$corrello = SDfive$correl + sd(SDfive$correl)
SDfive$correlhi = SDfive$correl - sd(SDfive$correl)
model.RM1.SDfive.corrello.pes = lm(RM1.pes ~ corrello*levels, data = SDfive)
summary(model.RM1.SDfive.corrello.pes)
model.RM1.SDfive.correlhi.pes = lm(RM1.pes ~ correlhi*levels, data = SDfive)
summary(model.RM1.SDfive.correlhi.pes)

SDthree$corrello = SDthree$correl + sd(SDthree$correl)
SDthree$correlhi = SDthree$correl - sd(SDthree$correl)
model.RM1.SDthree.corrello.pes = lm(RM1.pes ~ corrello*levels, data = SDthree)
summary(model.RM1.SDthree.corrello.pes)
model.RM1.SDthree.correlhi.pes = lm(RM1.pes ~ correlhi*levels, data = SDthree)
summary(model.RM1.SDthree.correlhi.pes)

SDone$corrello = SDone$correl + sd(SDone$correl)
SDone$correlhi = SDone$correl - sd(SDone$correl)
model.RM1.SDone.corrello.ges = lm(RM1.ges ~ corrello*levels, data = SDone)
summary(model.RM1.SDone.corrello.ges)
model.RM1.SDone.correlhi.ges = lm(RM1.ges ~ correlhi*levels, data = SDone)
summary(model.RM1.SDone.correlhi.ges)
model.RM1.SDone.corrello.fes = lm(RM1.fes ~ corrello*levels, data = SDone)
summary(model.RM1.SDone.corrello.fes)
model.RM1.SDone.correlhi.fes = lm(RM1.fes ~ correlhi*levels, data = SDone)
summary(model.RM1.SDone.correlhi.fes)
model.BN2.SDone.corrello.pos = lm(BN2.pos ~ corrello*levels, data = SDone)
summary(model.BN2.SDone.corrello.pos)
model.BN2.SDone.correlhi.pos = lm(BN2.pos ~ correlhi*levels, data = SDone)
summary(model.BN2.SDone.correlhi.pos)
#means RM1 split by SD split by levels FOS
SDfive$levelslo = SDfive$levels + sd(SDfive$levels)
SDfive$levelshi = SDfive$levels - sd(SDfive$levels)
model.RM1.SDfive.levelslo.fos = lm(RM1.fos ~ levelslo*N, data = SDfive)
summary(model.RM1.SDfive.levelslo.fos)
model.RM1.SDfive.levelshi.fos = lm(RM1.fos ~ levelshi*N, data = SDfive)
summary(model.RM1.SDfive.levelshi.fos)

SDthree$levelslo = SDthree$levels + sd(SDthree$levels)
SDthree$levelshi = SDthree$levels - sd(SDthree$levels)
model.RM1.SDthree.levelslo.fos = lm(RM1.fos ~ levelslo*N, data = SDthree)
summary(model.RM1.SDthree.levelslo.fos)
model.RM1.SDthree.levelshi.fos = lm(RM1.fos ~ levelshi*N, data = SDthree)
summary(model.RM1.SDthree.levelshi.fos)

SDone$levelslo = SDone$levels + sd(SDone$levels)
SDone$levelshi = SDone$levels - sd(SDone$levels)
model.RM1.SDone.levelslo.fos = lm(RM1.fos ~ levelslo*N, data = SDone)
summary(model.RM1.SDone.levelslo.fos)
model.RM1.SDone.levelshi.fos = lm(RM1.fos ~ levelshi*N, data = SDone)
summary(model.RM1.SDone.levelshi.fos)

#means RM1 split by SD split by levels POS
SDfive$levelslo = SDfive$levels + sd(SDfive$levels)
SDfive$levelshi = SDfive$levels - sd(SDfive$levels)
model.BN2.SDfive.levelslo.pos = lm(BN2.pos ~ levelslo*N, data = SDfive)
summary(model.BN2.SDfive.levelslo.pos)
model.BN2.SDfive.levelshi.pos = lm(BN2.pos ~ levelshi*N, data = SDfive)
summary(model.BN2.SDfive.levelshi.pos)

SDthree$levelslo = SDthree$levels + sd(SDthree$levels)
SDthree$levelshi = SDthree$levels - sd(SDthree$levels)
model.BN2.SDthree.levelslo.pos = lm(BN2.pos ~ levelslo*N, data = SDthree)
summary(model.BN2.SDthree.levelslo.pos)
model.BN2.SDthree.levelshi.pos = lm(BN2.pos ~ levelshi*N, data = SDthree)
summary(model.BN2.SDthree.levelshi.pos)




##means RM2 simple slopes RM2 split by SD
model.RM2.sdfive = lm(RM2.ges ~ N*levels*correl, data = SDfive)
summary(model.RM2.sdfive)
model.RM2.sdthree = lm(RM2.ges ~ N*levels*correl, data = SDthree)
summary(model.RM2.sdthree)
model.RM2.sdone = lm(RM2.ges ~ N*levels*correl, data = SDone)
summary(model.RM2.sdone)

model.RM2.sdfive.pes = lm(RM2.pes ~ N*levels*correl, data = SDfive)
summary(model.RM2.sdfive.pes)
model.RM2.sdthree.pes = lm(RM2.pes ~ N*levels*correl, data = SDthree)
summary(model.RM2.sdthree.pes)
model.RM2.sdone.pes = lm(RM2.pes ~ N*levels*correl, data = SDone)
summary(model.RM2.sdone.pes)

model.RM2.sdfive.fes = lm(RM2.fes ~ N*levels*correl, data = SDfive)
summary(model.RM2.sdfive.fes)
model.RM2.sdthree.fes = lm(RM2.fes ~ N*levels*correl, data = SDthree)
summary(model.RM2.sdthree.fes)
model.RM2.sdone.fes = lm(RM2.fes ~ N*levels*correl, data = SDone)
summary(model.RM2.sdone.fes)

model.RM2.sdfive.fos = lm(RM2.fos ~ N*levels*correl, data = SDfive)
summary(model.RM2.sdfive.fos)
model.RM2.sdthree.fos = lm(RM2.fos ~ N*levels*correl, data = SDthree)
summary(model.RM2.sdthree.fos)
model.RM2.sdone.fos = lm(RM2.fos ~ N*levels*correl, data = SDone)
summary(model.RM2.sdone.fos)

model.RM2.sdfive.pos = lm(RM2.pos ~ N*levels*correl, data = SDfive)
summary(model.RM2.sdfive.pos)
model.RM2.sdthree.pos = lm(RM2.pos ~ N*levels*correl, data = SDthree)
summary(model.RM2.sdthree.pos)
model.RM2.sdone.pos = lm(RM2.pos ~ N*levels*correl, data = SDone)
summary(model.RM2.sdone.pos)


#means RM2 split by SD split by Cor
model.RM2.SDfive.corrello.pes = lm(RM2.pes ~ corrello*levels, data = SDfive)
summary(model.RM2.SDfive.corrello.pes)
model.RM2.SDfive.correlhi.pes = lm(RM2.pes ~ correlhi*levels, data = SDfive)
summary(model.RM2.SDfive.correlhi.pes)

model.RM2.SDthree.corrello.pes = lm(RM2.pes ~ corrello*levels, data = SDthree)
summary(model.RM2.SDthree.corrello.pes)
model.RM2.SDthree.correlhi.pes = lm(RM2.pes ~ correlhi*levels, data = SDthree)
summary(model.RM2.SDthree.correlhi.pes)

#means RM2 split by SD split by levels FOS
model.RM2.SDfive.levelslo.fos = lm(RM2.fos ~ levelslo*N, data = SDfive)
summary(model.RM2.SDfive.levelslo.fos)
model.RM2.SDfive.levelshi.fos = lm(RM2.fos ~ levelshi*N, data = SDfive)
summary(model.RM2.SDfive.levelshi.fos)

model.RM2.SDthree.levelslo.fos = lm(RM2.fos ~ levelslo*N, data = SDthree)
summary(model.RM2.SDthree.levelslo.fos)
model.RM2.SDthree.levelshi.fos = lm(RM2.fos ~ levelshi*N, data = SDthree)
summary(model.RM2.SDthree.levelshi.fos)

model.RM2.SDone.levelslo.fos = lm(RM2.fos ~ levelslo*N, data = SDone)
summary(model.RM2.SDone.levelslo.fos)
model.RM2.SDone.levelshi.fos = lm(RM2.fos ~ levelshi*N, data = SDone)
summary(model.RM2.SDone.levelshi.fos)

#means RM2 split by SD split by levels POS
model.RM2.SDfive.levelslo.pos = lm(RM2.pos ~ levelslo*N, data = SDfive)
summary(model.RM2.SDfive.levelslo.pos)
model.RM2.SDfive.levelshi.pos = lm(RM2.pos ~ levelshi*N, data = SDfive)
summary(model.RM2.SDfive.levelshi.pos)

model.RM2.SDthree.levelslo.pos = lm(RM2.pos ~ levelslo*N, data = SDthree)
summary(model.RM2.SDthree.levelslo.pos)
model.RM2.SDthree.levelshi.pos = lm(RM2.pos ~ levelshi*N, data = SDthree)
summary(model.RM2.SDthree.levelshi.pos)







####are there differences in SD by these values?####
model.RM1.sdover = lm(RM1.ges ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM1.sdover)
model.RM1.sdover.pes = lm(RM1.pes ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM1.sdover.pes)
model.RM1.sdover.fes = lm(RM1.fes ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM1.sdover.fes)
model.RM1.sdover.fos = lm(RM1.fos ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM1.sdover.fos)
model.BN2.sdover.pos = lm(BN2.pos ~ N*levels*correl*stdev, data = stdev_data)
summary(model.BN2.sdover.pos)

model.RM2.sdover = lm(RM2.ges ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM2.sdover)
model.RM2.sdover.pes = lm(RM2.pes ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM2.sdover.pes)
model.RM2.sdover.fes = lm(RM2.fes ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM2.sdover.fes)
model.RM2.sdover.fos = lm(RM2.fos ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM2.sdover.fos)
model.RM2.sdover.pos = lm(RM2.pos ~ N*levels*correl*stdev, data = stdev_data)
summary(model.RM2.sdover.pos)

        
#RM1 SD split by SD
SDfiveSD = subset(stdev_data, stdev == 5)
SDthreeSD = subset(stdev_data, stdev == 3)
SDoneSD = subset(stdev_data, stdev == 1)

model.RM1.sd.sdfive = lm(RM1.ges ~ N*levels*correl, data = SDfiveSD)
summary(model.RM1.sd.sdfive)
model.RM1.sd.sdthree = lm(RM1.ges ~ N*levels*correl, data = SDthreeSD)
summary(model.RM1.sd.sdthree)
model.RM1.sd.sdone = lm(RM1.ges ~ N*levels*correl, data = SDoneSD)
summary(model.RM1.sd.sdone)

model.RM1.sd.sdfive.pes = lm(RM1.pes ~ N*levels*correl, data = SDfiveSD)
summary(model.RM1.sd.sdfive.pes)
model.RM1.sd.sdthree.pes = lm(RM1.pes ~ N*levels*correl, data = SDthreeSD)
summary(model.RM1.sd.sdthree.pes)
model.RM1.sd.sdone.pes = lm(RM1.pes ~ N*levels*correl, data = SDoneSD)
summary(model.RM1.sd.sdone.pes)

model.RM1.sd.sdfive.fes = lm(RM1.fes ~ N*levels*correl, data = SDfiveSD)
summary(model.RM1.sd.sdfive.fes)
model.RM1.sd.sdthree.fes = lm(RM1.fes ~ N*levels*correl, data = SDthreeSD)
summary(model.RM1.sd.sdthree.fes)
model.RM1.sd.sdone.fes = lm(RM1.fes ~ N*levels*correl, data = SDoneSD)
summary(model.RM1.sd.sdone.fes)

model.RM1.sd.sdfive.fos = lm(RM1.fos ~ N*levels*correl, data = SDfiveSD)
summary(model.RM1.sd.sdfive.fos)
model.RM1.sd.sdthree.fos = lm(RM1.fos ~ N*levels*correl, data = SDthreeSD)
summary(model.RM1.sd.sdthree.fos)
model.RM1.sd.sdone.fos = lm(RM1.fos ~ N*levels*correl, data = SDoneSD)
summary(model.RM1.sd.sdone.fos)

model.BN2.sd.sdfive.pos = lm(BN2.pos ~ N*levels*correl, data = SDfiveSD)
summary(model.BN2.sd.sdfive.pos)
model.BN2.sd.sdthree.pos = lm(BN2.pos ~ N*levels*correl, data = SDthreeSD)
summary(model.BN2.sd.sdthree.pos)
model.BN2.sd.sdone.pos = lm(BN2.pos ~ N*levels*correl, data = SDoneSD)
summary(model.BN2.sd.sdone.pos)  


#SD RM1 split by SD split and split by Cor
SDoneSD$corrello = SDoneSD$correl + sd(SDoneSD$correl)
SDoneSD$correlhi = SDoneSD$correl - sd(SDoneSD$correl)
model.RM1.SD.SDone.corrello.ges = lm(RM1.ges ~ corrello*levels*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.ges)
model.RM1.SD.SDone.correlhi.ges = lm(RM1.ges ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.ges)

model.RM1.SD.SDone.corrello.pes = lm(RM1.pes ~ corrello*levels*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.pes)
model.RM1.SD.SDone.correlhi.pes = lm(RM1.pes ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.pes)

model.RM1.SD.SDone.corrello.fes = lm(RM1.fes ~ corrello*levels*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.fes)
model.RM1.SD.SDone.correlhi.fes = lm(RM1.fes ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.fes)

model.RM1.SD.SDone.corrello.fos = lm(RM1.fos ~ corrello*levels*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.fos)
model.RM1.SD.SDone.correlhi.fos = lm(RM1.fos ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.fos)

model.BN2.SD.SDone.corrello.pos = lm(BN2.pos ~ corrello*levels*N, data = SDoneSD)
summary(model.BN2.SD.SDone.corrello.pos)
model.BN2.SD.SDone.correlhi.pos = lm(BN2.pos ~ correlhi*levels*N, data = SDoneSD)
summary(model.BN2.SD.SDone.correlhi.pos)


#SD RM1 split by SD split by Cor split by levels
SDoneSD$levelslo = SDoneSD$levels + sd(SDoneSD$levels)
SDoneSD$levelshi = SDoneSD$levels - sd(SDoneSD$levels)

model.RM1.SD.SDone.corrello.levelslo.ges = lm(RM1.ges ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.levelslo.ges)
model.RM1.SD.SDone.corrello.levelshi.ges = lm(RM1.ges ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.levelshi.ges)
model.RM1.SD.SDone.correlhi.levelslo.ges = lm(RM1.ges ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.levelslo.ges)        
model.RM1.SD.SDone.correlhi.levelshi.ges = lm(RM1.ges ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.levelshi.ges)

model.RM1.SD.SDone.corrello.levelslo.pes = lm(RM1.pes ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.levelslo.pes)
model.RM1.SD.SDone.corrello.levelshi.pes = lm(RM1.pes ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.levelshi.pes)
model.RM1.SD.SDone.correlhi.levelslo.pes = lm(RM1.pes ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.levelslo.pes)        
model.RM1.SD.SDone.correlhi.levelshi.pes = lm(RM1.pes ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.levelshi.pes) 

model.RM1.SD.SDone.corrello.levelslo.fes = lm(RM1.fes ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.levelslo.fes)
model.RM1.SD.SDone.corrello.levelshi.fes = lm(RM1.fes ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.levelshi.fes)
model.RM1.SD.SDone.correlhi.levelslo.fes = lm(RM1.fes ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.levelslo.fes)        
model.RM1.SD.SDone.correlhi.levelshi.fes = lm(RM1.fes ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.levelshi.fes)

model.RM1.SD.SDone.corrello.levelslo.fos = lm(RM1.fos ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.levelslo.fos)
model.RM1.SD.SDone.corrello.levelshi.fos = lm(RM1.fos ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM1.SD.SDone.corrello.levelshi.fos)
model.RM1.SD.SDone.correlhi.levelslo.fos = lm(RM1.fos ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.levelslo.fos)        
model.RM1.SD.SDone.correlhi.levelshi.fos = lm(RM1.fos ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM1.SD.SDone.correlhi.levelshi.fos)

model.BN2.SD.SDone.corrello.levelslo.pos = lm(BN2.pos ~ corrello*levelslo*N, data = SDoneSD)
summary(model.BN2.SD.SDone.corrello.levelslo.pos)
model.BN2.SD.SDone.corrello.levelshi.pos = lm(BN2.pos ~ corrello*levelshi*N, data = SDoneSD)
summary(model.BN2.SD.SDone.corrello.levelshi.pos)
model.BN2.SD.SDone.correlhi.levelslo.pos = lm(BN2.pos ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.BN2.SD.SDone.correlhi.levelslo.pos)        
model.BN2.SD.SDone.correlhi.levelshi.pos = lm(BN2.pos ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.BN2.SD.SDone.correlhi.levelshi.pos)


#SD RM1/BN2 SD5 and SD3 split by SD split by levels (N:levels interaction)
SDfiveSD$levelslo = SDfiveSD$levels + sd(SDfiveSD$levels)
SDfiveSD$levelshi = SDfiveSD$levels - sd(SDfiveSD$levels)
SDthreeSD$levelslo = SDthreeSD$levels + sd(SDthreeSD$levels)
SDthreeSD$levelshi = SDthreeSD$levels - sd(SDthreeSD$levels)

model.RM1.SD.SDfive.levelslo.ges = lm(RM1.ges ~ levelslo*N, data = SDfiveSD)
summary(model.RM1.SD.SDfive.levelslo.ges)
model.RM1.SD.SDfive.levelshi.ges = lm(RM1.ges ~ levelshi*N, data = SDfiveSD)
summary(model.RM1.SD.SDfive.levelshi.ges)
model.RM1.SD.SDthree.levelslo.ges = lm(RM1.ges ~ levelslo*N, data = SDthreeSD)
summary(model.RM1.SD.SDthree.levelslo.ges)
model.RM1.SD.SDthree.levelshi.ges = lm(RM1.ges ~ levelshi*N, data = SDthreeSD)
summary(model.RM1.SD.SDthree.levelshi.ges)

model.RM1.SD.SDfive.levelslo.pes = lm(RM1.pes ~ levelslo*N, data = SDfiveSD)
summary(model.RM1.SD.SDfive.levelslo.pes)
model.RM1.SD.SDfive.levelshi.pes = lm(RM1.pes ~ levelshi*N, data = SDfiveSD)
summary(model.RM1.SD.SDfive.levelshi.pes)
model.RM1.SD.SDthree.levelslo.pes = lm(RM1.pes ~ levelslo*N, data = SDthreeSD)
summary(model.RM1.SD.SDthree.levelslo.pes)
model.RM1.SD.SDthree.levelshi.pes = lm(RM1.pes ~ levelshi*N, data = SDthreeSD)
summary(model.RM1.SD.SDthree.levelshi.pes)

model.RM1.SD.SDfive.levelslo.fes = lm(RM1.fes ~ levelslo*N, data = SDfiveSD)
summary(model.RM1.SD.SDfive.levelslo.fes)
model.RM1.SD.SDfive.levelshi.fes = lm(RM1.fes ~ levelshi*N, data = SDfiveSD)
summary(model.RM1.SD.SDfive.levelshi.fes)
model.RM1.SD.SDthree.levelslo.fes = lm(RM1.fes ~ levelslo*N, data = SDthreeSD)
summary(model.RM1.SD.SDthree.levelslo.fes)
model.RM1.SD.SDthree.levelshi.fes = lm(RM1.fes ~ levelshi*N, data = SDthreeSD)
summary(model.RM1.SD.SDthree.levelshi.fes)

model.RM1.SD.SDfive.levelslo.fos = lm(RM1.fos ~ levelslo*N, data = SDfiveSD)
summary(model.RM1.SD.SDfive.levelslo.fos)
model.RM1.SD.SDfive.levelshi.fos = lm(RM1.fos ~ levelshi*N, data = SDfiveSD)
summary(model.RM1.SD.SDfive.levelshi.fos)
model.RM1.SD.SDthree.levelslo.fos = lm(RM1.fos ~ levelslo*N, data = SDthreeSD)
summary(model.RM1.SD.SDthree.levelslo.fos)
model.RM1.SD.SDthree.levelshi.fos = lm(RM1.fos ~ levelshi*N, data = SDthreeSD)
summary(model.RM1.SD.SDthree.levelshi.fos)

model.BN2.SD.SDfive.levelslo.pos = lm(BN2.pos ~ levelslo*N, data = SDfiveSD)
summary(model.BN2.SD.SDfive.levelslo.pos)
model.BN2.SD.SDfive.levelshi.pos = lm(BN2.pos ~ levelshi*N, data = SDfiveSD)
summary(model.BN2.SD.SDfive.levelshi.pos)
model.BN2.SD.SDthree.levelslo.pos = lm(BN2.pos ~ levelslo*N, data = SDthreeSD)
summary(model.BN2.SD.SDthree.levelslo.pos)
model.BN2.SD.SDthree.levelshi.pos = lm(BN2.pos ~ levelshi*N, data = SDthreeSD)
summary(model.BN2.SD.SDthree.levelshi.pos)



#SD RM2 split by SD
model.RM2.sd.sdfive = lm(RM2.ges ~ N*levels*correl, data = SDfiveSD)
summary(model.RM2.sd.sdfive)
model.RM2.sd.sdthree = lm(RM2.ges ~ N*levels*correl, data = SDthreeSD)
summary(model.RM2.sd.sdthree)
model.RM2.sd.sdone = lm(RM2.ges ~ N*levels*correl, data = SDoneSD)
summary(model.RM2.sd.sdone)

model.RM2.sd.sdfive.pes = lm(RM2.pes ~ N*levels*correl, data = SDfiveSD)
summary(model.RM2.sd.sdfive.pes)
model.RM2.sd.sdthree.pes = lm(RM2.pes ~ N*levels*correl, data = SDthreeSD)
summary(model.RM2.sd.sdthree.pes)
model.RM2.sd.sdone.pes = lm(RM2.pes ~ N*levels*correl, data = SDoneSD)
summary(model.RM2.sd.sdone.pes)

model.RM2.sd.sdfive.fes = lm(RM2.fes ~ N*levels*correl, data = SDfiveSD)
summary(model.RM2.sd.sdfive.fes)
model.RM2.sd.sdthree.fes = lm(RM2.fes ~ N*levels*correl, data = SDthreeSD)
summary(model.RM2.sd.sdthree.fes)
model.RM2.sd.sdone.fes = lm(RM2.fes ~ N*levels*correl, data = SDoneSD)
summary(model.RM2.sd.sdone.fes)

model.RM2.sd.sdfive.fos = lm(RM2.fos ~ N*levels*correl, data = SDfiveSD)
summary(model.RM2.sd.sdfive.fos)
model.RM2.sd.sdthree.fos = lm(RM2.fos ~ N*levels*correl, data = SDthreeSD)
summary(model.RM2.sd.sdthree.fos)
model.RM2.sd.sdone.fos = lm(RM2.fos ~ N*levels*correl, data = SDoneSD)
summary(model.RM2.sd.sdone.fos)

model.RM2.sd.sdfive.pos = lm(RM2.pos ~ N*levels*correl, data = SDfiveSD)
summary(model.RM2.sd.sdfive.pos)
model.RM2.sd.sdthree.pos = lm(RM2.pos ~ N*levels*correl, data = SDthreeSD)
summary(model.RM2.sd.sdthree.pos)
model.RM2.sd.sdone.pos = lm(RM2.pos ~ N*levels*correl, data = SDoneSD)
summary(model.RM2.sd.sdone.pos) 



#SD RM2 split by SD split by levels
SDfiveSD$levelslo = SDfiveSD$levels + sd(SDfiveSD$levels)
SDfiveSD$levelshi = SDfiveSD$levels - sd(SDfiveSD$levels)
SDthreeSD$levelslo = SDthreeSD$levels + sd(SDthreeSD$levels)
SDthreeSD$levelshi = SDthreeSD$levels - sd(SDthreeSD$levels)

model.RM2.SD.SDthree.levelslo.pes = lm(RM2.pes ~ levelslo*N, data = SDthreeSD)
summary(model.RM2.SD.SDthree.levelslo.pes)
model.RM2.SD.SDthree.levelshi.pes = lm(RM2.pes ~ levelshi*N, data = SDthreeSD)
summary(model.RM2.SD.SDthree.levelshi.pes)

model.RM2.SD.SDfive.levelslo.pos = lm(RM2.pos ~ levelslo*N, data = SDfiveSD)
summary(model.RM2.SD.SDfive.levelslo.pos)
model.RM2.SD.SDfive.levelshi.pos = lm(RM2.pos ~ levelshi*N, data = SDfiveSD)
summary(model.RM2.SD.SDfive.levelshi.pos)

model.RM2.SD.SDthree.levelslo.pos = lm(RM2.pos ~ levelslo*N, data = SDthreeSD)
summary(model.RM2.SD.SDthree.levelslo.pos)
model.RM2.SD.SDthree.levelshi.pos = lm(RM2.pos ~ levelshi*N, data = SDthreeSD)
summary(model.RM2.SD.SDthree.levelshi.pos)


#SD RM2 split by SD (SD1) split by Corr
model.RM2.SD.SDone.corrello.ges = lm(RM2.ges ~ corrello*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.ges)
model.RM2.SD.SDone.correlhi.ges = lm(RM2.ges ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.ges)

model.RM2.SD.SDone.corrello.pes = lm(RM2.pes ~ corrello*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.pes)
model.RM2.SD.SDone.correlhi.pes = lm(RM2.pes ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.pes)

model.RM2.SD.SDone.corrello.fes = lm(RM2.fes ~ corrello*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.fes)
model.RM2.SD.SDone.correlhi.fes = lm(RM2.fes ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.fes)

model.RM2.SD.SDone.corrello.fos = lm(RM2.fos ~ corrello*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.fos)
model.RM2.SD.SDone.correlhi.fos = lm(RM2.fos ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.fos)

model.RM2.SD.SDone.corrello.pos = lm(RM2.pos ~ corrello*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.pos)
model.RM2.SD.SDone.correlhi.pos = lm(RM2.pos ~ correlhi*levels*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.pos)
        

#SD RM2 split by SD split by Cor split by levels
model.RM2.SD.SDone.corrello.levelslo.ges = lm(RM2.ges ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelslo.ges)
model.RM2.SD.SDone.corrello.levelshi.ges = lm(RM2.ges ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelshi.ges)
model.RM2.SD.SDone.correlhi.levelslo.ges = lm(RM2.ges ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelslo.ges)        
model.RM2.SD.SDone.correlhi.levelshi.ges = lm(RM2.ges ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelshi.ges)

model.RM2.SD.SDone.corrello.levelslo.pes = lm(RM2.pes ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelslo.pes)
model.RM2.SD.SDone.corrello.levelshi.pes = lm(RM2.pes ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelshi.pes)
model.RM2.SD.SDone.correlhi.levelslo.pes = lm(RM2.pes ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelslo.pes)        
model.RM2.SD.SDone.correlhi.levelshi.pes = lm(RM2.pes ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelshi.pes) 

model.RM2.SD.SDone.corrello.levelslo.fes = lm(RM2.fes ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelslo.fes)
model.RM2.SD.SDone.corrello.levelshi.fes = lm(RM2.fes ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelshi.fes)
model.RM2.SD.SDone.correlhi.levelslo.fes = lm(RM2.fes ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelslo.fes)        
model.RM2.SD.SDone.correlhi.levelshi.fes = lm(RM2.fes ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelshi.fes)

model.RM2.SD.SDone.corrello.levelslo.fos = lm(RM2.fos ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelslo.fos)
model.RM2.SD.SDone.corrello.levelshi.fos = lm(RM2.fos ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelshi.fos)
model.RM2.SD.SDone.correlhi.levelslo.fos = lm(RM2.fos ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelslo.fos)        
model.RM2.SD.SDone.correlhi.levelshi.fos = lm(RM2.fos ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelshi.fos)

model.RM2.SD.SDone.corrello.levelslo.pos = lm(RM2.pos ~ corrello*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelslo.pos)
model.RM2.SD.SDone.corrello.levelshi.pos = lm(RM2.pos ~ corrello*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.corrello.levelshi.pos)
model.RM2.SD.SDone.correlhi.levelslo.pos = lm(RM2.pos ~ correlhi*levelslo*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelslo.pos)        
model.RM2.SD.SDone.correlhi.levelshi.pos = lm(RM2.pos ~ correlhi*levelshi*N, data = SDoneSD)
summary(model.RM2.SD.SDone.correlhi.levelshi.pos)




###########################################################################
#######BN2 for PES, MIX/RM2 POS

##means
model.BN2.meanover.pes = lm(BN2.pes ~ N*levels*correl*stdev, data = mean_data)
summary(model.BN2.meanover.pes)
model.MIX.meanover.pos = lm(MIX.pos ~ N*levels*correl*stdev, data = mean_data)
summary(model.MIX.meanover.pos)
#split by SD
model.BN2.sdfive.pes = lm(BN2.pes ~ N*levels*correl, data = SDfive)
summary(model.BN2.sdfive.pes)
model.BN2.sdthree.pes = lm(BN2.pes ~ N*levels*correl, data = SDthree)
summary(model.BN2.sdthree.pes)
model.BN2.sdone.pes = lm(BN2.pes ~ N*levels*correl, data = SDone)
summary(model.BN2.sdone.pes)
model.MIX.sdfive.pos = lm(MIX.pos ~ N*levels*correl, data = SDfive)
summary(model.MIX.sdfive.pos)
model.MIX.sdthree.pos = lm(MIX.pos ~ N*levels*correl, data = SDthree)
summary(model.MIX.sdthree.pos)
model.MIX.sdone.pos = lm(MIX.pos ~ N*levels*correl, data = SDone)
summary(model.MIX.sdone.pos)

#split by SD split by levels (n:level interaction for pos)
model.MIX.sdfive.levello.pos = lm(MIX.pos ~ N*levelslo, data = SDfive)
summary(model.MIX.sdfive.levello.pos)
model.MIX.sdfive.levelhi.pos = lm(MIX.pos ~ N*levelshi, data = SDfive)
summary(model.MIX.sdfive.levelhi.pos)
model.MIX.sdthree.levello.pos = lm(MIX.pos ~ N*levelslo, data = SDthree)
summary(model.MIX.sdthree.levello.pos)
model.MIX.sdthree.levelhi.pos = lm(MIX.pos ~ N*levelshi, data = SDthree)
summary(model.MIX.sdthree.levelhi.pos)
model.MIX.sdone.levello.pos = lm(MIX.pos ~ N*levelslo, data = SDone)
summary(model.MIX.sdone.levello.pos)
model.MIX.sdone.levelhi.pos = lm(MIX.pos ~ N*levelshi, data = SDone)
summary(model.MIX.sdone.levelhi.pos)
#split by SD split by corr
SDone$corrello = SDone$correl + sd(SDone$correl)
SDone$correlhi = SDone$correl - sd(SDone$correl)
SDthree$corrello = SDthree$correl + sd(SDthree$correl)
SDthree$correlhi = SDthree$correl - sd(SDthree$correl)
SDfive$corrello = SDfive$correl + sd(SDfive$correl)
SDfive$correlhi = SDfive$correl - sd(SDfive$correl)
model.BN2.SDone.corrello.pes = lm(BN2.pes ~ corrello*levels, data = SDone)
summary(model.BN2.SDone.corrello.pes)
model.BN2.SDone.correlhi.pes = lm(BN2.pes ~ correlhi*levels, data = SDone)
summary(model.BN2.SDone.correlhi.pes)
model.MIX.SDfive.corrello.pos = lm(MIX.pos ~ corrello*levels, data = SDfive)
summary(model.MIX.SDfive.corrello.pos)
model.MIX.SDfive.correlhi.pos = lm(MIX.pos ~ correlhi*levels, data = SDfive)
summary(model.MIX.SDfive.correlhi.pos)
model.MIX.SDthree.corrello.pos = lm(MIX.pos ~ corrello*levels, data = SDthree)
summary(model.MIX.SDthree.corrello.pos)
model.MIX.SDthree.correlhi.pos = lm(MIX.pos ~ correlhi*levels, data = SDthree)
summary(model.MIX.SDthree.correlhi.pos)
model.MIX.SDone.corrello.pos = lm(MIX.pos ~ corrello*levels, data = SDone)
summary(model.MIX.SDone.corrello.pos)
model.MIX.SDone.correlhi.pos = lm(MIX.pos ~ correlhi*levels, data = SDone)
summary(model.MIX.SDone.correlhi.pos)


##SD
model.BN2.sdover.pes = lm(BN2.pes ~ N*levels*correl*stdev, data = stdev_data)
summary(model.BN2.sdover.pes)
model.MIX.sdover.pos = lm(MIX.pos ~ N*levels*correl*stdev, data = stdev_data)
summary(model.MIX.sdover.pos)
#split by sd
model.BN2.sd.sdfive.pes = lm(BN2.pes ~ N*levels*correl, data = SDfiveSD)
summary(model.BN2.sd.sdfive.pes)
model.BN2.sd.sdthree.pes = lm(BN2.pes ~ N*levels*correl, data = SDthreeSD)
summary(model.BN2.sd.sdthree.pes)
model.BN2.sd.sdone.pes = lm(BN2.pes ~ N*levels*correl, data = SDoneSD)
summary(model.BN2.sd.sdone.pes)
model.MIX.sd.sdfive.pos = lm(MIX.pos ~ N*levels*correl, data = SDfiveSD)
summary(model.MIX.sd.sdfive.pos)
model.MIX.sd.sdthree.pos = lm(MIX.pos ~ N*levels*correl, data = SDthreeSD)
summary(model.MIX.sd.sdthree.pos)
model.MIX.sd.sdone.pos = lm(MIX.pos ~ N*levels*correl, data = SDoneSD)
summary(model.MIX.sd.sdone.pos)
#split by sd split by corr
model.BN2.SD.SDone.corrello.pes = lm(BN2.pes ~ corrello*levels, data = SDoneSD)
summary(model.BN2.SD.SDone.corrello.pes)
model.BN2.SD.SDone.correlhi.pes = lm(BN2.pes ~ correlhi*levels, data = SDoneSD)
summary(model.BN2.SD.SDone.correlhi.pes)
model.MIX.SD.SDone.corrello.pos = lm(MIX.pos ~ corrello*levels, data = SDoneSD)
summary(model.MIX.SD.SDone.corrello.pos)
model.MIX.SD.SDone.correlhi.pos = lm(MIX.pos ~ correlhi*levels, data = SDoneSD)
summary(model.MIX.SD.SDone.correlhi.pos)
#split by sd split by corr split by levels
model.BN2.SD.SDone.corrello.levelslo.pes = lm(BN2.pes ~ corrello*levelslo, data = SDoneSD)
summary(model.BN2.SD.SDone.corrello.levelslo.pes)
model.BN2.SD.SDone.corrello.levelshi.pes = lm(BN2.pes ~ corrello*levelshi, data = SDoneSD)
summary(model.BN2.SD.SDone.corrello.levelshi.pes)
model.BN2.SD.SDone.correlhi.levelslo.pes = lm(BN2.pes ~ correlhi*levelslo, data = SDoneSD)
summary(model.BN2.SD.SDone.correlhi.levelslo.pes)        
model.BN2.SD.SDone.correlhi.levelshi.pes = lm(BN2.pes ~ correlhi*levelshi, data = SDoneSD)
summary(model.BN2.SD.SDone.correlhi.levelshi.pes) 
model.MIX.SD.SDone.corrello.levelslo.pos = lm(MIX.pos ~ corrello*levelslo, data = SDoneSD)
summary(model.MIX.SD.SDone.corrello.levelslo.pos)
model.MIX.SD.SDone.corrello.levelshi.pos = lm(MIX.pos ~ corrello*levelshi, data = SDoneSD)
summary(model.MIX.SD.SDone.corrello.levelshi.pos)
model.MIX.SD.SDone.correlhi.levelslo.pos = lm(MIX.pos ~ correlhi*levelslo, data = SDoneSD)
summary(model.MIX.SD.SDone.correlhi.levelslo.pos)        
model.MIX.SD.SDone.correlhi.levelshi.pos = lm(MIX.pos ~ correlhi*levelshi, data = SDoneSD)
summary(model.MIX.SD.SDone.correlhi.levelshi.pos)
