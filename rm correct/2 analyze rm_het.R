setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/eta-sims/rm correct")

master = read.csv("sim_rm_data_het.csv")
master = master[ , -c(1)]

####calculate eta and partial eta squared####
master$eta_full = master$RM1.ssm.main/(master$RM1.ssm.main + master$RM1.ssr.main + master$RM1.ssr.p)
master$eta_part = master$RM1.ssm.main/(master$RM1.ssm.main + master$RM1.ssr.main)

tapply(master$RM1.ges, list(master$levels, master$stdev), mean)
tapply(master$eta_full, list(master$levels, master$stdev), mean)
tapply(master$eta_part, list(master$levels, master$stdev), mean)

####calculate the confidence intervals####
library(MBESS)

master$gesLL = NA
master$gesUL = NA
master$etaLL = NA
master$etaUL = NA
master$pesLL = NA
master$pesUL = NA
master$gesLLF = NA
master$gesULF = NA
master$etaLLF = NA
master$etaULF = NA
master$pesLLF = NA
master$pesULF = NA
master$fakeF.ges = NA
master$fakeF.eta = NA
master$fakeF.pes = NA
master$NCP.gesLL = NA
master$NCP.gesUL = NA
master$NCP.etaLL = NA
master$NCP.etaUL = NA
master$NCP.pesLL = NA
master$NCP.pesUL = NA

for (i in 1:nrow(master)){

  ##calculate the CI based on R2
  master[ i , c("gesLL", "gesUL")] = ci.R2(R2 = master$RM1.ges[i],
                                           df.1 = master$RM1.dfm[i],
                                           df.2 = master$RM1.dfr[i],
                                           conf.level = .95)[c(1,3)]  
  
  master[ i , c("etaLL", "etaUL")] = ci.R2(R2 = master$eta_full[i],
                                           df.1 = master$RM1.dfm[i],
                                           df.2 = master$RM1.dfr[i],
                                           conf.level = .95)[c(1,3)]
  
  master[ i , c("pesLL", "pesUL")] = ci.R2(R2 = master$eta_part[i],
                                           df.1 = master$RM1.dfm[i],
                                           df.2 = master$RM1.dfr[i],
                                           conf.level = .95)[c(1,3)]
  
  ##calculate the CI based on real F only
  master[ i , c("gesLLF", "gesULF")] = ci.R2(df.1 = master$RM1.dfm[i],
                                           df.2 = master$RM1.dfr[i],
                                           conf.level = .95,
                                           F.value = master$RM1.F[i])[c(1,3)]  
  
  master[ i , c("etaLLF", "etaULF")] = ci.R2(df.1 = master$RM1.dfm[i],
                                           df.2 = master$RM1.dfr[i],
                                           conf.level = .95,
                                           F.value = master$RM1.F[i])[c(1,3)]
  
  master[ i , c("pesLLF", "pesULF")] = ci.R2(df.1 = master$RM1.dfm[i],
                                           df.2 = master$RM1.dfr[i],
                                           conf.level = .95,
                                           F.value = master$RM1.F[i])[c(1,3)]
  
  ##first, calculate fake F
  master$fakeF.ges[i] = Rsquare2F(R2 = master$RM1.ges[i],
                                  df.1 = master$RM1.dfm[i],
                                  df.2 = master$RM1.dfr[i])
  
  master$fakeF.eta[i] = Rsquare2F(R2 = master$eta_full[i],
                                  df.1 = master$RM1.dfm[i],
                                  df.2 = master$RM1.dfr[i])
  
  master$fakeF.pes[i] = Rsquare2F(R2 = master$eta_part[i],
                                  df.1 = master$RM1.dfm[i],
                                  df.2 = master$RM1.dfr[i])
  
  ##calculate NCP based on the fake F
  master[ i , c("NCP.gesLL", "NCP.gesUL")] = conf.limits.ncf(master$fakeF.ges[i], 
                                                           df.1 = master$RM1.dfm[i],
                                                           df.2 = master$RM1.dfr[i],
                                                           conf.level = .95)[c(1,3)]
  
  master[ i , c("NCP.etaLL", "NCP.etaUL")] = conf.limits.ncf(master$fakeF.eta[i], 
                                                             df.1 = master$RM1.dfm[i],
                                                             df.2 = master$RM1.dfr[i],
                                                             conf.level = .95)[c(1,3)]
  
  master[ i , c("NCP.pesLL", "NCP.pesUL")] = conf.limits.ncf(master$fakeF.pes[i], 
                                                             df.1 = master$RM1.dfm[i],
                                                             df.2 = master$RM1.dfr[i],
                                                             conf.level = .95)[c(1,3)]
  
  master$p.value[i] = pf(master$RM1.F[i], master$RM1.dfm[i], 
                         master$RM1.dfr[i], lower.tail = F)
  
  
  
}

####deal with the NAs in NCP, which MBESS lists as 0####
master$NCP.gesLL[ is.na(master$NCP.gesLL) ] = 0
master$NCP.gesUL[ is.na(master$NCP.gesUL) ] = 0
master$NCP.etaLL[ is.na(master$NCP.etaLL) ] = 0
master$NCP.etaUL[ is.na(master$NCP.etaUL) ] = 0
master$NCP.pesLL[ is.na(master$NCP.pesLL) ] = 0
master$NCP.pesUL[ is.na(master$NCP.pesUL) ] = 0

####calculate the GG corrected CI####
master$cor.gesLLgg = (master$NCP.gesLL * master$RM1.gg) / (master$NCP.gesLL * master$RM1.gg + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.gesULgg = (master$NCP.gesUL * master$RM1.gg) / (master$NCP.gesUL * master$RM1.gg + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.etaLLgg = (master$NCP.etaLL * master$RM1.gg) / (master$NCP.etaLL * master$RM1.gg + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.etaULgg = (master$NCP.etaUL * master$RM1.gg) / (master$NCP.etaUL * master$RM1.gg + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.pesLLgg = (master$NCP.pesLL * master$RM1.gg) / (master$NCP.pesLL * master$RM1.gg + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.pesULgg = (master$NCP.pesUL * master$RM1.gg) / (master$NCP.pesUL * master$RM1.gg + master$RM1.dfm + master$RM1.dfr + 1)

####calculate the HF corrected CI####
master$cor.gesLLhf = (master$NCP.gesLL * master$RM1.hf) / (master$NCP.gesLL * master$RM1.hf + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.gesULhf = (master$NCP.gesUL * master$RM1.hf) / (master$NCP.gesUL * master$RM1.hf + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.etaLLhf = (master$NCP.etaLL * master$RM1.hf) / (master$NCP.etaLL * master$RM1.hf + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.etaULhf = (master$NCP.etaUL * master$RM1.hf) / (master$NCP.etaUL * master$RM1.hf + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.pesLLhf = (master$NCP.pesLL * master$RM1.hf) / (master$NCP.pesLL * master$RM1.hf + master$RM1.dfm + master$RM1.dfr + 1)
master$cor.pesULhf = (master$NCP.pesUL * master$RM1.hf) / (master$NCP.pesUL * master$RM1.hf + master$RM1.dfm + master$RM1.dfr + 1)

write.csv(master, "CI_master_het.csv", row.names = F)

summary(master)

####figure out the best CI####
##for each CI type what is the % of time it's in the CI

##ges
master$ges1 = master$RM1.ges > master$gesLL & master$RM1.ges < master$gesUL
master$ges2 = master$RM1.ges > master$gesLLF & master$RM1.ges < master$gesULF
master$ges3 = master$RM1.ges > master$cor.gesLLgg & master$RM1.ges < master$cor.gesULgg
master$ges4 = master$RM1.ges > master$cor.gesLLhf & master$RM1.ges < master$cor.gesULhf

onlygg = master[master$RM1.gg < .75 & master$RM1.hf < .75 & master$RM1.Wp < .05,  ]
onlyhf = master[(master$RM1.gg > .75 | master$RM1.hf > .75) & master$RM1.Wp < .05,  ]

tapply(master$ges1, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$ges2, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$ges3, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$ges4, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})

tapply(onlygg$ges3, list(onlygg$N, onlygg$levels, onlygg$stdev, onlygg$correl), 
       function (x) {sum(x)/length(x)})
tapply(onlyhf$ges4, list(onlyhf$N, onlyhf$levels, onlyhf$stdev, onlyhf$correl), 
       function (x) {sum(x)/length(x)})

mean(master$ges1);mean(master$ges2);mean(master$ges3);mean(master$ges4)
mean(onlygg$ges3);mean(onlyhf$ges4)

##eta
master$eta1 = master$eta_full > master$etaLL & master$eta_full < master$etaUL
master$eta2 = master$eta_full > master$etaLLF & master$eta_full < master$etaULF
master$eta3 = master$eta_full > master$cor.etaLLgg & master$eta_full < master$cor.etaULgg
master$eta4 = master$eta_full > master$cor.etaLLhf & master$eta_full < master$cor.etaULhf

tapply(master$eta1, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$eta2, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$eta3, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$eta4, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})

onlygg = master[master$RM1.gg < .75 & master$RM1.hf < .75 & master$RM1.Wp < .05,  ]
onlyhf = master[(master$RM1.gg > .75 | master$RM1.hf > .75) & master$RM1.Wp < .05,  ]

tapply(onlygg$eta3, list(onlygg$N, onlygg$levels, onlygg$stdev, onlygg$correl), 
       function (x) {sum(x)/length(x)})
tapply(onlyhf$eta4, list(onlyhf$N, onlyhf$levels, onlyhf$stdev, onlyhf$correl), 
       function (x) {sum(x)/length(x)})

mean(master$eta1);mean(master$eta2);mean(master$eta3);mean(master$eta4)
mean(onlygg$eta3);mean(onlyhf$eta4)

##pes
master$pes1 = master$eta_part > master$pesLL & master$eta_part < master$pesUL
master$pes2 = master$eta_part > master$pesLLF & master$eta_part < master$pesULF
master$pes3 = master$eta_part > master$cor.pesLLgg & master$eta_part < master$cor.pesULgg
master$pes4 = master$eta_part > master$cor.pesLLhf & master$eta_part < master$cor.pesULhf

tapply(master$pes1, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$pes2, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$pes3, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$pes4, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})

onlygg = master[master$RM1.gg < .75 & master$RM1.hf < .75 & master$RM1.Wp < .05,  ]
onlyhf = master[(master$RM1.gg > .75 | master$RM1.hf > .75) & master$RM1.Wp < .05,  ]

tapply(onlygg$pes3, list(onlygg$N, onlygg$levels, onlygg$stdev, onlygg$correl), 
       function (x) {sum(x)/length(x)})
tapply(onlyhf$pes4, list(onlyhf$N, onlyhf$levels, onlyhf$stdev, onlyhf$correl), 
       function (x) {sum(x)/length(x)})

mean(master$pes1);mean(master$pes2);mean(master$pes3);mean(master$pes4)
mean(onlygg$pes3);mean(onlyhf$pes4)

####figure out how many times the "TRUE" effect size is within each CI####
level3 = sum((c(2.5, 3.0, 3.5) - mean(c(2.5, 3.0, 3.5)))^2) / 3
level4 = sum((c(2.5, 3.0, 3.5, 4.0) - mean(c(2.5, 3.0, 3.5, 4.0)))^2) / 4
level5 = sum((c(2.5, 3.0, 3.5, 4.0, 4.5) - mean(c(2.5, 3.0, 3.5, 4.0, 4.5)))^2) / 5
level6 = sum((c(2.5, 3.0, 3.5, 4.0, 4.5, 5.0) - mean(c(2.5, 3.0, 3.5, 4.0, 4.5, 5.0)))^2) / 6

pop3sd5 = level3 / (level3 + mean(c(5,5*2,5)))
pop3sd3 = level3 / (level3 + mean(c(3,3*2,3)))
pop3sd1 = level3 / (level3 + mean(c(1,1*2,1)))
pop4sd5 = level4 / (level4 + mean(c(5,5*2,5)))
pop4sd3 = level4 / (level4 + mean(c(3,3*2,3)))
pop4sd1 = level4 / (level4 + mean(c(1,1*2,1)))
pop5sd5 = level5 / (level5 + mean(c(5,5*2,5)))
pop5sd3 = level5 / (level5 + mean(c(3,3*2,3)))
pop5sd1 = level5 / (level5 + mean(c(1,1*2,1)))
pop6sd5 = level6 / (level6 + mean(c(5,5*2,5)))
pop6sd3 = level6 / (level6 + mean(c(3,3*2,3)))
pop6sd1 = level6 / (level6 + mean(c(1,1*2,1)))

master$pop = NA
master$pop[ master$levels == 3 & master$stdev == 5 ] = pop3sd5
master$pop[ master$levels == 3 & master$stdev == 3 ] = pop3sd3
master$pop[ master$levels == 3 & master$stdev == 1 ] = pop3sd1
master$pop[ master$levels == 4 & master$stdev == 5 ] = pop4sd5
master$pop[ master$levels == 4 & master$stdev == 3 ] = pop4sd3
master$pop[ master$levels == 4 & master$stdev == 1 ] = pop4sd1
master$pop[ master$levels == 5 & master$stdev == 5 ] = pop5sd5
master$pop[ master$levels == 5 & master$stdev == 3 ] = pop5sd3
master$pop[ master$levels == 5 & master$stdev == 1 ] = pop5sd1
master$pop[ master$levels == 6 & master$stdev == 5 ] = pop6sd5
master$pop[ master$levels == 6 & master$stdev == 3 ] = pop6sd3
master$pop[ master$levels == 6 & master$stdev == 1 ] = pop6sd1

master$TPges1 = master$pop > master$gesLL & master$pop < master$gesUL
master$TPges2 = master$pop > master$gesLLF & master$pop < master$gesULF
master$TPges3 = master$pop > master$cor.gesLLgg & master$pop < master$cor.gesULgg
master$TPges4 = master$pop > master$cor.gesLLhf & master$pop < master$cor.gesULhf
master$TPpes1 = master$pop > master$pesLL & master$pop < master$pesUL
master$TPpes2 = master$pop > master$pesLLF & master$pop < master$pesULF
master$TPpes3 = master$pop > master$cor.pesLLgg & master$pop < master$cor.pesULgg
master$TPpes4 = master$pop > master$cor.pesLLhf & master$pop < master$cor.pesULhf
master$TPeta1 = master$pop > master$etaLL & master$pop < master$etaUL
master$TPeta2 = master$pop > master$etaLLF & master$pop < master$etaULF
master$TPeta3 = master$pop > master$cor.etaLLgg & master$pop < master$cor.etaULgg
master$TPeta4 = master$pop > master$cor.etaLLhf & master$pop < master$cor.etaULhf

##recreate the smaller datasets with these new values
onlygg = master[master$RM1.gg < .75 & master$RM1.hf < .75 & master$RM1.Wp < .05,  ]
onlyhf = master[(master$RM1.gg > .75 | master$RM1.hf > .75) & master$RM1.Wp < .05,  ]

##TPges
tapply(master$TPges1, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPges2, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPges3, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPges4, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})

tapply(onlygg$TPges3, list(onlygg$N, onlygg$levels, onlygg$stdev, onlygg$correl), 
       function (x) {sum(x)/length(x)})
tapply(onlyhf$TPges4, list(onlyhf$N, onlyhf$levels, onlyhf$stdev, onlyhf$correl), 
       function (x) {sum(x)/length(x)})

mean(master$TPges1);mean(master$TPges2);mean(master$TPges3);mean(master$TPges4)
mean(onlygg$TPges3);mean(onlyhf$TPges4)

##TPeta
tapply(master$TPeta1, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPeta2, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPeta3, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPeta4, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})

tapply(onlygg$TPeta3, list(onlygg$N, onlygg$levels, onlygg$stdev, onlygg$correl), 
       function (x) {sum(x)/length(x)})
tapply(onlyhf$TPeta4, list(onlyhf$N, onlyhf$levels, onlyhf$stdev, onlyhf$correl), 
       function (x) {sum(x)/length(x)})

mean(master$TPeta1);mean(master$TPeta2);mean(master$TPeta3);mean(master$TPeta4)
mean(onlygg$TPeta3);mean(onlyhf$TPeta4)

##TPpes
tapply(master$TPpes1, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPpes2, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPpes3, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})
tapply(master$TPpes4, list(master$N, master$levels, master$stdev, master$correl), function (x) {sum(x)/1000})

tapply(onlygg$TPpes3, list(onlygg$N, onlygg$levels, onlygg$stdev, onlygg$correl), 
       function (x) {sum(x)/length(x)})
tapply(onlyhf$TPpes4, list(onlyhf$N, onlyhf$levels, onlyhf$stdev, onlyhf$correl), 
       function (x) {sum(x)/length(x)})

mean(master$TPpes1);mean(master$TPpes2);mean(master$TPpes3);mean(master$TPpes4)
mean(onlygg$TPpes3);mean(onlyhf$TPpes4)


####does it match the p value?####
master$sig = master$p.value < .05
master$sigCIeta = master$etaLL > 0
master$sigCIpes = master$pesLL > 0
master$sigCIges = master$gesLL > 0
master$sigCIetaF = master$etaLLF > 0
master$sigCIpesF = master$pesLLF > 0
master$sigCIgesF = master$gesLLF > 0

table(master$sig, master$sigCIeta) / nrow(master) 
table(master$sig, master$sigCIetaF) / nrow(master) 
table(master$sig, master$sigCIpes) / nrow(master) 
table(master$sig, master$sigCIpesF) / nrow(master) 
table(master$sig, master$sigCIges) / nrow(master) 
table(master$sig, master$sigCIgesF) / nrow(master) 

####summarize the simulation rounds####
mean_data = aggregate(master, by=list(master$N, master$levels, master$stdev, master$correl),
                      FUN = mean, na.rm = T, simplify = TRUE)

mean_data$sim.gesLL = aggregate(master$RM1.ges, list(master$N, master$levels, master$stdev, master$correl),
                   function (x) quantile(x, .025))$x
mean_data$sim.gesUL = aggregate(master$RM1.ges, list(master$N, master$levels, master$stdev, master$correl),
                                function (x) quantile(x, .975))$x
mean_data$sim.etaLL = aggregate(master$eta_full, list(master$N, master$levels, master$stdev, master$correl),
                                function (x) quantile(x, .025))$x
mean_data$sim.etaUL = aggregate(master$eta_full, list(master$N, master$levels, master$stdev, master$correl),
                                function (x) quantile(x, .975))$x
mean_data$sim.pesLL = aggregate(master$eta_part, list(master$N, master$levels, master$stdev, master$correl),
                                function (x) quantile(x, .025))$x
mean_data$sim.pesUL = aggregate(master$eta_part, list(master$N, master$levels, master$stdev, master$correl),
                                function (x) quantile(x, .975))$x

##subtract the sim CI from the avg CIs
diff_cor = mean_data[ , c("gesLL","gesUL","etaLL","etaUL","pesLL","pesUL")] - 
  mean_data[ , c("sim.gesLL","sim.gesUL","sim.etaLL","sim.etaUL","sim.pesLL","sim.pesUL")]
diff_regular = mean_data[ , c("gesLLF","gesULF","etaLLF","etaULF","pesLLF","pesULF")] - 
  mean_data[ , c("sim.gesLL","sim.gesUL","sim.etaLL","sim.etaUL","sim.pesLL","sim.pesUL")]

diff_regular = cbind(diff_regular, mean_data[ , 5:8])
diff_cor = cbind(diff_cor, mean_data[ , 5:8])

summary(diff_regular)
summary(diff_cor)

##you want the abs value to be smaller - closer to zero difference
##we expect the corrected one to be closer to zero
##so F here means the regular is closer to zero while T means corrected is closer to zero
smaller = as.data.frame(abs(diff_regular[ , 1:6]) > abs(diff_cor[ , 1:6]))
tapply(smaller$gesLL, list(diff_cor$N, diff_cor$levels, diff_cor$stdev, diff_cor$correl), sum)
tapply(smaller$gesUL, list(diff_cor$N, diff_cor$levels, diff_cor$stdev, diff_cor$correl), sum)
tapply(smaller$etaLL, list(diff_cor$N, diff_cor$levels, diff_cor$stdev, diff_cor$correl), sum)
tapply(smaller$etaUL, list(diff_cor$N, diff_cor$levels, diff_cor$stdev, diff_cor$correl), sum)
tapply(smaller$pesLL, list(diff_cor$N, diff_cor$levels, diff_cor$stdev, diff_cor$correl), sum)
tapply(smaller$pesUL, list(diff_cor$N, diff_cor$levels, diff_cor$stdev, diff_cor$correl), sum)

##tried to graph but it's weird
colnames(diff_cor) = colnames(diff_regular)
graphdata = rbind(diff_regular, diff_cor)
graphdata$type = c(rep("regular", nrow(diff_regular)),
                   rep("corrected", nrow(diff_cor)))
library(reshape)
longgraph = melt(graphdata, 
                 id = c("N", "levels", "stdev", "correl", "type"))
longgraph$limit = c(rep("lower", nrow(graphdata)), rep("upper", nrow(graphdata)))

library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 13))


ggplot(longgraph[longgraph$stdev=="5" , ], aes(variable, value, color = type)) + geom_point()
