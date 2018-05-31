setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/eta-sims/rm correct")

master = read.csv("sim_rm_data_het.csv")
master = master[ , -c(1)]

####calculate eta and partial eta squared####
master$eta_full = master$RM1.ssm.main/(master$RM1.ssm.main + master$RM1.ssm.p + master$RM1.ssr.main + master$RM1.ssr.p)
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
  
  
  
}

####deal with the NAs in NCP, which MBESS lists as 0####
master$NCP.gesLL[ is.na(master$NCP.gesLL) ] = 0
master$NCP.gesUL[ is.na(master$NCP.gesUL) ] = 0
master$NCP.etaLL[ is.na(master$NCP.etaLL) ] = 0
master$NCP.etaUL[ is.na(master$NCP.etaUL) ] = 0
master$NCP.pesLL[ is.na(master$NCP.pesLL) ] = 0
master$NCP.pesUL[ is.na(master$NCP.pesUL) ] = 0

master$RM1.gg >.75 & master$RM1.hf > .75

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

diff_regular = mean_data[ , 24:29] - mean_data[ , 45:50]
diff_cor = mean_data[ , 39:44] - mean_data[ , 45:50]

diff_regular = cbind(diff_regular, mean_data[ , 5:8])
diff_cor = cbind(diff_cor, mean_data[ , 5:8])

summary(diff_regular)
summary(diff_cor)

##you want the abs value to be smaller - closer to zero difference
##so here we are seeing if the regular is larger than the corrected
##zero means F, so regular is smaller, one means T so corrected is better 
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
