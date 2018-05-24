##set working directory
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Eta simulations/distribution estimation")

##open files
fes_data = read.csv("fes_distribution.csv")
fos_data = read.csv("fos_distribution.csv")
ges_data = read.csv("ges_distribution.csv")
aiccolumns1 = c("norm.1.aic", "exp.1.aic", "gamma.1.aic", "lnorm.1.aic", 
                "beta.1.aic", "chi.1.aic", "f.1.aic", "log.1.aic")
aiccolumns2 = c("norm.2.aic", "exp.2.aic", "gamma.2.aic", "lnorm.2.aic",
                "beta.2.aic", "chi.2.aic", "f.2.aic", "log.2.aic")


####fes information####
##calculate for first estimate
temp = fes_data[ , aiccolumns1]
table(colnames(temp)[apply(temp,1,which.min)])
fes_data$smallest1 = colnames(temp)[apply(temp,1,which.min)]

##calculate for the second estimate
temp = fes_data[ , aiccolumns2]
table(colnames(temp)[apply(temp,1,which.min)])
fes_data$smallest2 = colnames(temp)[apply(temp,1,which.min)]

with(fes_data, table(smallest1, N))
with(fes_data, table(smallest1, stdev))
with(fes_data, table(smallest1, correl))
with(fes_data, table(smallest1, levels))

with(fes_data, table(smallest2, N))
with(fes_data, table(smallest2, stdev))
with(fes_data, table(smallest2, correl))
with(fes_data, table(smallest2, levels))

table(fes_data$beta.1)
table(fes_data$beta.7)
mean(fes_data$beta.1.shape1)
mean(fes_data$beta.1.shape2)
sd(fes_data$beta.1.shape1)
sd(fes_data$beta.1.shape2)



####fos information####
##calculate for first estimate
temp = fos_data[ , aiccolumns1]
table(colnames(temp)[apply(temp,1,which.min)])
fos_data$smallest1 = colnames(temp)[apply(temp,1,which.min)]

##calculate for the second estimate
temp = fos_data[ , aiccolumns2]
table(colnames(temp)[apply(temp,1,which.min)])
fos_data$smallest2 = colnames(temp)[apply(temp,1,which.min)]

with(fos_data, table(smallest1, N))
with(fos_data, table(smallest1, stdev))
with(fos_data, table(smallest1, correl))
with(fos_data, table(smallest1, levels))

with(fos_data, table(smallest2, N))
with(fos_data, table(smallest2, stdev))
with(fos_data, table(smallest2, correl))
with(fos_data, table(smallest2, levels))

table(fos_data$beta.1)
table(fos_data$beta.7)
mean(fos_data$beta.1.shape1)
mean(fos_data$beta.1.shape2)
sd(fos_data$beta.1.shape1)
sd(fos_data$beta.1.shape2)

####ges information####
##calculate for first estimate
temp = ges_data[ , aiccolumns1]
table(colnames(temp)[apply(temp,1,which.min)])
ges_data$smallest1 = colnames(temp)[apply(temp,1,which.min)]

##calculate for the second estimate
temp = ges_data[ , aiccolumns2]
table(colnames(temp)[apply(temp,1,which.min)])
ges_data$smallest2 = colnames(temp)[apply(temp,1,which.min)]

with(ges_data, table(smallest1, N))
with(ges_data, table(smallest1, stdev))
with(ges_data, table(smallest1, correl))
with(ges_data, table(smallest1, levels))

with(ges_data, table(smallest2, N))
with(ges_data, table(smallest2, stdev))
with(ges_data, table(smallest2, correl))
with(ges_data, table(smallest2, levels))

table(ges_data$beta.1)
table(ges_data$beta.7)
mean(ges_data$beta.1.shape1)
mean(ges_data$beta.1.shape2)
sd(ges_data$beta.1.shape1)
sd(ges_data$beta.1.shape2)
