
##load data
#setwd("C:/Users/jel7c5/Desktop")
fulldata = read.csv("fulldata.csv")
names(fulldata)
RM2data = fulldata[,-c(8:15,28:56)]
library(metafor)
z2e = function(z){
  ((exp(z/0.5)-1)/(exp(z/0.5)+1))^2
}



#get unique combination of variables
popvardat = unique(RM2data[ ,c('N','levels','stdev','correl')])
popvardat$popvar = NA
popvardat$popeta = NA
popvardat$partno = 1:nrow(popvardat)
#plug in population variance
for(i in 1:nrow(popvardat)){
  nx = popvardat$N[i]
  levx = popvardat$levels[i]
  stdevx = popvardat$stdev[i]
  corx = popvardat$correl[i]
  
  tempdat = subset(RM2data, N==nx &
                     levels==levx &
                     stdev==stdevx &
                     correl==corx) 
 
  popvardat$popvar[i] = sum((tempdat$RM2.ges-mean(tempdat$RM2.ges))^2)/nrow(tempdat)
  popvardat$popeta[i] = mean(tempdat$RM2.ges)
}



################################################## begin loop

Esize = c(5,3,1)
Nsize = c(25,50,100)
d_ata = expand.grid(Nsize,Esize)
colnames(d_ata) = c("N","stdev")
d_ata$sim_var_bias = NA
d_ata$z_var_bias = NA
d_ata$sim_meta_bias = NA
d_ata$z_meta_bias = NA
d_ata$n_meta_bias = NA
osim = 0 



ptm <- proc.time() #start clock

for(a in 1:length(Esize)){
  
  for(b in 1:length(Nsize)){
    
    osim = osim+1
    data1k = data.frame(N = 1:1000, simVarBias = NA,
                        zVarBias = NA, simVarMetaBias = NA,
                        zVarMetaBias = NA, nMetaBias = NA)
    #repeat 1k times for specific condition
    for(d1k in 1:1000){
      subdat = subset(RM2data, stdev == Esize[a])
      popetax = mean(subdat$RM2.ges)
      select = 1:nrow(subdat)
      pick = sample(select, Nsize[b], replace = FALSE, prob = NULL)
      studydat = subdat[pick,]
      
      #### pop var
      studydat$popvar = NA
      for (i in 1:nrow(studydat)){
        nx = studydat$N[i]
        levx = studydat$levels[i]
        stdevx = studydat$stdev[i]
        corx = studydat$correl[i]
        
        tempdat = subset(popvardat, N==nx &
                           levels==levx &
                           stdev==stdevx &
                           correl==corx)
        studydat$popvar[i] = tempdat$popvar[1]
      }
      
      #### calculate simulation variance
      studydat$simVar = NA
      for (i in 1:nrow(studydat)){
        nx = studydat$N[i]
        levx = studydat$levels[i]
        stdevx = studydat$stdev[i]
        corx = studydat$correl[i]
        
        tempdat = subset(RM2data, N==nx &
                           levels==levx &
                           stdev==stdevx &
                           correl==corx)
        studydat$simVar[i] = var(tempdat$RM2.ges)
      }
      
      simVarMeta = rma.uni(yi = RM2.ges, vi = simVar, data = studydat, method = "PM")
      simVarMeta$beta
      
      #### calculate z variance
      studydat$FishZ = NA
      studydat$zVar = NA
      for (i in 1:nrow(studydat)){
        ges = studydat$RM2.ges[i]
        studydat$FishZ[i] = 0.5*log((1+sqrt(ges))/(1-sqrt(ges)))
        nx = studydat$N[i]
        studydat$zVar[i] = 1/(nx-3)
      }
      zVarMeta = rma.uni(yi = FishZ, vi = zVar, data = studydat, method="PM")
      z2e(zVarMeta$beta)
      
      ####regular n weight meta
      nweightMeta = rma.uni(yi = RM2.ges, vi = simVar, weights = N, data = studydat, method = "PM")
      nweightMeta$beta
      
      
      ####calculate variance bias and meta bias
      studydat$simVarBias = studydat$simVar - studydat$popvar
      studydat$zVarBias = z2e(studydat$zVar) - studydat$popvar
      
      data1k$simVarBias[d1k] = mean(studydat$simVarBias)
      data1k$zVarBias[d1k] = mean(studydat$zVarBias)
      data1k$simVarMetaBias[d1k] = simVarMeta$beta - popetax
      data1k$zVarMetaBias[d1k] = z2e(zVarMeta$beta) - popetax
      data1k$nMetaBias[d1k] = nweightMeta$beta - popetax
    }
    
    
    d_ata$sim_var_bias[osim] = mean(data1k$simVarBias, na.rm = T)
    d_ata$z_var_bias[osim] = mean(data1k$zVarBias, na.rm = T)
    d_ata$sim_meta_bias[osim] = mean(data1k$simVarMetaBias, na.rm = T)
    d_ata$z_meta_bias[osim] = mean(data1k$zVarMetaBias, na.rm = T)
    d_ata$n_meta_bias[osim] = mean(data1k$nMetaBias, na.rm = T)
    
    print(b)
  }
  
  print(a)
}

proc.time() - ptm #stop clock










