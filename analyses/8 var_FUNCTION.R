



##requires the following packages: data.table, mvtnorm, ez, reshape
##args: Design - BN1, BN2, RM1, RM2, MIX
##args: type - ges, pes, fes, fos, pos
##args: Levels - 3,4,5,6
##args: Size - small, medium, large
##args: N - if between subjects design, n per group. if within subjects design, total n. 

##values: Returns the sampling variance of effect size from simulated effect sizes based on N

library(data.table)
library(mvtnorm)
library(ez)
library(reshape)
SamplingVariance = function(Design, type, Levels, Size, N){
  if((N%%2)!=0){
    N = N-1
  }
  if(Design == "BN1" | Design == "BN2"){
    numsim = N*Levels
  } else if(Design == "RM1" | Design == "RM2" | Design == "MIX"){
    numsim = N
  }
  if(Design == "BN1" | Design == "BN2"){
    mydata = as.data.table(matrix(ncol=2, nrow=N*Levels))
  } else if(Design == "RM1" | Design == "RM2" | Design == "MIX"){
    mydata = as.data.table(matrix(ncol=2, nrow=N))
  }
  colnames(mydata)=c("dfr","ES")
  Sizex = 4.623546 + ((-9.882827)*Size)
  if(Sizex == 0){
    Sizex = 0.01
  } else if(Sizex < 0){
    Sizex = 0.01
  }
  round=0
  for (i in 1:numsim){
    round=round+1
    #### make datasets
    if(Levels == 3) {
      sigma = matrix(c(Sizex,0,0,
                       0,Sizex,0,
                       0,0,Sizex), nrow = 3, ncol = 3)
      Means = c(2,2.5,3)
    } else if(Levels == 4) {
      sigma = matrix(c(Sizex,0,0,0,
                       0,Sizex,0,0,
                       0,0,Sizex,0,
                       0,0,0,Sizex), nrow = 4, ncol = 4)
      Means = c(2,2.5,3,2.5)
    } else if(Levels == 5) {
      sigma = matrix(c(Sizex,0,0,0,0,
                       0,Sizex,0,0,0,
                       0,0,Sizex,0,0,
                       0,0,0,Sizex,0,
                       0,0,0,0,Sizex), nrow = 5, ncol = 5)
      Means = c(2,2.5,3,3.5,4)
    } else if(Levels == 6) {
      sigma = matrix(c(Sizex,0,0,0,0,0,
                       0,Sizex,0,0,0,0,
                       0,0,Sizex,0,0,0,
                       0,0,0,Sizex,0,0,
                       0,0,0,0,Sizex,0,
                       0,0,0,0,0,Sizex), nrow = 6, ncol = 6)
      Means = c(2,2.5,3,3.5,4,4.5)
    }
    
    dataset = as.data.table(rmvnorm(N, Means, sigma))
    dataset = round(dataset, digits = 0)
    dataset[ dataset < 1 ] = 1
    dataset[ dataset > 7 ] = 7
    dataset$partno = 1:nrow(dataset)
    dataset$partno = factor(dataset$partno)
    long = melt(dataset,
                id = "partno",
                measured = colnames(long[1:(ncol(long)-1), ]))
    colnames(long)=c("partno","variable","value")
    #### end make datasets
    
    if(Design == "BN1"){
      #### BN1 stats
      long$bnpartno = as.factor(1:nrow(long))
      bnoutput = ezANOVA(data = long,
                         wid = bnpartno,
                         between = variable,
                         dv = value, 
                         type = 3, 
                         detailed = T)
      if(type == "ges"){
        mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
        mydata$ES[round] = bnoutput$ANOVA$ges[2]
      } else if(type == "pes"){
        mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
        mydata$ES[round] = bnoutput$ANOVA$SSn[2]/(bnoutput$ANOVA$SSn[2] + bnoutput$ANOVA$SSd[2])
      } else if(type == "fes"){
        mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
        mydata$ES[round] = bnoutput$ANOVA$SSn[2]/(bnoutput$ANOVA$SSn[2] + bnoutput$ANOVA$SSd[2])
      } else if(type == "fos"){
        mydata$dfr[round] = bnoutput$ANOVA$DFd[2]
        mydata$ES[round] = bnoutput$ANOVA$DFn[2]*((bnoutput$ANOVA$SSn[2]/bnoutput$ANOVA$DFn[2])-(bnoutput$ANOVA$SSd[2]/bnoutput$ANOVA$DFd[2]))/
          ((bnoutput$ANOVA$SSn[2]+bnoutput$ANOVA$SSd[2])+(bnoutput$ANOVA$SSd[2]/bnoutput$ANOVA$DFd[2]))
      }
      #### end BN1 stats
    } else if(Design == "BN2"){
      #### BN2 stats
      long$bnpartno = as.factor(1:nrow(long))
      long$level2 = gl(2, N/2, nrow(long), labels = c("level 1", "level 2"))
      bnoutput2 = ezANOVA(data = long,
                          wid = bnpartno,
                          between = .(variable, level2),
                          dv = value,
                          type = 3,
                          detailed = T)
      if(type == "ges"){
        mydata$dfr[round] = bnoutput2$ANOVA$DFd[2]
        mydata$ES[round] = bnoutput2$ANOVA$ges[2]
      } else if(type == "pes"){
        mydata$dfr[round] = bnoutput2$ANOVA$DFd[2]
        mydata$ES[round] = bnoutput2$ANOVA$SSn[2] / (bnoutput2$ANOVA$SSn[2] + bnoutput2$ANOVA$SSd[1])
      } else if(type == "fes"){
        mydata$dfr[round] = bnoutput2$ANOVA$DFd[2]
        mydata$ES[round] = bnoutput2$ANOVA$SSn[2] / (bnoutput2$ANOVA$SSn[2] + bnoutput2$ANOVA$SSd[1] + bnoutput2$ANOVA$SSn[3] + bnoutput2$ANOVA$SSn[4])
      } else if(type == "fos"){
        mydata$dfr[round] = bnoutput2$ANOVA$DFd[2]
        mydata$ES[round] = (bnoutput2$ANOVA$DFn[2]*((bnoutput2$ANOVA$SSn[2]/bnoutput2$ANOVA$DFn[2])-(bnoutput2$ANOVA$SSd[1]/bnoutput2$ANOVA$DFd[2])))/
          ((bnoutput2$ANOVA$SSn[2] + bnoutput2$ANOVA$SSd[1] + bnoutput2$ANOVA$SSn[3] + bnoutput2$ANOVA$SSn[4])+
             (bnoutput2$ANOVA$SSd[1]/bnoutput2$ANOVA$DFd[2]))
      } else if(type == "pos"){
        mydata$dfr[round] = bnoutput2$ANOVA$DFd[2]
        mydata$ES[round] = (bnoutput2$ANOVA$DFn[2]*((bnoutput2$ANOVA$SSn[2]/bnoutput2$ANOVA$DFn[2])-(bnoutput2$ANOVA$SSd[1]/bnoutput2$ANOVA$DFd[2])))/
          (bnoutput2$ANOVA$SSn[2]+(((N*Levels)-bnoutput2$ANOVA$DFn[2])*(bnoutput2$ANOVA$SSd[1]/bnoutput2$ANOVA$DFd[2])))
      }
      #### end BN2 stats
    } else if(Design == "RM1"){
      #### RM1 stats
      rmoutput = ezANOVA(data = long,
                         wid = partno,
                         within = variable,
                         dv = value,
                         type = 3, 
                         detailed = T)
      if(type == "ges"){
        mydata$dfr[round] = rmoutput$ANOVA$DFd[2]
        mydata$ES[round] = rmoutput$ANOVA$ges[2]
      } else if(type == "pes"){
        mydata$dfr[round] = rmoutput$ANOVA$DFd[2]
        mydata$ES[round] = rmoutput$ANOVA$SSn[2]/(rmoutput$ANOVA$SSn[2] + rmoutput$ANOVA$SSd[2])
      } else if(type == "fes"){
        mydata$dfr[round] = rmoutput$ANOVA$DFd[2]
        mydata$ES[round] = rmoutput$ANOVA$SSn[2]/(rmoutput$ANOVA$SSn[2] + rmoutput$ANOVA$SSd[2] + rmoutput$ANOVA$SSd[1])
      } else if(type == "fos"){
        mydata$dfr[round] = rmoutput$ANOVA$DFd[2]
        mydata$ES[round] = rmoutput$ANOVA$DFn[2]*((rmoutput$ANOVA$SSn[2]/rmoutput$ANOVA$DFn[2])-(rmoutput$ANOVA$SSd[2]/rmoutput$ANOVA$DFd[2]))/
          ((rmoutput$ANOVA$SSn[2]+rmoutput$ANOVA$SSd[2]+rmoutput$ANOVA$SSd[1])+(rmoutput$ANOVA$SSn[1]/(rmoutput$ANOVA$DFd[2]/rmoutput$ANOVA$DFn[2])))
      }
      #### end RM1 stats
    } else if(Design == "RM2"){
      #### RM2 stats
      tempstuff = long
      randomvalues = rnorm(N, mean(Means), 1)
      tempstuff$value = tempstuff$value + randomvalues
      tempstuff$value = round(tempstuff$value, digits = 0)
      tempstuff$value[  tempstuff$value < 1 ] = 1
      tempstuff$value[  tempstuff$value > 7 ] = 7
      doublermdata = rbind(long, tempstuff)
      level1 = as.numeric(gl(2, N/2, nrow(long), labels = c("1", "2")))
      level2 = 3 - as.numeric(gl(2, N/2, nrow(long), labels = c("2", "1")))
      doublermdata$rmlevel2 = as.factor(c(level1,level2))
      
      rmoutput2 = ezANOVA(data = doublermdata,
                          wid = partno,
                          within = .(variable, rmlevel2),
                          dv = value,
                          type = 3,
                          detailed = T)
      if(type == "ges"){
        mydata$dfr[round] = rmoutput2$ANOVA$DFd[2]
        mydata$ES[round] = rmoutput2$ANOVA$ges[2]
      } else if(type == "pes"){
        mydata$dfr[round] = rmoutput2$ANOVA$DFd[2]
        mydata$ES[round] = rmoutput2$ANOVA$SSn[2]/(rmoutput2$ANOVA$SSn[2] + rmoutput2$ANOVA$SSd[2])
      } else if(type == "fes"){
        mydata$dfr[round] = rmoutput2$ANOVA$DFd[2]
        mydata$ES[round] = rmoutput2$ANOVA$SSn[2]/
          (rmoutput2$ANOVA$SSn[2] + rmoutput2$ANOVA$SSd[2] + rmoutput2$ANOVA$SSn[3] + 
             rmoutput2$ANOVA$SSd[3] + rmoutput2$ANOVA$SSn[4] + rmoutput2$ANOVA$SSd[4] + rmoutput2$ANOVA$SSd[1])
      } else if(type == "fos"){
        mydata$dfr[round] = rmoutput2$ANOVA$DFd[2]
        mydata$ES[round] = rmoutput2$ANOVA$DFn[2]*((rmoutput2$ANOVA$SSn[2]/rmoutput2$ANOVA$DFn[2])-(rmoutput2$ANOVA$SSd[2]/rmoutput2$ANOVA$DFd[2]))/
          ((rmoutput2$ANOVA$SSn[2]+rmoutput2$ANOVA$SSd[2]+rmoutput2$ANOVA$SSn[3]+rmoutput2$ANOVA$SSd[3]+rmoutput2$ANOVA$SSn[4]+rmoutput2$ANOVA$SSd[4]+rmoutput2$ANOVA$SSd[1])+
             (rmoutput2$ANOVA$SSn[1]/(rmoutput2$ANOVA$DFd[2]/rmoutput2$ANOVA$DFn[2])))
      } else if(type == "pos"){
        mydata$dfr[round] = rmoutput2$ANOVA$DFd[2]
        mydata$ES[round] = (rmoutput2$ANOVA$DFn[2]*((rmoutput2$ANOVA$SSn[2]/rmoutput2$ANOVA$DFn[2])-(rmoutput2$ANOVA$SSd[2]/rmoutput2$ANOVA$DFd[2])))/
          (rmoutput2$ANOVA$SSn[2]+rmoutput2$ANOVA$SSd[2]+rmoutput2$ANOVA$SSn[1]+
             (rmoutput2$ANOVA$SSn[1]/(rmoutput2$ANOVA$DFd[2]/rmoutput2$ANOVA$DFn[2])))
      }
      #### end RM2 stats
    } else if(Design == "MIX"){
      ####  MIX stats
      long$level2 = gl(2, N/2, nrow(long), labels = c("level 1", "level 2"))
      mixedoutput = ezANOVA(data = long,
                            wid = partno,
                            within = variable,
                            between = level2,
                            dv = value,
                            type = 3,
                            detailed = T)
      if(type == "ges"){
        mydata$dfr[round] = mixedoutput$ANOVA$DFd[2]
        mydata$ES[round] = mixedoutput$ANOVA$ges[2]
      } else if(type == "pes"){
        mydata$dfr[round] = mixedoutput$ANOVA$DFd[2]
        mydata$ES[round] = mixedoutput$ANOVA$SSn[3] / (mixedoutput$ANOVA$SSn[3] + mixedoutput$ANOVA$SSd[3])
      } else if(type == "fes"){
        mydata$dfr[round] = mixedoutput$ANOVA$DFd[2]
        mydata$ES[round] = mixedoutput$ANOVA$SSn[3] / 
          (mixedoutput$ANOVA$SSn[3] + mixedoutput$ANOVA$SSd[3] + mixedoutput$ANOVA$SSn[2] + mixedoutput$ANOVA$SSn[4] +
             mixedoutput$ANOVA$SSd[1] + mixedoutput$ANOVA$SSd[2] + mixedoutput$ANOVA$SSd[4])
      } else if(type == "fos"){
        mydata$dfr[round] = mixedoutput$ANOVA$DFd[2]
        mydata$ES[round] = (mixedoutput$ANOVA$DFn[3]*((mixedoutput$ANOVA$SSn[3]/mixedoutput$ANOVA$DFn[3])-(mixedoutput$ANOVA$SSd[3]/mixedoutput$ANOVA$DFd[3])))/
          ((mixedoutput$ANOVA$SSn[3]+mixedoutput$ANOVA$SSd[3]+mixedoutput$ANOVA$SSn[2]+mixedoutput$ANOVA$SSd[2]+
              mixedoutput$ANOVA$SSn[4]+mixedoutput$ANOVA$SSd[4]+mixedoutput$ANOVA$SSd[1])+
             (mixedoutput$ANOVA$SSn[1]/(mixedoutput$ANOVA$DFd[3]/mixedoutput$ANOVA$DFn[3])))
      } else if(type == "pos"){
        mydata$dfr[round] = mixedoutput$ANOVA$DFd[2]
        mydata$ES[round] = (mixedoutput$ANOVA$DFn[3]*((mixedoutput$ANOVA$SSn[3]/mixedoutput$ANOVA$DFn[3])-(mixedoutput$ANOVA$SSd[3]/mixedoutput$ANOVA$DFd[3])))/
          (mixedoutput$ANOVA$SSn[3]+mixedoutput$ANOVA$SSd[3]+mixedoutput$ANOVA$SSn[1]+
             (mixedoutput$ANOVA$SSn[1]/(mixedoutput$ANOVA$DFd[3]/mixedoutput$ANOVA$DFn[3])))
      } #### end MIX stats
    } ##last if then
  } #end for loop
  if(Design == "BN1" | Design == "BN2"){
    var1 = sum((mydata$ES-mean(mydata$ES))^2)/((N*length(Means))-1)
    var2 = sum((mydata$ES-mean(mydata$ES))^2)/mydata$dfr[1]
  } else if(Design == "RM1" | Design == "RM2" | Design == "MIX"){
    var1 = sum((mydata$ES-mean(mydata$ES))^2)/(N-1)
    var2 = sum((mydata$ES-mean(mydata$ES))^2)/mydata$dfr[1]
  }
  if(var1<var2){
    varlo = var2
  } else{
    varlo = var1
  }
  return(varlo)
} ##end function




##BN1 works fine
SamplingVariance("BN1", "ges", 3, .3, 10)
SamplingVariance("BN1", "pes", 5, .3, 10)
SamplingVariance("BN1", "fes", 5, .3, 10)
SamplingVariance("BN1", "fos", 5, .3, 10)

##RM1 runs fine but sometimes gives you warning
SamplingVariance("RM1", "ges", 5, .3, 10)
SamplingVariance("RM1", "pes", 5, .3, 10)
SamplingVariance("RM1", "fes", 5, .3, 5)
SamplingVariance("RM1", "fos", 5, .3, 10)


##works fine
SamplingVariance("BN2", "ges", 5, .3, 9)
SamplingVariance("BN2", "pes", 5, .3, 10)
SamplingVariance("BN2", "fes", 5, .3, 10)
SamplingVariance("BN2", "fos", 5, .3, 10)
SamplingVariance("BN2", "pos", 5, .3, 10)


## sometimes gives you warning
SamplingVariance("RM2", "ges", 5, .07, 5) 
SamplingVariance("RM2", "pes", 5, .05, 5) 
SamplingVariance("RM2", "fes", 5, .04, 5) 
SamplingVariance("RM2", "fos", 5, .1, 5) 
SamplingVariance("RM2", "pos", 5, .02, 5)


##works fine just make sure N is greater than number of levels
SamplingVariance("MIX", "ges", 5, .3, 9)
SamplingVariance("MIX", "pes", 5, .3, 5)
SamplingVariance("MIX", "fes", 5, .3, 10)
SamplingVariance("MIX", "fos", 5, .3, 10)
SamplingVariance("MIX", "pos", 5, .3, 10)






























