library(shiny)
library(ggplot2)
#library(reshape)
#library(plotly)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 13))

####load data####
master = read.csv("fulldata.csv")
fesdist = read.csv("fes_distribution.csv")
fosdist = read.csv("fos_distribution.csv")
gesdist = read.csv("ges_distribution.csv")
pesdist = read.csv("pes_distribution.csv")
posdist = read.csv("pos_distribution.csv")

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

fulldata$popeta = NA
fulldata$popeta[ fulldata$levels == 3 & fulldata$stdev == 5] = 0.03225806
fulldata$popeta[ fulldata$levels == 3 & fulldata$stdev == 3] = 0.05263158
fulldata$popeta[ fulldata$levels == 3 & fulldata$stdev == 1] = 0.14285714
fulldata$popeta[ fulldata$levels == 4 & fulldata$stdev == 5] = 0.05882353
fulldata$popeta[ fulldata$levels == 4 & fulldata$stdev == 3] = 0.09433962
fulldata$popeta[ fulldata$levels == 4 & fulldata$stdev == 1] = 0.23809524
fulldata$popeta[ fulldata$levels == 5 & fulldata$stdev == 5] = 0.09090909
fulldata$popeta[ fulldata$levels == 5 & fulldata$stdev == 3] = 0.14285714
fulldata$popeta[ fulldata$levels == 5 & fulldata$stdev == 1] = 0.33333333
fulldata$popeta[ fulldata$levels == 6 & fulldata$stdev == 5] = 0.12727270
fulldata$popeta[ fulldata$levels == 6 & fulldata$stdev == 3] = 0.19553070
fulldata$popeta[ fulldata$levels == 6 & fulldata$stdev == 1] = 0.42168670

fulldata$ncp = fulldata$popeta / ((1-fulldata$popeta)*(fulldata$N * fulldata$levels))

####user interface####
ui <- fluidPage(

     titlePanel("Eta Simulations"),
     
     sidebarLayout(
        
        ####input area####
        sidebarPanel(
               
               ##drop-down menus
               selectInput("sample_size", "Sample Size:",
                           c(20, 26, 32, 38, 44, 50, 56, 62, 
                             68, 74, 80, 86, 92, 98, 104, 110),
                           selected = 20),
               
               selectInput("design", "Type of Design:",
                           c("One-Way Repeated Measures" = "RM1",
                             "One-Way Between Subjects" = "BN1",
                             "Two-Way Repeated Measures" = "RM2",
                             "Two-Way Between Subjects" = "BN2"),
                           selected = "owrm"),
               
               selectInput("effect_type", "Type of Effect Size:",
                           c("Full Eta-Squared" = "fes",
                             "Partial Eta-Squared" = "pes",
                             "Full Omega-Squared" = "fos",
                             "Partial Omega-Squared" = "pos",
                             "Generalized Eta-Squared" = "ges"),
                           selected = "fes"),
               
               selectInput("effect_size", "Effect Size:",
                           c("Small" = 5,
                             "Medium" = 3,
                             "Large" = 1),
                           selected = 5),
               
               selectInput("correlation", "Correlation:",
                           c(0, 0.1, 0.3, 0.5, 0.7, 0.9),
                           selected = 0),
               
               selectInput("levels", "Number of Levels:",
                           c(3, 4, 5, 6),
                           selected = 3)

        ), ## close sidebarPanel
        
        ####output area####
        mainPanel( 
               tabsetPanel(
                 tabPanel("Distributions", plotOutput("distributions"),
                          br(),
                          helpText("Complete dataset avaliable at: ___")),
                 tabPanel("Sampling Variance", plotOutput("variance"),
                          br(),
                          helpText("Complete dataset avaliable at: ___")),
                 tabPanel("Confidence Intervals", plotOutput("confidence"),
                          br(),
                          helpText("Complete dataset avaliable at: ___"))
               ) ## close tabset panel
               
        ) ## close mainPanel
      ) ##close sidebarLayout
) ##close fluidPage


####server functions####
server <- function(input,output) {
    output$distributions <- renderPlot({ 
      
      ##subset out the combinations they picked
      graphdata = subset(fulldata, 
                         N == input$sample_size & 
                         stdev == input$effect_size & 
                         correl == input$correlation &
                         levels == input$levels)
      
      #figure out which column to use
      columntopull = paste(input$design, ".", input$effect_type, sep = "")
      #deal with the fact that these don't exist
      if (columntopull == "RM1.pos") {columntopull = "RM1.fos"; input$effect_type = "fos"}
      if (columntopull == "BN1.pos") {columntopull = "BN1.fos"; input$effect_type = "fos"}
        
      #figure out the distribution estimation information from runs
      if (input$effect_type == "fes") {
        
        normalmean = fesdist$norm.1.mean[fesdist$levels == input$levels & fesdist$correl == input$correlation & fesdist$stdev == input$effect_size & fesdist$N == input$sample_size]
        normalsd = fesdist$norm.1.sd[fesdist$levels == input$levels & fesdist$correl == input$correlation & fesdist$stdev == input$effect_size & fesdist$N == input$sample_size]
        beta1 = fesdist$beta.1.shape1[fesdist$levels == input$levels & fesdist$correl == input$correlation & fesdist$stdev == input$effect_size & fesdist$N == input$sample_size]
        beta2 = fesdist$beta.1.shape2[fesdist$levels == input$levels & fesdist$correl == input$correlation & fesdist$stdev == input$effect_size & fesdist$N == input$sample_size]
        gshape = fesdist$gamma.1.shape[fesdist$levels == input$levels & fesdist$correl == input$correlation & fesdist$stdev == input$effect_size & fesdist$N == input$sample_size]
        grate = fesdist$gamma.1.rate[fesdist$levels == input$levels & fesdist$correl == input$correlation & fesdist$stdev == input$effect_size & fesdist$N == input$sample_size]
        
      }
      
      if (input$effect_type == "pes") {
        
        normalmean = pesdist$norm.1.mean[pesdist$levels == input$levels & pesdist$correl == input$correlation & pesdist$stdev == input$effect_size & pesdist$N == input$sample_size]
        normalsd = pesdist$norm.1.sd[pesdist$levels == input$levels & pesdist$correl == input$correlation & pesdist$stdev == input$effect_size & pesdist$N == input$sample_size]
        beta1 = pesdist$beta.1.shape1[pesdist$levels == input$levels & pesdist$correl == input$correlation & pesdist$stdev == input$effect_size & pesdist$N == input$sample_size]
        beta2 = pesdist$beta.1.shape2[pesdist$levels == input$levels & pesdist$correl == input$correlation & pesdist$stdev == input$effect_size & pesdist$N == input$sample_size]
        gshape = pesdist$gamma.1.shape[pesdist$levels == input$levels & pesdist$correl == input$correlation & pesdist$stdev == input$effect_size & pesdist$N == input$sample_size]
        grate = pesdist$gamma.1.rate[pesdist$levels == input$levels & pesdist$correl == input$correlation & pesdist$stdev == input$effect_size & pesdist$N == input$sample_size]
        
      }
      
      if (input$effect_type == "fos") {
        
        normalmean = fosdist$norm.1.mean[fosdist$levels == input$levels & fosdist$correl == input$correlation & fosdist$stdev == input$effect_size & fosdist$N == input$sample_size]
        normalsd = fosdist$norm.1.sd[fosdist$levels == input$levels & fosdist$correl == input$correlation & fosdist$stdev == input$effect_size & fosdist$N == input$sample_size]
        beta1 = fosdist$beta.1.shape1[fosdist$levels == input$levels & fosdist$correl == input$correlation & fosdist$stdev == input$effect_size & fosdist$N == input$sample_size]
        beta2 = fosdist$beta.1.shape2[fosdist$levels == input$levels & fosdist$correl == input$correlation & fosdist$stdev == input$effect_size & fosdist$N == input$sample_size]
        gshape = fosdist$gamma.1.shape[fosdist$levels == input$levels & fosdist$correl == input$correlation & fosdist$stdev == input$effect_size & fosdist$N == input$sample_size]
        grate = fosdist$gamma.1.rate[fosdist$levels == input$levels & fosdist$correl == input$correlation & fosdist$stdev == input$effect_size & fosdist$N == input$sample_size]
        
      }
      
      if (input$effect_type == "pos") {
       
        normalmean = posdist$norm.1.mean[posdist$levels == input$levels & posdist$correl == input$correlation & posdist$stdev == input$effect_size & posdist$N == input$sample_size]
        normalsd = posdist$norm.1.sd[posdist$levels == input$levels & posdist$correl == input$correlation & posdist$stdev == input$effect_size & posdist$N == input$sample_size]
        beta1 = posdist$beta.1.shape1[posdist$levels == input$levels & posdist$correl == input$correlation & posdist$stdev == input$effect_size & posdist$N == input$sample_size]
        beta2 = posdist$beta.1.shape2[posdist$levels == input$levels & posdist$correl == input$correlation & posdist$stdev == input$effect_size & posdist$N == input$sample_size]
        gshape = posdist$gamma.1.shape[posdist$levels == input$levels & posdist$correl == input$correlation & posdist$stdev == input$effect_size & posdist$N == input$sample_size]
        grate = posdist$gamma.1.rate[posdist$levels == input$levels & posdist$correl == input$correlation & posdist$stdev == input$effect_size & posdist$N == input$sample_size]
        
      }
      
      if (input$effect_type == "ges") {
        
        normalmean = gesdist$norm.1.mean[gesdist$levels == input$levels & gesdist$correl == input$correlation & gesdist$stdev == input$effect_size & gesdist$N == input$sample_size]
        normalsd = gesdist$norm.1.sd[gesdist$levels == input$levels & gesdist$correl == input$correlation & gesdist$stdev == input$effect_size & gesdist$N == input$sample_size]
        beta1 = gesdist$beta.1.shape1[gesdist$levels == input$levels & gesdist$correl == input$correlation & gesdist$stdev == input$effect_size & gesdist$N == input$sample_size]
        beta2 = gesdist$beta.1.shape2[gesdist$levels == input$levels & gesdist$correl == input$correlation & gesdist$stdev == input$effect_size & gesdist$N == input$sample_size]
        gshape = gesdist$gamma.1.shape[gesdist$levels == input$levels & gesdist$correl == input$correlation & gesdist$stdev == input$effect_size & gesdist$N == input$sample_size]
        grate = gesdist$gamma.1.rate[gesdist$levels == input$levels & gesdist$correl == input$correlation & gesdist$stdev == input$effect_size & gesdist$N == input$sample_size]
        
      }
      
      binsize = .005
      samplesize = 1000
      bin = NA
      
      CP = seq(min(graphdata[ , columntopull]), max(graphdata[ , columntopull]), .005)
      
      ##calculate the frequency of values you would expect from beta
      betaraw = pbeta(CP, shape1 = beta1, shape2 = beta2)
      
      ##calculate the frequency of values you would expect from normal
      normalraw = pnorm(CP, mean = normalmean, sd = normalsd)
      
      ##calculate the frequency of values you would expect from gamma
      gammaraw = pgamma(CP, shape = gshape, rate = grate)
      
      for (i in 1:length(CP)){
        bin[i] = sum(as.numeric(graphdata[ , columntopull] < CP[i]))
      }
      
      last = length(CP)
      freqb = (c(betaraw[-1],0) - betaraw)[-last] * samplesize
      freqnorm = (c(normalraw[-1],0) - normalraw)[-last] * samplesize
      freqgamma = (c(gammaraw[-1],0) - gammaraw)[-last] * samplesize
      freqreal = (c(bin[-1],0) - bin)[-last]
      
      CP1 = CP[-1]
      freqdata = cbind.data.frame("freqb1" = freqb, CP1, "freqnorm1" = freqnorm, 
                                  "freqgamma1" = freqgamma, "freqreal1" = freqreal)   
      
      ggplot(freqdata, aes(CP1, freqreal)) +
        geom_line(data = freqdata, aes(x = CP1, y = freqreal1), color = "red", linetype = "solid") +
        geom_line(data = freqdata, aes(x = CP1, y = freqb1), linetype = "dashed") + 
        geom_line(data = freqdata, aes(x = CP1, y = freqnorm1), linetype = "dotdash") +
        geom_line(data = freqdata, aes(x = CP1, y = freqgamma1), linetype = "longdash") +  
        cleanup +
        xlab("Effect Size") + 
        ylab("Frequency") +
        annotate("text", x = max(CP)-.1, y = 60, label = "– – Gamma", hjust = 0) +
        annotate("text", x = max(CP)-.1, y = 56, label = "-- Beta", hjust = 0) +
        annotate("text", x = max(CP)-.1, y = 52, label = "-. Normal", hjust = 0) +
        annotate("text", x = max(CP)-.1, y = 44, label = "Observed", hjust = 0, color = "red") + 
        coord_cartesian(xlim = c(min(CP), max(CP)))
      
      })
    
    output$variance <- renderPlot({ })
    
    output$confidence <- renderPlot({ })
  
}

# Run application
shinyApp(ui = ui, server = server)
