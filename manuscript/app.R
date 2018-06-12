library(shiny)
library(ggplot2)
library(reshape)
library(plotly)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 13))

####load data####


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
                           c("One-Way Repeated Measures" = "owrm",
                             "One-Way Between Subjects" = "owbs",
                             "Two-Way Repeated Measures" = "twrm",
                             "Two-Way Between Subjects" = "twbs"),
                           selected = "owrm"),
               
               selectInput("effect_type", "Type of Effect Size:",
                           c("Full Eta-Squared" = "fes",
                             "Partial Eta-Squared" = "pes",
                             "Full Omega-Squared" = "fos",
                             "Partial Omega-Squared" = "pos",
                             "Generalized Eta-Squared" = "ges"),
                           selected = "fes"),
               
               selectInput("effect_size", "Effect Size:",
                           c("Small" = "small",
                             "Medium" = "medium",
                             "Large" = "large"),
                           selected = "small"),
               
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
    output$distributions <- renderPlot({ })
    
    output$variance <- renderPlot({ })
    
    output$confidence <- renderPlot({ })
  
}

# Run application
shinyApp(ui = ui, server = server)
