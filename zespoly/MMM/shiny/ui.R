library(shiny)
library(stringi)
library(ggplot2)
library(survMisc)

#setwd('~/downloads/shiny/')
load("data/mRNA.data.rda")
load("data/markers.rda")

shinyUI(fluidPage(
  titlePanel("Gene expression (RTCGA.mRNA)"),
  sidebarLayout(
    sidebarPanel(
      br(),
      selectInput("marker",
                  "Select marker",
                  levels(markers)),
      br(),
      selectizeInput("cancer",
                     "Select type of cancer (max 4)",
                     unique(mRNA.data$cancer),
                     multiple = TRUE,
                     selected = c('BRCA', 'OV', 'COAD', 
                                  'COADREAD'),
                     options = list(maxItems = 4)),
      br(),
      sliderInput("time",
                  "Select survival time",
                  min = 24, max = 120, step = 6, value = 60),
      br(),
      h2("Welcome!"),
      p("This tool will allow you to analyze more than 100 genetic markers in terms of their significance as prognostic factor in cancer treatment."),
p(tags$b("Please, follow the below steps:")),
      tags$ol(
        tags$li("Select the marker you want to examine in the first place."), 
        tags$li("Select types of cancer (maximum number of chosen values is 4)"), 
        tags$li("Select the time point (in months), which will limit the time interval in the plots."),
        tags$li("In the right part of the application page, the ", tags$b("survival curves") ,"(estimated with the Kaplan-Mayer method) for chosen diseases are presented. You may assess how the overall survival changes with time for that particular marker. Additionally, the p~values for logrank test are included."),
        tags$li("After analysing the significance of gene markers, go to the second tab ",tags$b("(violin plots).") ,"The violin plots picture the distribution of the marker values in patients for different types of cancer. The median and quantiles are also considered."),
        tags$li("The last tab contains the ", tags$b("densities plots.")," Again, you may examine the differences and distribution of the marker across several cancers.")
      ),
      #code('install.packages("shiny")'),
      br(),
      br(),
      br(),
      br()
    ),
    
    mainPanel(
      p(""),
      br(),
      tabsetPanel(
        tabPanel("KM",   
                 fluidRow(
                   column(12,
                          br(),
                          br(),
                          p("In this example, what's visible in the client isn't",
                            "what's interesting. The server is writing to a log",
                            "file each time the slider value changes.")
                   )
                 ),
                 uiOutput("KM")
        ),
        tabPanel("Box plot",plotOutput("marker_box_plot", height = 600)),
        tabPanel("Densities", plotOutput("density",
                                         height = 600))
        #tabPanel("Median", tableOutput("median"))
      )
    )
  )
))