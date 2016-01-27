library(shiny)
library(stringi)
library(ggplot2)
library(survMisc)
library(plotly)

#setwd('~/downloads/shiny/')
load("data/mRNA.data.rda")
load("data/markers.rda")

shinyUI(fluidPage(
  titlePanel("Gene Expression (RTCGA.mRNA)"),
  sidebarLayout(
    sidebarPanel(
      br(),
      selectInput("marker",
                  "1. Select marker",
                  levels(markers)),
      br(),
      selectizeInput("cancer",
                     "2. Select type of cancer (max 4)",
                     unique(mRNA.data$cancer),
                     multiple = TRUE,
                     selected = c('BRCA', 'OV', 'COAD', 
                                  'COADREAD'),
                     options = list(maxItems = 4)),
      br(),
      sliderInput("time",
                  "3. Select survival time",
                  min = 12, max = 120, step = 6, value = 60),
      br(),
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
        tabPanel("Instructions",
                 h2("Welcome!"),
                 p("This tool will allow you to analyze more than 100 gene markers in terms of their significance as prognostic factor in cancer treatment."),
                 p(tags$b("Please, follow the below steps:")),
                 tags$ol(
                   tags$li("Select the marker you want to examine in the first place."), 
                   tags$li("Select types of cancer (maximum number of chosen values is 4)"), 
                   tags$li("Select the time point (in months)."),
                   tags$li("In the next tab of the page (", tags$b("Kaplan Meier: Survival Curve"),") the survival curves (estimated with the Kaplan-Mayer method) for chosen diseases are presented. You may assess how the overall survival changes with time for that particular marker."),
                   br(),
                   p("Example:"),
                   tags$div( img(src = "KM1.png", height = 300, width = 450), tags$div("The examplary plot presents the survival curves for two groups of patients: those with the value of marker greater the median belong to the first group (higher)
                                                                                       and others were assigned to the second group (lower). The examined type of cancer is BRCA. The X-axis shows the time (in months) and the Y-axis presents the probability of survival.
                                                                                       The additional vertical line marks the time selected in the prompt nr 3. The main objective of this plot is to show potential differences in survival times between two groups,
                                                                                       so the p-value is also included. Moreover, the ratio of survival probabilities of higher and lower group in selected point of time can also be the point of the interest (labeled as OR).")),
                   br(),
                   br(),
                   tags$li("After analysing the significance of gene markers, go to the third tab ",tags$b("(violin plots).") ),
                   br(),
                   p("Example:"),
                   tags$div( img(src = "V1.png", height = 150, width = 600), tags$div("The violin plots picture the distribution of the marker values in patients for different types of cancer. As you can see median and quantiles are also considered. All violin plots are organized horizontally,
                                                                                      with the same x-axis in order to ease the comparison of the behave of selected marker across all cancers")),
                   br(),
                   tags$li("The last tab contains the ", tags$b("table")," with the relevant data. By clicking the button ",  tags$b("Download"), " you can transfer this data into your disk and then perform another analysis."),
                   br(),
                   br()
                   )),
        tabPanel("Kaplan Meier: Survival Curve",   
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
        tabPanel("Data", fluidPage(
          titlePanel("mRNA data")
        ),
        # Create a new row for the table.
        fluidRow(
          DT::dataTableOutput("table")
        ),
        br(),
        br(),
        br(),
        
        fluidRow(
          column(4,
                 downloadButton('downloadData', 'Download')),
          br(),
          br()
        )
        ))
      #tabPanel("Median", tableOutput("median"))
        )
      )
        )
  )