library(shiny)

raki <- c("BRCA", "COAD", "GBM", "KIPAN", 'KIRC', 
          "LUAD", "LUSC", "UCEC", "LAML")

shinyUI(fluidPage(
  titlePanel("RTCGA methylation"),
  sidebarLayout(
    sidebarPanel(

      selectInput('cancers', '1. Choose from cancer types:', raki,
                  selected = 'BRCA',
                  multiple = TRUE),
      br(),
      selectInput('features', '2. Choose from common features:', c(), multiple = TRUE),
      br(),
      radioButtons('co', '3. Choose methylation threshold:',
                   c('median' = 'median',
                     'mean'='mean',
                     'manual'='manual')),
      conditionalPanel(condition = "input.co == 'manual'",
                       sliderInput('threshold', "Methylation threshold:", min=0, max=1,
                                   value=0.5)),
      br(),
      actionButton("ok", "\nGo!"),
      actionButton("clean", "Clean" )),
    
    mainPanel(
      tabsetPanel(
        tabPanel("All features intersect", dataTableOutput('featuresIntersect')),
        tabPanel("Features distribution", plotOutput('boxplots', width='100%')),
        tabPanel("Density plots", plotOutput('density', width='100%')),
        tabPanel("Survival curves", plotOutput('survival', width='100%'))
      )
#         tabPanel("Krzywe przeżycia", verbatimTextOutput("opis_wykresu"), plotOutput("wykres", width = 800, height = 600)),
      )
    )
  )
)
