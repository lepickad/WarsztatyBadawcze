library(shiny)
load("~/apka.RData")
# Define UI for random distribution application 
fluidPage(
  
  # Application title
  titlePanel("ZADANIE 3"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      selectInput('sex', 'SEX', unique(levels(ramka_1_w_s$ST004D01T))),
     
      selectInput('subject', 'SUBJECT', unique(levels(ramka_1_w_s$item_short))),
      selectInput('result', 'RESULT', unique(levels(ramka_1_w_s$result))),
      numericInput('n', 'Cluster count', 3,
                   min = 1, max = 9)
    ),
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Table", tableOutput("table"))
      )
    )
  )
)





