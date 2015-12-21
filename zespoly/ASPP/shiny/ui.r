library(shiny)
library(ggplot2)
library(survMisc)
sciezka = '/E/szkola/methylation/'
setwd(sciezka)
markery = readLines('markery.txt' , warn = FALSE)
markery = markery[-c(1:3)]
shinyUI(fluidPage(
  titlePanel("Profil metylacji"),
  sidebarLayout(
    sidebarPanel(
      selectInput("marker",
                  label = "Wybierz marker",
                  choices = markery),
                  #slected = "cg27584469"),
      #zmienic na wielokrotny wybor:
      selectInput("rak",
                  label = "Wybierz nowotwór",
                  choices = (c('brca','coad', 'coadread', 'gbm', 'gbmlgg', 'kipan',
                         'kirc', 'kirp', 'laml','luad','lusc', 'read','ucec','stes','stad','ov')),
                  selected = "brca")
    ),
    
    mainPanel(
      p("Dane dotyczące danego markeru"),
      br(),
      tabsetPanel(
        tabPanel("Wykres", plotOutput("wykres", width = 500)),
        tabPanel("Podsumowanie", verbatimTextOutput("podsumowanie"))
      )
    )
  )
))