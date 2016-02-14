library(shiny)
library(stringi)
library(ggplot2)
#library(survMisc)

#setwd('~/downloads/shiny/')
load("data/dane.Rda")
load("data/razem.Rda")
load("data/bialka.Rda")
shinyUI(fluidPage(
  
   titlePanel("Protein expression (RTCGA.RPPA)"),
   sidebarLayout(
      sidebarPanel(
         selectInput("bialka",
                     "Choice one protein: ",
                     levels(bialka)),
         selectInput("rak", label = "Choice cancer types:",
                     unique(dane$cancer),
                     choices = c( "ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", 
                                  "COADREAD", "DLBC", "ESCA", "GBM","GBMLGG", "HNSC", 
                                  "KICH", "KIPAN", "KIRC","KIRP", "LGG", "LUAD", "LUSC", 
                                  "MESO", "OV", "PAAD","PRAD" ,"PCPG", "READ", "SARC", "SKCM",
                                  "STAD", "STES", "TGCT", "THCA", "THYM", "UCEC", 
                                  "UCS", "UVM"  
                     ), 
                     selected = c("COADREAD","GBMLGG","KIPAN","KIRC", "STES"), 
                     multiple = TRUE, selectize = TRUE)
      ),
      
      mainPanel(
         p(""),
         br(),
         tabsetPanel(tabPanel("Instructions", 
                              h2("Welcome in our application."),
                              br(),
                              "Our application is a tool, developed to perform the basic data analysis on RPPA data (Reverse phase protein array,
                              more information:" ,a("RPPA", href='https://en.wikipedia.org/wiki/Reverse_phase_protein_lysate_microarray'),")
                              and to visualize the results biological information. It will help you with the evaluation of standard RPPA experiments.
                              In our application we have got a division for four part: Cancer types, Kaplan-Meier estimator, Boxplot and Densities.", 
                              br(),
                              "For any further information see documentation:" ,
                              a("KM", href='https://cran.r-project.org/web/views/Survival.html'),",",
                              a("Boxplot", href= "https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/boxplot.html"), ",",
                              a("Densities", href='https://stat.ethz.ch/R-manual/R-devel/library/stats/html/density.html'),".",
                              br(),
                              br(),
                              "The first thing you need to do is to choose protein type in the upper right corner.",
                              br(),
                              "Next you have to choose at least one cancer type of 35 available." ,
                              br(),
                              br(),
                              "The list of cancer types, which can be found in this application is available in the first tab.
                              There are abbreviation of cancer type, the full name of cancer and count of observation for the cancer type. ",
                              br(),
                              br(),
                              "In the second tab are graphs of survival curves. 
                              The graph shows two curves which values are larger/smaller than the median. 
                              At the bottom of the tab is calculated p-value of certain types of cancer.",
                              br(),
                              HTML('<font size="1"> (14 February 2016)</font>'),
                              br(),
                              HTML('<font size="1">Authors: Agata Starzyk, Marta Czerwinska</font>'),
                              br()),
                     tabPanel(
                       # Use imageOutput to place the image on the page
                       "Cancer Types", dataTableOutput("mytable2")),
                     tabPanel("KM",
                       p("Kaplan-Meier survival curves for a given protein and a given cancer type. If you choose more than 
                          5 types of cancer you have to wait a moment for loading data!"),uiOutput("KM"),
                       p("P-value for indicated protein and a choosen types of cancer."),
                       tableOutput("pval")),
                     tabPanel("Box plot",
                       p("Box plots show the distribution of choosen types of cancer and a type of protein."),
                       plotOutput("bialka_box_plot",height =600)),
                     tabPanel("Densities",mainPanel(
                       p("Densities for a given types of cancer and a type of protein.")),plotOutput("density",height = 600))
                    
                     )
            
      ))
   )
)