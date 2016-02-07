library(shiny)
library(ggplot2)
library(survMisc)

cancer_types<-c("BRCA","COAD","COADREAD","GBMLGG","KIPAN","KIRC","KIRP","LGG",
                "LUAD","LUSC","OV","READ","UCEC")
cancer_names <- c("Breast invasive carcinoma", "Colon adenocarcinoma",
                  "Colon adenocarcinoma & Rectum adenocarcinoma",
                  "Glioblastoma multiforme & Brain Lower Grade Glioma",
                  "Kidney: Chromophobe, renal clear cell carcinoma, renal papillary cell carcinoma",
                  "Kidney renal clear cell carcinoma",
                  "Kidney renal papillary cell carcinoma","Brain Lower Grade Glioma",
                  "Lung adenocarcinoma","Lung squamous cell carcinoma",
                  "Ovarian serous cystadenocarcinoma","Rectum adenocarcinoma",
                  "Uterine Corpus Endometrial Carcinoma")
# cancer_types<-c("READ","UCEC")
# cancer_names <- c("Rectum adenocarcinoma",
#                   "Uterine Corpus Endometrial Carcinoma")


cancer_list <- eval(parse(text=paste0("list(",paste('"',cancer_names,'"','=',
                                                    '"',cancer_types,'"',
                                              sep="", collapse=", "),")")))


shinyUI(fluidPage(
   HTML('<div align="center">'),
   img(src="english_title.png"),
   HTML ('</div>'),
   # Application title
   headerPanel(title="", windowTitle="mRNA Browser"),
   
   conditionalPanel(condition="input.instructionButton>=input.appButton",
     h2("Welcome in mRNA Browser!"),
     br(),
     "It is an application that helps exploring the data from",
     a("RTCA", href='https://tcga-data.nci.nih.gov/tcga/'),
     " project.", br(), "All the data was analased using ", 
     a("RTCGA package", href= "https://github.com/RTCGA"),
     "dedicated to this project.", br(),
     "For any further information see documentation: ",
     br(),
     br(),
     HTML('<font size="1">Version: 3.2 (27 January 2016)</font>'),
     br(),
     HTML('<font size="1">Authors: Karolina Wyszyńska, Krzysztof Rudaś</font>'),
     br(),
     HTML('<div align="center">'),
     actionButton("appButton", "Go to Application!"),
     HTML('</div>')
   ),
   
   
   conditionalPanel(condition="input.instructionButton<input.appButton",
      HTML('<div align="right">'),
      actionButton("instructionButton", "Instruction"),
      HTML('</div>'),
      
      tabsetPanel(
          tabPanel("Histogram",value="Histogram",
            # Sidebar with a slider input for the number of bins
            sidebarLayout(
            sidebarPanel(
                selectInput("cancer",label  = h3(strong("Cancer type")),
                     choices  = cancer_list,selected = cancer_list[1]), 
                uiOutput("significant_genes")
            ),
      
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("histogram"),
                uiOutput("plot_help_hist")
            )
            )
          
          ),
          
          tabPanel("Survival Plot", value="SurvPlot",
                   # Sidebar with a slider input for the number of bins
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("cancerSurv",label  = h3(strong("Cancer type")),
                                   choices  = cancer_list,selected = cancer_list[1]), 
                       uiOutput("significant_genesSurv"),
                        uiOutput("plot_help_surv1")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("survplot"),
                        uiOutput("plot_help_surv2")
                     )
                   )
        ),
        
        
        
        tabPanel("Comparison - gene", value="compare",
                 fluidPage(
                   fluidRow(column(4),
                            column(8, 
                                   h3("Survival plots for choosen mRNA", align="center"))),
                   fluidRow(
                   column(4, class="well",
                     uiOutput("significant_genesCompGenes"),
                     h3(strong("Cancer type")),
                     fluidRow(
                       column(6,
                              HTML('<font size="1">'),
                              selectInput("cancerComp1",label  =NULL,
                                          choices  = cancer_list,selected = cancer_list[1]),
                       selectInput("cancerComp3",label  = NULL,
                                   choices  = cancer_list,selected = cancer_list[3])
                       ),
                       column(6,
                              selectInput("cancerComp2",label  = NULL,
                                          choices  = cancer_list,selected = cancer_list[2]),
                              selectInput("cancerComp4",label  =NULL,
                                          choices  = cancer_list,selected = cancer_list[4]),
                     HTML("</font>")))
                   ),
                    column(4, 
                        plotOutput("survplotComp1", width="100%"),

                        plotOutput("survplotComp3", width="100%")
                      
                    ),
                    column(4,
                    plotOutput("survplotComp2", width="100%"),
                
                    plotOutput("survplotComp4", width="100%")

                      )
                     )
                   )
                 
        ),
      id="typ", selected="Histogram"
      )
  )
))