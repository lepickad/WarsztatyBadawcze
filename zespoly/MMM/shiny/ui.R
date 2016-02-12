library(shiny)
library(stringi)
library(ggplot2)
library(survMisc)
library(plotly)

#setwd('~/downloads/shiny/')
load("data/mRNA.data.rda")
load("data/markers.rda")

cancers <- c("BRCA", "COAD", "COADREAD", "GBMLGG", "KIPAN", "KIRC","KIRP", "LGG", "LUAD",    
             "LUSC", "OV", "READ", "UCEC")  
names(cancers) <- c("BRCA - Breast invasive carcinoma", 
                    "COAD - Colon adenocarcinoma",
                    "COADREAD - Colorectal adenocarcinoma",
                    "GBMLGG - Glioblastoma multiforme",
                    "KIPAN",
                    "KIRC - Kidney renal clear cell carcinoma",
                    "KIRP - Kidney renal papillary cell carcinoma",
                    "LGG - Lower Grade Glioma",
                    "LUAD - Lung adenocarcinoma",
                    "LUSC - Lung squamous cell carcinoma",
                    "OV - Ovarian serous cystadenocarcinoma",
                    "READ - Rectum adenocarcinoma",
                    "UCEC - Uterine Corpus Endometrial Carcinoma")

shinyUI(fluidPage(
  tags$div(style="display: table; width: 60%", tags$div(style="display: table-row;", tags$div(style="width: 10%; display:table-cell", img(src = "logo.png", height = 90, width = 90)), tags$div(style="width: 95%; vertical-align: middle; display:table-cell",h1("Expression gene (RTCGA.mRNA)")))),
  h4("Please, consult that the main aim of this application is to present qualities and prognostic abilities for particular gene marker. Running the comparison of several
     markers was not in authors' intention."),
  sidebarLayout(
    sidebarPanel(
      br(),
      selectInput("marker",
                  "1. Select marker",
                  markers),
      br(),
      selectizeInput("cancer",
                     "2. Select type of cancer (max 4)",
                     cancers,
                     multiple = TRUE,
                     selected = c('BRCA', 'OV', 'COAD', 
                                  'COADREAD'),
                     options = list(maxItems = 4)),
      br(),
      conditionalPanel(
        condition = "input.tabs == 'survival'",
        sliderInput("time",
                    "3. Select survival time",
                    min = 12, max = 120, step = 6, value = 60)
      ),
      #img(src = "logortcg.png", height = 70, width = 70),
      HTML('<font size="2"><br><b>Details:</b></br> 
           The application was built with database from
           <a href = "https://github.com/RTCGA">RTCGA</a> and 
           click <a href = "https://github.com/pbiecek/WarsztatyBadawcze/blob/master/zespoly/MMM/report/Report_MMM.pdf">here</a>
           to see more details about this application.'),
      br(),
      HTML('<font size="2"><br><b>Authors:</b></br> 
           <a href="mailto:momotkoe@student.mini.pw.edu.pl">Emilia Momotko</a>, 
           <a href="mailto:spiewakm2@student.mini.pw.edu.pl">Martyna Śpiewak</a>, 
           <a href="mailto:wasniewskim@student.mini.pw.edu.pl">Mikołaj Waśniewski</a> <br> </font>'),
      br(),
      HTML('The application was created 
           during Scientific workshops at
           <a href = "http://www.mini.pw.edu.pl/tikiwiki/">Faculty of Mathematics and Information Science</a>,
           <a href = "https://www.pw.edu.pl/">Warsaw University of Technology.</a>')
      ),
    
    mainPanel(
      p(""),
      br(),
      tabsetPanel(id = "tabs", 
                  tabPanel("Instructions", value = "A", 
                           h2("Welcome!"),
                           p("This tool will allow you to analyze more than 1000 gene markers in terms of their significance as prognostic factor in cancer treatment."),
                           p(tags$b("Please, follow the below steps:")),
                           tags$ol(
                             tags$li("Select the marker you want to examine in the first place."), 
                             tags$li("Select types of cancer (maximum number of chosen values is 4)"), 
                             tags$li("In the next tab of the page (", tags$b("Kaplan Meier: Survival Curve"),") the additional slider appears. Select the time point (in months) and then contemplate the survival curves (estimated with the Kaplan-Mayer method) for chosen diseases presented on the right panel. You may assess how the overall survival changes with time for that particular marker.",
                                     tags$div(br()), tags$div("Please, familiarize yourself with the below example which also contains detailed description of one of the graphs.")),
                             br(),
                             h5("Example:"),
                             tags$div(style="display: table; width: 100%", tags$div(style="display: table-row;", tags$div(style="width: 45%; display:table-cell", img(src = "KM1.png", height = 300, width = 500)), tags$div(style="width: 60%; vertical-align: middle; display:table-cell","The examplary plot presents the survival curves for two groups of patients: those with the value of marker greater the median belong to the first group (",tags$b("higher"),")
                                                                                                                                                                                                                             and others were assigned to the second group (",tags$b("lower"),"). Here, we want to examine the ACSM3 marker and his impact on survival for patients with OV cancer. The X-axis shows the time (in months) and the Y-axis presents the probability of survival.
                                                                                                                                                                                                                             The additional vertical line marks the time selected in the prompt nr 3 (slider). The main objective of this plot is to show potential differences in survival times between two groups,
                                                                                                                                                                                                                             so the ",tags$b("p-value")," is also included (this value is coloured in red, so the chosen marker is significant prognostic factor for OV cancer). Moreover, the ", tags$b("odds ratio"), " between lower and higher group in selected point of time can also be the point of the interest (labeled as OR)."))),
                             br(),
                             br(),
                             tags$li("After analysing the significance of gene markers, go to the third tab ",tags$b("(Density & Box plot).") ),
                             br(),
                             h5("Example:"),
                             tags$div(style="display: table; width: 100%", tags$div(style="display: table-row;", tags$div(style="width: 45%; display:table-cell", img(src = "V1.png", height = 100, width = 500)), tags$div(style="width: 60%; vertical-align: middle; display:table-cell","The violin plots picture the distribution of the marker values in patients for different types of cancer. As you can see median and quantiles are also considered. All violin plots are organized horizontally,
                                                                                                                                                                                                                            with the same x-axis in order to ease the comparison of the behaviour of selected marker across all cancers"))),
                             br(),br(),
                             tags$li("The last tab contains the ", tags$b("table")," with the relevant data. By clicking the button ",  tags$b("Download"), " you can transfer this data into your disk and then perform another analysis. On the top of the page (just above the Download button), you will find the link which transfer you to the Wikipedia where information about chosen marker are included."),
                             br(),
                             br(),
                             br()
                             )),
                  tabPanel("Kaplan Meier: Survival Curve",  value = "survival",  
                           fluidRow(
                             column(12,
                                    br(),
                                    br(),
                                    h5("Survival curves presented below were estimated with Kaplan-Meyer method. Prognostic propertis of chosen marker can be examined across 4 types of cancer.")
                             )
                           ),
                           uiOutput("KM3")
                  ),
                  tabPanel("Density & Box plot", value = "density",
                           fluidRow(
                             column(12,
                                    br(),
                                    br(),
                                    h5("Violin plots will provide you with information about the distribution of marker values across specified cancers. Additional box plots enrich the density graphs by
                                       including details about the median and quantiles.")
                                    )
                             ),
                           plotOutput("marker_box_plot", height = 600)),
                  tabPanel("Data",fluidRow(
                    column(12,
                           h2("mRNA.data"),
                           h5("The table with source data is also available for downloading. Be aware that this set is limited to the marker and cancers chosen in the prompts.")
                    )
                  ),
                  fluidRow(value = "data22", column(12, htmlOutput("text"))
                           
                  ),
                  br(),
                  br(),
                  fluidRow(
                    column(2,
                           downloadButton('downloadData', 'Download')),
                    tags$style(type='text/css', "#downloadData { width:100%; margin-top: -10px;}"),
                    column(6, p("Click the button to download the data associated with expression gene.")),
                    br(),
                    br(),
                    br(),
                    br()
                  ),
                  # Create a new row for the table.
                  fluidRow(value = "data",
                           DT::dataTableOutput("table")
                  ),
                  br(),
                  
                  br(),
                  br()
                  ))
      #tabPanel("Median", tableOutput("median"))
                             )
                           )
      )
    )