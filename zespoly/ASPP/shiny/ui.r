# setwd("data")
# all_files <- list.files("data")
# for(i in seq_along(all_files)){
#    load(all_files[i])
# }
#setwd("data")
load("data/markery_ist.rda")

shinyUI(navbarPage("Methylation profile and survival analysis", 
                   tabPanel("Introduction", 
                            h1("Research workshops", align = "center"),
                            h2("Methylation profile", align = "center"),
                            h3("Adrianna Sudol & Pawel Pytlak", align = "center"),
                            br(),
                            p("Our application...")
                            ),
                   tabPanel("Distribution of methylation", 
                            sidebarLayout(
                               sidebarPanel(
                                  selectInput("marker", 
                                              label = "Choose a probe to analyse:",
                                              choices = markery_ist,
                                              selected = "cg00016968_RHOC"),
                                  
#                                   sliderInput("threshold", 
#                                               label = "Choose a threshold for methylation:",
#                                               min = 0, max = 1, value = 0.5),
                                  
                                  selectInput("cancer", label = "Choose cancer types:", 
                                              choices = c("brca", "coad", "coadread", "gbm", 
                                                          "gbmlgg", "kipan", "kirc", "kirp", 
                                                          "laml", "luad", "lusc", "ov", "read", 
                                                          "stad", "stes", "ucec"), 
                                              selected = c("brca", "coad", "coadread", "gbm", 
                                                           "gbmlgg", "kipan", "kirc", "kirp", 
                                                           "laml", "luad", "lusc", "ov", "read", 
                                                           "stad", "stes", "ucec"), 
                                              multiple = TRUE, selectize = TRUE)
                               ),
                               
                               mainPanel(
                                  p("Statistical plots showing the distribution of methylation"),
                                  br(),
                                  tabsetPanel(
                                     tabPanel("Cumulated histogram", 
                                              "A histogram showing the distribution of methylation
                                              for a given marker for all chosen cancer types", 
                                              br(), 
                                              plotOutput("histogram")), 
                                     tabPanel("Comparison histograms", 
                                              "Histograms showing the distribution of methylation 
                                              for a given marker, by type of chosen cancer", 
                                              br(), 
                                              plotOutput("compHistograms")), 
                                     tabPanel("Cumulated boxplot", 
                                              "A boxplot showing the distribution of methylation
                                              for a given marker for all chosen cancer types", 
                                              br(), 
                                              plotOutput("boxplot")), 
                                     tabPanel("Comparison boxplots", 
                                              "Boxplots showing the distribution of methylation
                                              for a given marker, by type of chosen cancer", 
                                              br(), 
                                              plotOutput("compBoxplots"))
                                    
                                  )
                           )
                        )),
                   tabPanel("Significant markers", 
                            sidebarLayout(
                               sidebarPanel(
                                  selectInput("cancer_table", label = "Choose cancer types:", 
                                              choices = c("brca", "coad", "coadread", "gbm", 
                                                          "gbmlgg", "kipan", "kirc", "kirp", 
                                                          "laml", "luad", "lusc", "ov", "read", 
                                                          "stad", "stes", "ucec"), 
                                              selected = "brca")
                               ), 
                               mainPanel(
                                  p("Information about significant markers"), 
                                  br(),
                                  tabsetPanel(
                                     tabPanel("Cancer-specific significant markers", 
                                              "A table showing 20 most significant markers for 
                                              a given cancer type", 
                                              br(), 
                                              tableOutput("signifCancer"))
                                     
                                     )
                                  )
                            )     
                   ),

                tabPanel("Common markers", 
                         sidebarLayout(
                           sidebarPanel(),
                          mainPanel(
                             p("Information about significant markers common for given cancer types"), 
                             br(),
                             tabsetPanel(
                               tabPanel("At least 2 cancer types:", 
                                      br(), 
                                        tableOutput("intersect2")),
                               tabPanel("At least 3 cancer types:", 
                                        br(), 
                                        tableOutput("intersect3")),
                               tabPanel("At least 4 cancer types:", 
                                        br(), 
                                        tableOutput("intersect4")),
                               tabPanel("Atleast 5 cancer types:", 
                                        br(), 
                                        tableOutput("intersect5"))
                               
                             )
                           )
                         )     
                ),
            
                tabPanel("survival curves",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("marker2", 
                                            label = "Choose a probe to analyse:",
                                            choices = markery_ist,
                                            selected = "cg00016968_RHOC"),
                                
                                selectInput("cancer2", label = "Choose cancer types:", 
                                                   choices = c("brca", "coad", "coadread", "gbm", 
                                                               "gbmlgg", "kipan", "kirc", "kirp", 
                                                               "laml", "luad", "lusc", "ov", "read", 
                                                               "stad", "stes", "ucec"), 
                                                   selected = "brca"),
                                sliderInput("methylation",
                                            "Stopien metylacji",min=1,max=99,value = 50)
                              ),
                              mainPanel(
                                p("Plots depicting Kaplan-Meier survival curves for 
                                  a given marker and a given cancer type. The division 
                                  into two groups is based on the degree of methylation."),
                                br(),
                                tabsetPanel(
                                  tabPanel("KM1", plotOutput("km", width = 500))
                            
                                )
                              )
                            ) 
                  
                )
))

