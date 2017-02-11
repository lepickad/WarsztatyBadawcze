load("data/probe_gene.rda")
load("data/joint_names.rda")

shinyUI(navbarPage("Methylation profile and survival analysis", 
                   tabPanel("Introduction", 
                            sidebarLayout(
                               sidebarPanel(
                                  h1("Research workshops", align = "center"),
                                  h2("Methylation profile", align = "center"),
                                  h3("Adrianna Sudol & Pawel Pytlak", align = "center"),
                                  br(),
                                  h4("Faculty of Mathematics and Information Science", align  = "center"), 
                                  h4("Warsaw University of Technology", align = "center"),
                                  br(),
                                  h4("Dear user! Enjoy our application!", align = "center")
                                  ),
                               
                               mainPanel(
                                  h2(strong("A brief introduction to the application")),
                                  br(),
                                  h4("The application primarily aims to show the relationship between the ", 
                                     a("DNA methylation profile", href = "https://en.wikipedia.org/wiki/DNA_methylation"), 
                                    " and the survival time of patients suffering from particular cancers."),
                                  br(),
                                  h4("The analysis presented in the application is based on the data derived 
                                     from the ", a("RTCGA.methylation", href = "https://github.com/RTCGA/RTCGA.methylation"), 
                                     " and ", a("RTCGA.clinical", href = "https://github.com/RTCGA/RTCGA.clinical"), 
                                     "platforms. 16 types of cancers are considered, as well as almost 2500 different ", 
                                     a("CpG sites", href = "https://en.wikipedia.org/wiki/CpG_site"), 
                                     ", hereinafter refered to as ", span(strong("probes,")), 
                                     " where the methylation occurs. The set of chosen probes is restricted to the top 200 
                                     significant probes for each cancer type."), 
                                  br(),
                                  h4("The first panel, ", span(strong("Survival curves")), 
                                     ", depicts the Kaplan-Meier survival curves for a given probe and a 
                                     given cancer type, as well as the results of the corresponding log-rank test. 
                                     In the second panel, ", span(strong("Significant probes")), ", the names 
                                     of probes significant for a given cancer type are shown. In the third panel, ", 
                                     span(strong("Common probes")), ", the names of probes significant for at 
                                     least one cancer type are presented. The fourth panel, ", 
                                     span(strong("Distribution of methylation")), ", depicts three 
                                     statistical plots showing the distribution of methylation for a 
                                     given probe and given cancer types.")
                                  
                                  )
                            
                            )),
                   
                   tabPanel("Survival curves",
                            sidebarLayout(
                               sidebarPanel(
                                  
                                 selectInput("marker2", 
                                              label = "Choose a probe to analyse:",
                                              choices = probe_gene,
                                              selected = "cg00009407_TTC8"),
                                 
                                 br(),
                                 
                                 helpText("The term", span(strong(" SIGNIFICANT ")), 
                                          "after certain cancer names in the input below indicates the 
                                            significance of the chosen probe for these cancers."),
                                 
                                 uiOutput("dynamic1"),
                                  
                                 br(),
                                 
                                 helpText("Choose an appropriate threshold dividing the 
                                           two groups of patients, based on the degree of 
                                           methylation."),
                                    
                                 sliderInput("methylation",
                                              "Choose a threshold:", min=0.01, max=0.99, 
                                              value = 0.5),
                                 
                                 br(),

                                 p("The plot in the upper right-hand side depicts the 
                                    Kaplan-Meier survival curves for the chosen probe and 
                                    the selected cancer type. The green and orange curves 
                                    refer to the patients with a methylation profile 
                                    respectively above and below the chosen threshold. 
                                    "),
                                 
                                 p("The table in the lower right-hand side shows the basic 
                                   information concerning the log-rank test used to compare 
                                   the survival probability in the two groups, e.g. the 
                                   number of patients in each group (marked as ", 
                                   span(em("N")), ").")
                              ),
                               mainPanel(
                                  
                                  plotOutput("km"), 
                                  br(),
                                  verbatimTextOutput("survtest"))
                            ) 
                            
                   ),
                   
                   tabPanel("Significant probes", 
                            sidebarLayout(
                               sidebarPanel(
                                  selectInput("cancer_table", label = "Choose a cancer type:", 
                                              choices = joint_names, 
                                              selected = joint_names[1]),
                                  
                                  sliderInput("markers_displayed", 
                                              label = "Choose the number of probes to be 
                                              displayed:",
                                              min = 1, max = 200, value = 20),
                                  
                                  br(),
                                  
                                  p("The table beside depicts the names of probes being
                                    the part of the 200 most significant probes for a given 
                                    cancer type. The number of probes to be displayed can be 
                                    altered by choosing an appropriate value in the input 
                                    above.")
                                  
                               ), 
                               mainPanel(
                                  
                                  h3(strong("Cancer-specific significant probes")),
                                  
                                  verbatimTextOutput("signifCancer")
                               )
                                     
                            )   
                               
                                    
                   ),
                   
                   tabPanel("Common probes", 
                            sidebarLayout(
                               sidebarPanel(
                                  sliderInput("cmarkers_displayed", 
                                              label = "Choose the number of probes to be displayed:",
                                              min = 1, max = 1800, value = 20),
                                  
                                  br(),
                                  
                                  
                                  
                                  p("The panels beside contain the list of probe names 
                                    significant to at least one cancer type. The probes 
                                    displayed are taken from the set of 3200 most significant 
                                    probes (200 for each cancer type). In addition, the 
                                    respective tables contain the abbreviations of cancer 
                                    types for which the given probes are significant. The full 
                                    names of cancer types are presented below."),
                                  
                                 br(),
                                  
                                  p(span(strong("BRCA: ")), "Breast Invasive Carcinoma",                                                              
                                    span(strong("COAD: ")), "Colon Adenocarcinoma",                                                                 
                                    span(strong("COADREAD: ")), "Colon Adenocarcinoma and Rectum Adenocarcinoma combined",                          
                                    span(strong("GBM: ")), "Glioblastoma Multiforme",                                                               
                                    span(strong("GBMLGG: ")),"Glioblastoma Multiforme and Brain Lower Grade Glioma combined",                      
                                    span(strong("KIPAN: ")),"Kidney Renal Clear Cell Carcinoma and Kidney Renal Papillary Cell Carcinoma combined",
                                    span(strong("KIRC: ")),"Kidney Renal Clear Cell Carcinoma",                                                    
                                    span(strong("KIRP: ")),"Kidney Renal Papillary Cell Carcinoma",                                                
                                    span(strong("LAML: ")),"Acute Myeloid Leukemia",                                                               
                                    span(strong("LUAD: ")),"Lung Adenocarcinoma",                                                                  
                                    span(strong("LUSC: ")),"Lung Squamous Cell Carcinoma",                                                         
                                    span(strong("OV: ")),"Ovarian Serous Cystadenocarcinoma",                                                      
                                    span(strong("READ: ")),"Rectum Adenocarcinoma",                                                                
                                    span(strong("STAD: ")),"Stomach Adenocarcinoma",                                                               
                                    span(strong("STES: ")),"Stomach Adenocarcinoma and Esophageal Carcinoma combined",                             
                                    span(strong("UCEC: ")),"Uterine Corpus Endometrial Carcinoma")
                               ),
                               
                              mainPanel(
                                  h3(strong("Significant markers common for given cancer types")), 
                                  tabsetPanel(
                                     tabPanel("1 cancer type", 
                                              br(), 
                                              verbatimTextOutput("intersect1")),
                                     tabPanel("2 cancer types", 
                                              br(), 
                                              verbatimTextOutput("intersect2")),
                                     tabPanel("3 cancer types", 
                                              br(), 
                                              verbatimTextOutput("intersect3")),
                                     tabPanel("4 cancer types", 
                                              br(), 
                                              verbatimTextOutput("intersect4")),
                                     tabPanel("5 cancer types", 
                                              br(), 
                                              verbatimTextOutput("intersect5"))
                                     
                                  )
                               )
                            )     
                   ),
                   
                   
                   tabPanel("Distribution of methylation", 
                            sidebarLayout(
                               sidebarPanel(
                                  
                                  selectInput("marker", 
                                              label = "Choose a probe to analyse:",
                                              choices = probe_gene,
                                              selected = "cg00009407_TTC8"),
                                  
                                  br(),
                                  
                                  helpText("The term", span(strong(" SIGNIFICANT ")), 
                                           "after certain cancer names in the input below indicates the 
                                            significance of the chosen probe for these cancers."),
                                  
                                  uiOutput("dynamic2")),
                               
                               mainPanel(
                                  h3(strong("Distribution of methylation for a given probe")),
                                  tabsetPanel(
                                     tabPanel("Cumulated histogram", 
                                              h4("The following plot presents a histogram of methylation for the selected probe, 
                                                 jointly for the selected cancer types (by default all cancer 
                                                 types are chosen)."),
                                              plotOutput("histogram") 
                                              ), 
                                     
                                     tabPanel("Comparison histogram",
                                              h4("The plot below depicts a histogram of methylation for the chosen probe, 
                                        by type of cancer selected, i.e. each colour in the bin shows the 
                                        frequency of occurrence of patients with a given methylation 
                                        degree and with a given cancer type (by default all cancer 
                                        types are chosen)."),
                                              plotOutput("compHistograms")
                                              ), 
                                     
                                     tabPanel("Comparison boxplots",
                                              h4("The following plot shows methylation boxplots for the selected probe and selected cancer 
                                     types (by default all cancer types are chosen)."),
                                              plotOutput("compBoxplots")
                                              
                                              )
                                    
                                  )
                           )
                        ))
                   
            
                
))
