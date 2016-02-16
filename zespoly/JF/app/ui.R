library(shiny)

raki <- c("BRCA", "COAD", "GBM", "KIPAN", 'KIRC', "LAML", "LUAD", "LUSC", "UCEC")
## Chunk for getting data info from gdac page 
# library(XML)
# theurl <- "http://gdac.broadinstitute.org/"
# tables <- readHTMLTable(theurl)
# table <- tables$counts_table
# library(dplyr)
# library(tidyr)
# table %>%
#   filter( Cohort %in% raki ) %>%
#   mutate(Cases = paste0( `Disease Name`, " (", Cases, ")")) %>%
#   select(Cases) -> raki2
raki2 <- c(
"'Breast invasive carcinoma' (343)",
"'Colon adenocarcinoma' (202)",
"'Glioblastoma multiforme' (283)",
"'Pan-kidney cohort (KICH+KIRC+KIRP)' (973)",
"'Kidney renal clear cell carcinoma' (439)",
"'Acute Myeloid Leukemia' (194)",
"'Lung adenocarcinoma' (89)",
"'Lung squamous cell carcinoma' (160)",
"'Uterine Corpus Endometrial Carcinoma' (118)")
# "'Rectum adenocarcinoma' (73)",
# "'to i ovo' (666)r")

shinyUI(fluidPage(
  titlePanel(NULL, "RTCGA methylation explorator"),
  br(),
  sidebarLayout(position='right',
    sidebarPanel(width=3,

      selectInput('cancers', 
                  label= strong('1. Choose from 9 cancer types: ', h6(em('(here is the number of cases)'))),
                  paste(raki, raki2),
                  selected = c("BRCA 'Breast invasive carcinoma' (343)", 
                               "COAD", "'Colon adenocarcinoma' (202)"),
                  multiple = TRUE),
      br(),
      selectInput('features', 
                  label='2. Choose from ... common biomarkers:',
                  c(), multiple = TRUE),
      actionButton("ok", "\nGo!", icon=icon('bicycle')),
      hr(),
      h4(icon('cogs'), 'Further options:'), 
      ############# SURVIVAL CURVES PANEL ############# 
      conditionalPanel(condition = "input.tabs1 == 'sc'",
        radioButtons('co', 'Strata cut-off:',
                     c('median' = 'median',
                       'mean'='mean',
                       'manual'='manual'), selected='median', inline=TRUE),
        conditionalPanel(condition = "input.co == 'manual'",
                       sliderInput('threshold', "Manual threshold:",
                                   min=0, max=1, value=0.5)),
        numericInput('days',
                     label=strong('The risk of dying within (days):'),
                     0, min=0, step=1),
        h6(em('(Insert 0 to skip this value.)')),
        radioButtons('pv', 
                     label=strong("P-value of significant difference between the populations indicated by", tags$u("median:")),
                     c('show' = 'show',
                       'hide' = 'hide'), inline=TRUE, selected='hide'),
        hr(),
        downloadButton('downloadSurv', 'Download survival curves')
      ),
      ############# FEATURE DISTRIBUTION PANEL  ############# 
      conditionalPanel(condition = "input.tabs1 == 'fd'",
                       conditionalPanel(condition = "input.cancers.length > 1",
                                        h5('Two sample Kolmogorov-Smirnov test, comparing distributions:'),
                                        h5('H0: cancer=CANCER'),
                                        h5('H1: cancer<CANCER'),
                                        radioButtons('ks', NULL,
                                                     c('P-value' = 'pval',
                                                       'Decision' = 'decision',
                                                       'Hide' = 'none'), 
                                                     inline=TRUE, selected='none')),
                       hr(),
                       downloadButton('downloadPlot', 'Download:'),
                       radioButtons('plot_kind', NULL, inline=TRUE,
                                    choices = c('boxplot' = 'boxplot','density' = 'density'))),
      ############# TABLE PANEL ############# 
      conditionalPanel(condition = "input.tabs1 == 'gn'",
                       checkboxInput('all', "Select all cancers", value=FALSE),
                       hr(),
                       downloadButton('downloadTable', 'Download Table'))
      ),
    
    mainPanel(
      navbarPage(id='tabs1',
                tabPanel(" "),
                tabPanel("RTCGA METHYLATION", icon=icon("info-circle"),
                         h4(strong('Welcome in our explorator of methylation data!')),
                         p('The', 
                            a('RTCGA package', href='https://github.com/RTCGA/RTCGA.methylation'),
                            'provides accessible and easy way to analyse data from',
                            a('TCGA project.', href='http://cancergenome.nih.gov/'),
                           'This application allows you to go into the data and visualize interesting
                            connections between DNA characteristics and the length of survival time
                            of patiens stricken with different types of cancer.', align='justify'),
                         hr(),
                         tags$details(
                           tags$summary(
                             strong('What you can find here?')
                             ),
                           tags$ul(
                             h5(strong(icon('leaf'), 'Survival')),
                             h6(em('The influence of particular biomarkers on survival and differences between them in 
                             different types of cancer.')),
                             tags$ul(
                               tags$li('Survival curves with possibility to customise strata (setting up methylation cut-off).'),
                               tags$li('Significance level (p-value) for given biomarker that has an influence on survival in strata indicated by median.'),
                               tags$li('Odds ratio for time interval and startum set up by user.'),
                               h6(icon('warning'), 'There is no possibility to calculate odds ratio
                                  with single stratum. In that case change the methylation cut-off 
                                  so as to obtain two groups to compare with each other. If you are not sure about appropriate threshold
                                  value, take advantage of', em('Biomarkers distribution'), 'panel.')),
                             h5(strong(icon('area-chart'), 'Biomarkers distribution')),
                             h6(em('What is the distribution of methylation values for the biomarkers
                                    in different types of cancer? Are there any similarities between them?')),
                             tags$ul(
                               tags$li('Boxplots and density plots for each biomarker.'),
                               tags$li('Kolmogorow-Smirnov test results from comparing a biomarker distribution in each two types of cancer.')),
                             h5(strong(icon('reorder'), 'Biomarkers list')),
                             h6(em('How can I find gene name for my biomarker (DNA loci name)?
                                   Is it possible to find genes that are suitable to my biomarkers?')),
                             tags$ul(
                               tags$li('Biomarker names (loci on DNA).'),
                               tags$li('Gene name (in accordance with given locus). Notice, that one locus may have more than one accordant gene.'),
                               tags$li('P-value as the measure of biomarker significancy for survival where strata are defined by median.'),
                               tags$li('Number of a biomarker appearances in given cancer types.')
                             )
                           )
                         ),
                         hr(),
                         tags$details(
                           tags$summary(strong('More about data specification')),
                           tags$ul(
                             tags$li('There are 9 different types of cancer.'),
                             tags$li('In each of the cancer types we have different number of cases (assays of DNA).'),
                             tags$li('We extracted over 13,000 biomarkers (from almost 300,000) that are significant for survival. They are listed in', em('Gene names'),'panel.'),
                             tags$li('Data values are from unit interval and they describe percentage of methylation for each biomarker.')
                           )
                         ),
                         h5('Enjoy!', align='right'),
                         hr(),
                         h6(icon('female'), ' Katarzyna Sobiczewska', code('fk.katarzyna@gmail.com'), '(author, creator)', align='right'),
                         h6(icon('female'), ' Justyna Jankowiak', code('just.jankowiak@gmail.com'), '(author)', align='right'),
                         hr(),
                         h6('This project was created in cooperation with', a('Faculty of Mathematics and Information Science', href='http://www.mini.pw.edu.pl/tikiwiki/tiki-index.php'),
                            'at', a('Warsaw University of Technology', href='https://www.pw.edu.pl/engpw'), align='right'),
                         br()
                         ),
                tabPanel("Survival", icon=icon('leaf'), value = 'sc',
                          plotOutput('survival', width='100%', height='600px')),
                tabPanel("Biomarkers distribution", icon=icon('area-chart'), value = 'fd',
                          plotOutput('boxplots', width='100%', height='600px')),
                tabPanel("Biomarkers list", icon=icon('reorder'), value='gn',
                     dataTableOutput('featuresIntersect')))
#       ,  type="pills")
, width=9 )
    )
  )
)
