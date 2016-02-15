#library(survMisc)
library(ggplot2)
library(stringi)
library(xtable)

names <- dir("data")
for (i in seq_along(names)){
   load(paste0("data/", names[i]))
}

options(scipen=999)


shinyServer(
   function(input, output) {
      
      markerName2 <- reactive({unlist(stri_extract_all_regex(input$marker2, "^.+(?=_)"))})
      signifCancers2 <- reactive({stri_trans_toupper(
         as.character(signif_probes$cancer[signif_probes$probe==markerName2()]))})
      restCancers2 <- reactive({stri_trans_toupper(
         as.character(unique(
            signif_probes$cancer[!(signif_probes$cancer %in% stri_trans_tolower(signifCancers2()))])))})
      fullNames2s <- reactive({cancer_names$full_name[cancer_names$abbreviation %in% signifCancers2()]})
      fullNames2r <- reactive({cancer_names$full_name[cancer_names$abbreviation %in% restCancers2()]})
      joint2s <- reactive({stri_paste(fullNames2s(), stri_paste("(", signifCancers2(), ")"), 
                                      "SIGNIFICANT", sep = " ")})
      joint2r <- reactive({stri_paste(fullNames2r(), stri_paste("(", restCancers2(), ")"), sep = " ")})
      joint2 <- reactive({c(joint2s(), joint2r())})
      
      quantile=stats::quantile
      
      output$dynamic1 <- renderUI({
         selectInput("cancer2", label = "Choose a cancer to analyse:", 
                     choices = joint2(), selected = joint2()[1])
      })
      
      output$km <- renderPlot({
         extracted <- unlist(stri_extract_all_regex(input$cancer2, "(?<=\\().*?(?=\\))"))
         if(is.null(input$cancer2)){
            return(plot.new())
#          } else if(!(input$cancer2 %in% signifCancers2())){
#          } else if(!(extracted %in% signifCancers2())){
#             return(plot.new())
         } else {
            cancerSet <- get(stri_trans_tolower(extracted))
            n <- which(colnames(cancerSet)==markerName2())
            methylation <- cancerSet[,n]
            x <- sort(methylation)
            k <- input$methylation 
            l <- length(methylation)
            threshold <- x[ceiling(k*l)]
            indays <- cancerSet$time
            inyears <- indays/365.25
            test <- survival::survfit(survival::Surv(inyears, status=="dead")~(methylation>threshold), data=cancerSet)
            test2 <- survival::survdiff(survival::Surv(inyears, status=="dead")~(methylation>threshold), data=cancerSet)
            pvalue <- 1 - pchisq(test2$chi, 1)
            #survMisc::autoplot(test, title = "Kaplan-Meier curves", nodeLabels=c("below", "over"), plotTable=TRUE)
#             survMisc::autoplot(test)$plot  + ylim(0,1) + xlim(0,19) + 
#                ylab("survival probability") + xlab("time (in years)") + 
#                ggtitle(paste("p-value: ", pvalue))
            survMisc::autoplot(test, legLabs = c("lower methylation", "higher methylation"), 
                               legTitle = "population")$plot + ylim(0,1) + xlim(0,19) + 
               ylab("survival probability") + xlab("time (in years)") + 
               ggtitle(paste("p-value: ", pvalue))
            #          ggsurv(test) + ylim(0,1) + xlim(0,19) + 
            #             ylab("survival probability") + xlab("time (in years)") + 
            #             scale_colour_discrete(name="Population", labels=c("below", "under"))   
         }      
})
      
      output$survtest <- renderPrint({
         extracted <- unlist(stri_extract_all_regex(input$cancer2, "(?<=\\().*?(?=\\))"))
         if(is.null(input$cancer2)){
            return(plot.new())
#          } else if(!(input$cancer2 %in% signifCancers2())){
#          } else if(!(extracted %in% signifCancers2())){
#             return(plot.new())
         } else {
            cancerSet <- get(stri_trans_tolower(extracted))
            n <- which(colnames(cancerSet)==markerName2())
            methylation <- cancerSet[,n]
            x <- sort(methylation)
            k <- input$methylation 
            l <- length(methylation)
            threshold <- x[ceiling(k*l)]
            indays <- cancerSet$time
            inyears <- indays/365.25
            test3 <- survival::survdiff(survival::Surv(inyears, status=="dead")~(methylation>threshold), data=cancerSet)
            names(test3$n)[1] <- "lower methylation"
            names(test3$n)[2] <- "higher methylation"
            test3
            #print(dimnames(test3$obs))
            #print(str(test3))
            #data.frame(test3$n, test3$obs, test3$exp, test3$var)
         }
      })
      

#       markerName <- reactive({unlist(stri_extract_all_regex(input$marker, "^.+(?=_)"))})
#       signifCancers <- reactive({stri_trans_toupper(
#          as.character(signif_probes$cancer[signif_probes$probe==markerName()]))})
# #       chosen <- reactive({signifCancers()[signifCancers() %in% input$cancer==TRUE]})
#       fullNames <- reactive({cancer_names$full_name[cancer_names$abbreviation %in% signifCancers()]})
#       joint <- reactive({stri_paste(fullNames2(), stri_paste("(", signifCancers2(), ")"), sep = " ")})
      
      markerName <- reactive({unlist(stri_extract_all_regex(input$marker, "^.+(?=_)"))})
      signifCancers <- reactive({stri_trans_toupper(
         as.character(signif_probes$cancer[signif_probes$probe==markerName()]))})
      restCancers <- reactive({stri_trans_toupper(
         as.character(unique(
            signif_probes$cancer[!(signif_probes$cancer %in% stri_trans_tolower(signifCancers()))])))})
      fullNamess <- reactive({cancer_names$full_name[cancer_names$abbreviation %in% signifCancers()]})
      fullNamesr <- reactive({cancer_names$full_name[cancer_names$abbreviation %in% restCancers()]})
      joints <- reactive({stri_paste(fullNamess(), stri_paste("(", signifCancers(), ")"), 
                                      "SIGNIFICANT", sep = " ")})
      jointr <- reactive({stri_paste(fullNamesr(), stri_paste("(", restCancers(), ")"), sep = " ")})
      joint <- reactive({c(joints(), jointr())})
      

      output$dynamic2 <- renderUI({
         selectInput("cancer", label = "Choose cancer types to analyse:", 
                     choices = joint(), 
                     selected = joint(), 
                     multiple = TRUE, selectize = TRUE)
      })

      
      output$histogram <- renderPlot({
         if (length(input$cancer)==0){
            return(NULL)
         } else {
            extracted <- unlist(stri_extract_all_regex(input$cancer, "(?<=\\().*?(?=\\))"))
            ggplot2::ggplot(data2[data2$marker==markerName() & (data2$cancer %in% extracted),], 
                            aes(x=methylation)) + geom_histogram(fill="blue", alpha=0.5) + 
               ylab("number of patients") + xlab("degree of methylation") + xlim(0,1) + 
               theme(axis.title=element_text(size=14), axis.text=element_text(size=10))
               
         }
      })
      
      output$compHistograms <- renderPlot({
         if (length(input$cancer)==0){
            return(NULL)
         } else {
            extracted <- unlist(stri_extract_all_regex(input$cancer, "(?<=\\().*?(?=\\))"))
            ggplot2::ggplot(data2[data2$marker==markerName() & (data2$cancer %in% extracted),], 
                   aes(x=methylation, fill=cancer, color=cancer)) + geom_histogram(alpha = 0.5) + 
              ylab("number of patients") + xlab("degree of methylation") + xlim(0,1) + 
              theme(axis.title=element_text(size=14), axis.text=element_text(size=10))
         }
      })
      
      output$compBoxplots <- renderPlot({
         if (length(input$cancer)==0){
            return(NULL)
         } else {
           quantile=stats::quantile
           extracted <- unlist(stri_extract_all_regex(input$cancer, "(?<=\\().*?(?=\\))"))
           ggplot2::ggplot(data2[data2$marker==markerName() & (data2$cancer %in% extracted),], 
                   aes(x=marker, y=methylation, fill=cancer)) + 
               geom_boxplot(position = position_dodge(width = 0.9)) + 
              ylab("degree of methylation") + xlab("name of probe") + 
              theme(axis.title=element_text(size=14), axis.text=element_text(size=12))
         }
      })
      
      extractedNames <- reactive({unlist(stri_extract_all_regex(input$cancer_table, "(?<=\\().*?(?=\\))"))})
      signifMarkers <- reactive({signif_probes$probe[signif_probes$cancer==stri_trans_tolower(extractedNames())]})

      output$signifCancer <- renderPrint({
           as.character(signifMarkers())[1:input$markers_displayed]
           })
      
      
      output$intersect1 <- renderPrint({
         signif1 <- as.character(significant1)[1:min(length(significant1),input$cmarkers_displayed)]
         cancers1 <- lapply(signif1, 
                            function(x) stri_paste(
                               stri_trans_toupper(as.character(
                                  signif_probes$cancer[signif_probes$probe==x])), 
                               collapse=", "))
         stri_paste(signif1, cancers1, sep = ": ")
      })

      output$intersect2 <- renderPrint({
         signif2 <- as.character(significant2)[1:min(length(significant2),input$cmarkers_displayed)]
         cancers2 <- lapply(signif2, 
                            function(x) stri_paste(
                               stri_trans_toupper(as.character(
                                  signif_probes$cancer[signif_probes$probe==x])), 
                               collapse=", "))
         stri_paste(signif2, cancers2, sep = ": ")
      })
      
      output$intersect3 <- renderPrint({
        signif3 <- as.character(significant3)[1:min(length(significant3),input$cmarkers_displayed)]
        cancers3 <- lapply(signif3, 
                           function(x) stri_paste(
                              stri_trans_toupper(as.character(
                                 signif_probes$cancer[signif_probes$probe==x])), 
                              collapse=", "))
        stri_paste(signif3, cancers3, sep = ": ")
      })
      
      output$intersect4 <- renderPrint({
        signif4 <- as.character(significant4)[1:min(length(significant4),input$cmarkers_displayed)]
        cancers4 <- lapply(signif4, 
                           function(x) stri_paste(
                              stri_trans_toupper(as.character(
                                 signif_probes$cancer[signif_probes$probe==x])), 
                              collapse=", "))
        stri_paste(signif4, cancers4, sep = ": ")
      })
      
      output$intersect5 <- renderPrint({
        signif5 <- as.character(significant5)[1:min(length(significant5),input$cmarkers_displayed)]
        cancers5 <- lapply(signif5, 
                           function(x) stri_paste(
                              stri_trans_toupper(as.character(
                                 signif_probes$cancer[signif_probes$probe==x])), 
                              collapse=", "))
        stri_paste(signif5, cancers5, sep = ": ")
      })

#       output$cancer_names <- renderPrint({
#          c("BRCA: Breast Invasive Carcinoma", "COAD: Colon Adenocarcinoma",
#            "COADREAD: Colon Adenocarcinoma and Rectum Adenocarcinoma combined", 
#            "GBM: Glioblastoma Multiforme", 
#            "GBMLGG: Glioblastoma Multiforme and Brain Lower Grade Glioma combined", 
#            "KIPAN: Kidney Renal Clear Cell Carcinoma and Kidney Renal Papillary Cell Carcinoma combined", 
#            "KIRC: Kidney Renal Clear Cell Carcinoma", 
#            "KIRP: Kidney Renal Papillary Cell Carcinoma",                                                
#            "LAML: Acute Myeloid Leukemia",                                                               
#            "LUAD: Lung Adenocarcinoma", 
#            "LUSC: Lung Squamous Cell Carcinoma",                                                          
#            "OV: Ovarian Serous Cystadenocarcinoma", 
#            "READ: Rectum Adenocarcinoma",                                                                 
#            "STAD: Stomach Adenocarcinoma",                                                                
#            "STES: Stomach Adenocarcinoma and Esophageal Carcinoma combined",                             
#            "UCEC: Uterine Corpus Endometrial Carcinoma")})

      
   }
)


#          cancers2 <- lapply(signif2, function(x) as.character(signif_probes$cancer[signif_probes$probe==x]))
#          cancersf <- lapply(cancers, function(y) as.factor(y))
#          stri_paste(1:length(signif2), stri_paste(signif2, cancers2, sep = ": "), sep=") ")
#          n <- length(signif2)
#          pasted <- character(n)
#          for(i in seq_along(signif2)){
#             pasted[i] <- stri_paste(signif2[i], ": ", stri_paste(cancers2[[i]]), sep=" ")
#          }
#          pasted
