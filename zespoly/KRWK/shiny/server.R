library(shiny)
library(ggplot2)
library(survMisc)
#All cancer types that we consider
cancer_types<-c("BRCA","COAD","COADREAD","GBMLGG","KIPAN","KIRC","KIRP",
                "LGG","LUAD","LUSC","OV","READ","UCEC")
# cancer_types<-c("READ","UCEC")

#Read in data and pvalues for each combination (cancer,gene)
for(i in 1:length(cancer_types)){
   eval(parse(text=paste0(cancer_types[i],"data <- readRDS('./data/",
                          cancer_types[i],"dane.RDS')")))
#    eval(parse(text=paste0(cancer_types[i],"pvalue <- readRDS('./data/",
#                           cancer_types[i],"pvalue.RDS')")))
   eval(parse(text=paste0(cancer_types[i],"genes_list <- readRDS('./data/",
                          cancer_types[i],"genes_list.RDS')")))
}

shinyServer(function(input, output){
   #Creating select list with genes
   output$significant_genes<-renderUI({
      #Creating list to show as : "GENE_NAME (p-value: VALUE)"
      #Genes are already sorted from the most significat at the begining
      #If the value wasn't counted pvalue is "-"
      genes_list <- eval(parse(text=paste0("tmp <- ",input$cancer, 
                                           "genes_list")))
      selectInput("choosen_gene", label=h3(strong("mRNA")), 
                  choices=tmp, selected=tmp[[1]])
      
   })
   output$significant_genesSurv<-renderUI({
     #Creating list to show as : "GENE_NAME (p-value: VALUE)"
     #Genes are already sorted from the most significat at the begining
     #If the value wasn't counted pvalue is "-"
     eval(parse(text=paste0("tmp <- ",input$cancerSurv, 
                                          "genes_list")))
     selectInput("choosen_geneSurv", label=h3(strong("mRNA")), 
                 choices=tmp, selected=tmp[[1]])
     
   })
   output$significant_genesCompGenes<-renderUI({
     #Creating list to show as : "GENE_NAME (p-value: VALUE)"
     #Genes are already sorted from the most significat at the begining
     #If the value wasn't counted pvalue is "-"
     eval(parse(text=paste0("tmp <- ",input$cancerSurv, 
                            "genes_list")))
     names(tmp) <- NULL
     selectInput("choosen_geneCompGenes", label=h3(strong("mRNA")), 
                 choices=tmp, selected=tmp[[1]])
     
   })
   #Creating stratum value slider for survival curve plot
   output$plot_help_surv1 <- renderUI({
     eval(parse(text=paste0("gene <- ",input$cancerSurv, 
                            "data[, '",input$choosen_geneSurv,"']")))
      max <- max(gene, na.rm=TRUE)
      min <- min(gene, na.rm=TRUE)
      med <- median(gene, na.rm=TRUE)
      sliderInput("stratum_value_choosen", label="Stratum value: ", 
                  min=round(min,2), max=round(max,2),
                  value=med, step=0.01, round=-2, ticks=FALSE)

   })
   
   #Creating text information under surv - plot
   output$plot_help_surv2 <- renderText({
     eval(parse(text=paste0("gene <- ",input$cancerSurv, 
                            "data[, '",input$choosen_geneSurv,"']")))
     n_low <- length(gene[gene<=input$stratum_value_choosen])
     n_high <- length(gene[gene>input$stratum_value_choosen])
      
     paste0("Number of observations <=", input$stratum_value_choosen,
                     ": ", n_low, br() , 
                     "Number of observations >", input$stratum_value_choosen,
                     ": ", n_high, br(),br(),
                     strong("Attention:"), br(),
                     "If the number of observations is small in one of 
                      above groups
                     the mRNA can seem to be significant, 
                     but it does not have to be.",br(),br())
     
   })
   
   #Create text information under histogram plot
   output$plot_help_hist <- renderUI({
     eval(parse(text=paste0("gene <- ",input$cancer, 
                            "data[, '",input$choosen_gene,"']")))
     helpText(paste0("Number of observations: ",length(gene[!is.na(gene)])))
     
   })
   
   #Creating plot itself
   output$histogram <- renderPlot({
        eval(parse(text=paste0("q <- ",
                               input$cancer, "data[,'",input$choosen_gene,
                               "',drop=FALSE]")))
         q<-ggplot(q,aes_string(names(q)[1]))+ geom_histogram(binwidth = 0.1)+
            scale_y_continuous(name="Number of observations")+
            scale_x_continuous(name="Values")+
            ggtitle(paste0("mRNA histogram ",input$choosen_gene))+
            theme(plot.title = element_text(lineheight=20, face="bold"))
      q
   })
   
   
   output$survplot <- renderPlot({
       eval(parse(text=paste0("tmp <- which(colnames(",input$cancerSurv,
                              "data)=='", input$choosen_geneSurv,"')")))
       eval(parse(text=paste0("dane <- ",input$cancerSurv, "data[,c(1,",
                              tmp,",(ncol(",
                              input$cancerSurv,"data)-1):ncol(",
                              input$cancerSurv,"data))]")))
       dane$time <- dane$time/365
       med <- input$stratum_value_choosen
       eval(parse(text=paste0(
         'q<-survfit(Surv(time, status == "dead")~(', input$choosen_geneSurv, 
         ">med), data=dane)")))
       try({suppressMessages({
         q<-autoplot(q,xLab="years",yLab="",
                     title=paste0("Survival plot for mRNA ",input$choosen_geneSurv,
                                  "\n1-P(death)"),
                     legTitle = "Stratum",
                     legLabs=c(paste0("Patients with value < ",round(med,4)),
                               paste0("Patients with value > ",
                                      round(med,4))))$plot+scale_y_continuous(
                                        limits=c(0,1)
                            )+theme(legend.justification=c(0,0),
                                    legend.position=c(0,0))})}, silent=TRUE)
     q
   })
   
   
   output$survplotComp1 <- renderPlot({
     eval(parse(text=paste0("tmp <- which(colnames(",input$cancerComp1,
                            "data)=='", input$choosen_geneCompGenes,"')")))
     eval(parse(text=paste0("dane <- ",input$cancerComp1, "data[,c(1,",
                            tmp,",(ncol(",
                            input$cancerComp1,"data)-1):ncol(",
                            input$cancerComp1,"data))]")))
     dane$time <- dane$time/365
     med <- median(dane[ ,2], na.rm=TRUE)
     eval(parse(text=paste0(
       'q<-survfit(Surv(time, status == "dead")~(', input$choosen_geneCompGenes, 
       ">med), data=dane)")))
     try({suppressMessages({
       q<-autoplot(q,xLab="years",yLab="",
                   title=paste0("Survival plot for  ",input$cancerComp1,
                                "\n1-P(death)"),
                   legTitle = "Stratum",
                   legLabs=c(paste0("Patients with value < ",round(med,4)),
                             paste0("Patients with value > ",
                                    round(med,4))))$plot+scale_y_continuous(
                                      limits=c(0,1)
                                      )+theme(legend.justification=c(0,0),
                                              legend.position=c(0,0))})}, silent=TRUE)
     q
   })
   
   output$survplotComp2 <- renderPlot({
     eval(parse(text=paste0("tmp <- which(colnames(",input$cancerComp2,
                            "data)=='", input$choosen_geneCompGenes,"')")))
     eval(parse(text=paste0("dane <- ",input$cancerComp2, "data[,c(1,",
                            tmp,",(ncol(",
                            input$cancerComp2,"data)-1):ncol(",
                            input$cancerComp2,"data))]")))
     dane$time <- dane$time/365
     med <- median(dane[ ,2], na.rm=TRUE)
     eval(parse(text=paste0(
       'q<-survfit(Surv(time, status == "dead")~(', input$choosen_geneCompGenes, 
       ">med), data=dane)")))
     try({suppressMessages({
       q<-autoplot(q,xLab="years",yLab="",
                   title=paste0("Survival plot for ",input$cancerComp2,
                                "\n1-P(death)"),
                   legTitle = "Stratum",
                   legLabs=c(paste0("Patients with value < ",round(med,4)),
                             paste0("Patients with value > ",
                                    round(med,4))))$plot+scale_y_continuous(
                                      limits=c(0,1)
                                      )+theme(legend.justification=c(0,0),
                                              legend.position=c(0,0))
       })}, silent=TRUE)
     q
   })
   
   output$survplotComp3 <- renderPlot({
     eval(parse(text=paste0("tmp <- which(colnames(",input$cancerComp3,
                            "data)=='", input$choosen_geneCompGenes,"')")))
     eval(parse(text=paste0("dane <- ",input$cancerComp3, "data[,c(1,",
                            tmp,",(ncol(",
                            input$cancerComp3,"data)-1):ncol(",
                            input$cancerComp3,"data))]")))
     dane$time <- dane$time/365
     med <- median(dane[ ,2], na.rm=TRUE)
     eval(parse(text=paste0(
       'q<-survfit(Surv(time, status == "dead")~(', input$choosen_geneCompGenes, 
       ">med), data=dane)")))
     try({suppressMessages({
       q<-autoplot(q,xLab="years",yLab="",
                   title=paste0("Survival plot for ",input$cancerComp3,
                                "\n1-P(death)"),
                   legTitle = "Stratum",
                   legLabs=c(paste0("Patients with value < ",round(med,4)),
                             paste0("Patients with value > ",
                                    round(med,4))))$plot+scale_y_continuous(
                                      limits=c(0,1))+theme(legend.justification=c(0,0),
                                                           legend.position=c(0,0))})}, 
       silent=TRUE)
     q
   })
   
   output$survplotComp4 <- renderPlot({
     eval(parse(text=paste0("tmp <- which(colnames(",input$cancerComp4,
                            "data)=='", input$choosen_geneCompGenes,"')")))
     eval(parse(text=paste0("dane <- ",input$cancerComp4, "data[,c(1,",
                            tmp,",(ncol(",
                            input$cancerComp4,"data)-1):ncol(",
                            input$cancerComp4,"data))]")))
     dane$time <- dane$time/365
     med <- median(dane[ ,2], na.rm=TRUE)
     eval(parse(text=paste0(
       'q<-survfit(Surv(time, status == "dead")~(', input$choosen_geneCompGenes, 
       ">med), data=dane)")))
     try({suppressMessages({
       q<-autoplot(q,xLab="years",yLab="",
                   title=paste0("Survival plot for ",input$cancerComp4,
                                "\n1-P(death)"),
                   legTitle = "Stratum",
                   legLabs=c(paste0("Patients with value < ",round(med,4)),
                             paste0("Patients with value > ",
                                    round(med,4))))$plot+scale_y_continuous(
                                      limits=c(0,1)
                                      )+theme(legend.justification=c(0,0),
                                              legend.position=c(0,0))})}, silent=TRUE)
     q
   })
})


