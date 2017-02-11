library(shiny)
library(stringi)
library(ggplot2)
library(survival) 
#library(survMisc) 
library(stats)
library(tidyr)
library(gridExtra)
library(data.table)

load("data/dane.Rda")
load("data/razem.Rda")
load("data/bialka.Rda")
quantile = stats::quantile

shinyServer(function(input, output) {
   

   output$KM <- renderUI({
      output$KM1 <- renderPlot({
         p <- lapply(input$rak, function(x) {
            val <- dane[dane$rak==x, input$bialka]
            if (all(is.na(val))){
              df <- data.frame()
              ggplot(df)+ ylim(c(0,1)) + 
                ylab('Survival') + xlab('Time [days]')+ggtitle(paste0(x))+
              annotate("text",x=0.5,y=0.5, label = paste0("NO DATA FOR SELECTED TYPE OF PROTEIN \n FOR THIS TYPE OF CANCER"))
            }
            else
            {
            z <- median(val)
            m <- survfit(Surv(time, status == "dead")~(val>median(val)),
                         data=dane[dane$rak==x, ])
            ob <- survdiff(Surv(time, status == "dead")~(val>median(val)),
                           data=dane[dane$rak==x,])
            pvalue <- 1-pchisq(ob$chisq, df=1)
            survMisc::autoplot(m, legLabs = c(paste0("lower < ",round(z,digits=4)),paste0("higher > ",round(z,digits=4))))$plot+
               ggtitle(paste0(x,":","\n","Pvalue =", round(pvalue,digits=4))) +  
               ylim(c(0,1)) + 
               ylab('Survival') + xlab('Time [days]')
            }
         })
         
    
         marrangeGrob(p, ncol = 1, nrow=length(input$rak))
         
         
                        
           
         
         
      })
      plotOutput("KM1", width=600, heigh= 500*length(input$rak))
     
   })


   
   
   output$bialka_box_plot <- renderPlot({
     quantile = stats::quantile
     
      ggplot(razem[razem$bialka==input$bialka &
                     razem$rak %in% input$rak, ], 
             aes(x= reorder(rak, val, FUN=median), y = val)) +
         geom_boxplot() + ylab(paste0('Value of protein', input$bialka)) + xlab('') + 
       theme(axis.text.x=element_text(size=14, face="bold"),
             axis.title=element_text(size=14, face = 'bold')
             )+ coord_flip()
   
   })
   
   
   output$pval <- renderText({
     val <- dane[which(dane$rak==input$rak), c("rak",input$bialka)]
     z <- data.table(val)
     d <- z[,.N,by=.(rak)]
     a <- data.table(val[is.na(val[,2]),])[,.N,by=.(rak)]
     setkey(d,rak)
     setkey(a,rak)
     l <- merge(x = d, y = a, by ="rak",all.x = TRUE)
     colnames(l) <- c("rak","ilex","iley")
     if (any(l$ilex==l$iley) & nrow(l) > 1 & any(!is.na(l$iley))){ 
     print(paste0("NO DATA FOR SELECTED TYPE OF PROTEIN \n FOR THIS TYPE OF CANCER \n 
                  CHOOSE ANOTHER COMBINATION OF DATA!"))}
     else{
      val <- dane[which(dane$rak==input$rak), input$bialka]
     ob <- survdiff(Surv(time, status == "dead")~(val>median(val)),
                    data=dane[which(dane$rak==input$rak),])
      1-pchisq(ob$chisq, df=1)
     }
       })
   
   
  
   
   output$mytable2 = renderDataTable({
     t <- read.csv("data/Tabelka.csv",sep=";")
     t <- t[,2:4]
     t
   }, options = list(orderClasses = TRUE))
 
  
   output$density <- renderPlot({
     
     cols <- rainbow(35,alpha=0.3)
     names(cols) <- unique(razem$rak)
     ggplot(razem[razem$bialka == input$bialka & razem$rak %in% input$rak, ],
            aes(x=val, fill=rak)) +
       geom_density(colour="black",lwd=1.25)  + 
       theme(legend.position="none") + 
       xlab(paste0('Value of protein ', input$bialka)) + 
       ylab('Density') + 
       theme(axis.text.y=element_text(size=14),
             axis.text.x=element_text(size=14),
             axis.title=element_text(size=16)) + 
       scale_fill_manual(values = cols,
                         name = 'Type of cancer:') + 
       theme(legend.title = element_text(colour="black", size=16, face="bold"),
             legend.text = element_text(colour="black", size = 14), 
             legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
             legend.position="top")+
       guides(fill=guide_legend(nrow=4,byrow=TRUE))
     
  
     
   })   
    
 
     

   
   
   
})