library(shiny)
library(stringi)
library(ggplot2)
library(survMisc) 
library(tidyr)
library(gridExtra)
library(data.table)
library(GGally)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(gtable)

load("data/mRNA.data.rda")
load("data/mRNA.data2.rda")
load("data/mediana.rda")
colnames(mediana)[2] <- 'marker'

mRNA.data <- data.table(mRNA.data)

shinyServer(function(input, output) {
  
  #    surv_model <- reactive({
  #       val <- mRNA.data[mRNA.data$cancer=="BRCA", input$marker]
  #       survfit(Surv(time, status == "dead")~(val>median(val)),
  #               data=mRNA.data[mRNA.data$cancer=="BRCA", ])
  #    })
  data <- reactive({
    data1 <- copy(mRNA.data)
    data1[time > input$time*30,  ":="(status = "alive",
                                      time = input$time*30)]
    
    data1
  })
  
  output$median <- renderTable({
    mediana[mediana$marker == input$marker & mediana$cancer %in% input$cancer ,]
  }, include.rownames=FALSE)
  
  output$KM <- renderUI({
    output$KM1 <- renderPlot({
      
      #       PValues <- sapply(input$cancer, function(x){
      #         
      #         val <- data()[cancer==x, eval(as.name(input$marker))]
      #         m <- survdiff(Surv(time, status == "dead")~(val>median(val)),
      #                      data=data()[cancer==x, ])
      #         1-pchisq(m$chisq, df=1)
      #         
      #       })
      #       
      #       names(PValues) <- input$cancer
      #       
      #       print(PValues)
      #       pValues <- sort(PValues)
      
      cols <- rainbow(13, alpha = 0.7)
      names(cols) <- unique(mRNA.data2$cancer)
      
      plots <- vector("list", length(input$cancer))
      
      PValues <- numeric(length(input$cancer))
      for(i in 1:length(input$cancer)){
        
        #x = names(PValues)[i]
        x <- input$cancer[i]
        val <- data()[cancer==x, eval(as.name(input$marker))]
        m <- survfit(Surv(time, status == "dead")~(val>median(val)),
                     data=data()[cancer==x, ])
        
        m2 <- survdiff(Surv(time, status == "dead")~(val>median(val)),
                       data=data()[cancer==x, ])
        PValues[i] <- 1-pchisq(m2$chisq, df=1)
        
        s = summary(m)
        ProbL <- min(s$surv[as.numeric(s$strata)==1]) # lower
        ProbH <- min(s$surv[as.numeric(s$strata)==2]) # higher
        k <- round(ProbL/ProbH, 2)
        k <- sapply(k, function(x){
          if(x==0 ||  x == Inf){
            res = NaN
          }
          else res = x
          return(res)
        })

        plots[[i]] <- autoplot(m, legLabs = c("lower", "higher"), survLineSize = 1,
                               censShape = 3, censSize = 3)$plot+
          #scale_color_manual(values=cols) +
          ggtitle(x) + 
          ylim(c(0,1)) +
          ylab('Survival') + xlab('Time') + 
          coord_cartesian(xlim= c(0, input$time*30)) + 
          scale_x_continuous(breaks=seq(0, input$time, by = 12)*30,
                             labels = seq(0,input$time, by = 12))+
          annotate("rect", xmin = 0, xmax = 12*input$time/2, ymin = 0, ymax = 0.25,
                   alpha = .2, colour = 'white')+
          annotate("text", x = 12, y = 0.17, xmin=10, 
                   label = stri_paste("p-value: ",round(PValues[i], 2)), 
                   hjust = 0, fontface = "bold") +
          annotate("text", x = 12, y = 0.07, xmin=10, 
                   label = stri_paste("k: ", k), 
                   hjust = 0, fontface = "bold")
      }
      
      
      top  <- textGrob("", gp=gpar(fontsize=20,font=2))
      marrangeGrob(plots, ncol = 2, nrow=ceiling(length(input$cancer)/2), top = top)
      #grid.arrange(plots, ncol = 2)
    })
    plotOutput("KM1", heigh= 150*length(input$cancer))
    
  })
  
  output$marker_box_plot <- renderPlot({
    
    
    cols <- rainbow(13, alpha = 0.7)
    names(cols) <- unique(mRNA.data2$cancer)
    ggplot(mRNA.data2[mRNA.data2$marker==input$marker &
                        mRNA.data2$cancer %in% input$cancer, ], 
           aes(x= reorder(cancer, val, FUN=median), y = val, fill=cancer)) +
      geom_violin() + 
      geom_boxplot(width = 0.4, aes(fill=NULL))+
      ylab(paste0('Value of marker ', input$marker)) + 
      xlab('') + 
      theme(axis.text.y=element_text(size=14, face="bold"),
            axis.text.x=element_text(size=14),
            axis.title=element_text(size=16)
      ) + 
      scale_fill_manual(values = cols, 
                        name = 'Type of cancer:') +
      theme(legend.position="none") + 
      #       theme(legend.title = element_text(colour="black", size=16, face="bold"),
      #             legend.text = element_text(colour="black", size = 14), 
      #             legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
      #             legend.position="top")+
      
      coord_flip()
  })
  
  
  output$violin <- renderUI({
    
    output$violin1 <- renderPlot({
      
      ggplot(mRNA.data2[mRNA.data2$marker==input$marker &
                          mRNA.data2$cancer %in% input$cancer, ], 
             aes(x= reorder(cancer, val, FUN=median), y = val)) +
        geom_violin(alpha = 0.5) + ylab(paste0('Value of marker ', input$marker)) + xlab('') + 
        theme(axis.text.x=element_text(size=14, face="bold"),
              axis.title=element_text(size=14, face = 'bold')
        ) + coord_flip()
      
    })
    
    plotOutput("violin1", width=500, heigh= 300*length(input$cancer))
    
  })
  
  
  output$pval <- renderText({
    val <- mRNA.data[, input$marker]
    x <- survdiff(Surv(time, status == "dead")~(val>median(val)),
                  data=mRNA.data)
    1-pchisq(x$chisq, df=1)
  })
  
  output$density <- renderPlot({
    
    #       h <- which(colnames(mRNA.data) == input$marker)
    #       
    #       p <- lapply(input$cancer, function(x) {
    #          m <- median(mRNA.data[mRNA.data$cancer == x,input$marker])
    #          print(m)
    #          ggplot(mRNA.data[cancer == x, c(cancer,eval(as.name(input$marker)))],
    #                 aes_string(x=colnames(mRNA.data)[h], fill=colnames(mRNA.data)[6])) +
    #             geom_density(alpha=0.5) + 
    #             ggtitle(paste0(x ,": ", 
    #                            input$marker)) + 
    #             ylim(c(0,1)) + geom_vline(xintercept = m)
    #       })
    #       marrangeGrob(p, ncol = 1, nrow=length(input$cancer))
    cols <- rainbow(13, alpha = 0.7)
    names(cols) <- unique(mRNA.data2$cancer)
    ggplot(mRNA.data2[mRNA.data2$marker == input$marker & mRNA.data2$cancer %in% input$cancer, ],
           aes(x=val, fill=cancer)) +
      geom_density()  + 
      theme(legend.position="none") + 
      xlab(paste0('Value of marker ', input$marker)) + 
      ylab('Density') + 
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            axis.title=element_text(size=16)) + 
      # geom_vline(aes(xintercept = median(val)))+
      scale_fill_manual(values = cols,
                        name = 'Type of cancer:') + 
      theme(legend.title = element_text(colour="black", size=16, face="bold"),
            legend.text = element_text(colour="black", size = 14), 
            legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
            legend.position="top")
    #facet_wrap(~cancer) 
  })
  
  #   output$median <- renderTable({
  #     mediana[mediana$marker == input$marker & mediana$cancer %in% input$cancer ,]
  #   }, include.rownames=FALSE)
  
})