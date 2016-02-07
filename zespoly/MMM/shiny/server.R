library(shiny)
library(stringi)
library(ggplot2)
library(survMisc) 
library(tidyr)
library(gridExtra)
library(data.table)
library(GGally)
library(grid)
library(ggplot2)
library(lattice)
library(DT)

load("data/mRNA.data.rda")
load("data/mRNA.data2.rda")


mRNA.data <- data.table(mRNA.data)
colnames(mRNA.data2)[length(colnames(mRNA.data2))] <- 'expression_gene'

makePlotContainers <- function(n, ncol=2, prefix="plot", height=100, width="100%", ...) {
  ## Validate inputs
  validateCssUnit(width)
  validateCssUnit(height)
  
  ## Construct plotOutputs
  lst <- lapply(seq.int(n), function(i)
    plotOutput(sprintf('%s_%g', prefix, i), height=height, width=width))
  
  ## Make columns
  lst <- lapply(split(lst, (seq.int(n)-1)%/%ncol), function(x) column(12/ncol, x))
  do.call(tagList, lst)
}

shinyServer(function(input, output) {
  
  
  renderPlots <- function(input, output, prefix="plot") {
    cols <- rainbow(13)
    names(cols) <- unique(mRNA.data2$cancer)
    cols2 <- rainbow(13, alpha = 0.4)
    names(cols2) <- unique(mRNA.data2$cancer)
    d1 <- data.frame(z = "higher", cancer = names(cols), 
                     col = cols, stringsAsFactors=FALSE) 
    d2 <- data.frame(z = "lower",cancer = names(cols2),
                     col = cols2, stringsAsFactors=FALSE) 
    cols <- rbind(d1,d2)
    plots <- vector("list", length(input$cancer))
    
    
    
    PValues <- numeric(length(input$cancer))
    for(i in 1:length(input$cancer)){
      local({
        ii = i
        output[[sprintf('%s_%g', prefix, ii)]] <- renderPlot({
          days <- 365*input$time/12
          #x = names(PValues)[i]
          x <- input$cancer[ii]
          val <- mRNA.data[cancer==x, eval(as.name(input$marker))]
          m <- survfit(Surv(time, status == "dead")~(val>median(val)),
                       data=mRNA.data[cancer==x, ])
          
          m2 <- survdiff(Surv(time, status == "dead")~(val>median(val)),
                         data=mRNA.data[cancer==x, ])
          PValues[i] <- 1-pchisq(m2$chisq, df=1)
          
          s = summary(m)
          ProbL <- min(s$surv[as.numeric(s$strata)==1 & s$time <= days]) # lower
          ProbH <- min(s$surv[as.numeric(s$strata)==2 & s$time <= days]) # higher
          OR <- round((ProbL/(1-ProbL))/(ProbH/(1-ProbH)), 2)
          if (mRNA.data[cancer==x,max(time,  na.rm=TRUE)] <days){
            OR = NaN 
          }
          curr_col <- cols[cols$cancer == x, "col"]
          
          p <- autoplot(m, legLabs = c("lower", "higher"), survLineSize = 1,
                        censShape = 3, censSize = 3, legend=FALSE)$plot+
            scale_colour_manual(values = curr_col,
                                name="Strata",
                                labels=c("higher", "lower")) +
            ggtitle(x) + 
            ylab('Survival') + xlab('Time') + 
            coord_cartesian(xlim= c(0, 120*30)) + 
            scale_x_continuous(breaks=seq(0, 10, by = 1)*365,
                               labels = seq(0,120, by = 12))+
            geom_rect(xmin = 0, xmax = 720, ymin = 0, ymax = 0.25,
                      alpha = .2, fill = "white", colour = "white")+
            annotate("text", x = 12, y = 0.07, xmin=10, 
                     label = stri_paste("OR (", input$time, " m.): ", OR), 
                     hjust = 0, fontface = "bold") +
            geom_vline(xintercept  = days, alpha = 0.5) +
            theme(legend.justification=c(1, 1), legend.position=c(1,1))+
            scale_y_continuous(limits = c(0,1), labels = scales::percent)
          if (PValues[i] < 0.05){
            p <- p + annotate("text", x = 12, y = 0.17, xmin=10, 
                              label = stri_paste("p-value: ",format(PValues[i], digit = 2,
                                                                    scientific = FALSE)), 
                              hjust = 0, fontface = "bold", colour = "red") 
          } else {
            p <-  p + annotate("text", x = 12, y = 0.17, xmin=10, 
                               label = stri_paste("p-value: ",format(PValues[i], digit = 2,
                                                                     scientific = FALSE)), 
                               hjust = 0, fontface = "bold")
          }
          p
        })
      })
    }
  }
  
  
  output$KM3 <- renderUI({
    n = length(input$cancer)
    makePlotContainers(n, ncol=ceiling(n/2), height= 200*min(n,2))
  })
  observeEvent(input, renderPlots(input, output))
  
  
  
  output$marker_box_plot <- renderPlot({
    
    
    cols <- rainbow(13, alpha = 0.7)
    names(cols) <- unique(mRNA.data2$cancer)
    ggplot(mRNA.data2[mRNA.data2$marker==input$marker &
                        mRNA.data2$cancer %in% input$cancer, ], 
           aes(x= reorder(cancer, expression_gene, FUN=median), y = expression_gene, fill=cancer)) +
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
      coord_flip()
    
  })
  
  
  
  
  
  mRNA.data2$expression_gene <- round(mRNA.data2$expression_gene, 4)
  output$table <- DT::renderDataTable(
    mRNA.data2[mRNA.data2$cancer %in% input$cancer & mRNA.data2$marker == input$marker,]
  )
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('RTCGA_mRNA', '.csv', sep='') },
    content = function(file) {
      write.csv(mRNA.data2[mRNA.data2$cancer %in% input$cancer & mRNA.data2$marker == input$marker,], file)
    }
  )
  
  output$text <- renderUI({
    str1 <- paste("Detailed information about the",input$marker," can be found ")
    str2 <- paste("https://en.wikipedia.org/wiki/",input$marker)
    HTML(str1,
         paste('<a href="',str2,'">here</a>.'))
    
  })
  
  
})