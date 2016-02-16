library(shiny)
library(gridExtra)
library(grid)
library(dplyr)
library(ggplot2)
# library(ggplot2, lib.loc='/home/warsztaty/R/x86_64-pc-linux-gnu-library/3.2/')


raki <- c("BRCA", "COAD", "GBM", "KIPAN", 'KIRC', "LAML", "LUAD", "LUSC", "UCEC")
### Ładowanie pvaluesów ###
for(cancer in raki){
  load(paste0('data/pvalues/',cancer, '.rda'))
  load(paste0('data/final_sig/', cancer, '.final_sig.rda'))
  assign( paste0(cancer, '_signifPval'),
          l$pvalues[l$pvalues<0.05])
}

shinyServer(function(input, output, session) {
    
  #### Selecting common features from given types of cancer ####  
  observe({
    if( length(input$cancers) >0 ){
      cancerTypes <- sapply(strsplit(input$cancers, " "), function(x) x[1])
      isolate({
        lapply( cancerTypes, function(cancer){
          fileName <- paste0(cancer, '_signifPval')
          if(cancer=='OV') names(get(fileName))[-c(1:6)] else
            names(get(fileName))[-c(1:7)]
        }) -> allFeatures
        commonFeatures <- Reduce(intersect, allFeatures)
        cF = commonFeatures
        updateSelectInput(session, "features", choices = cF, 
                          label = paste0( '2. Choose from ', length(cF), " common biomarkers:"))
      })
    }
  })
  
  features <- eventReactive(input$ok, {
    input$features
  })
  cancers <- eventReactive(input$ok, {
    sapply(strsplit(input$cancers, " "), function(x) x[1])
  })
  thresholds <- reactive({
    if( input$co == 'median' )  FUN = median
    if( input$co == 'mean' )    FUN = mean
    lapply(cancers(), function(cancer){
      sapply(features(), function(feature){
        if( input$co != 'manual' ){
          data <- get(paste0(cancer, ".final_sig"))[, feature ]
          do.call(FUN, list(data, na.rm=TRUE))
          } else {
            input$threshold
          }
        })
      }) -> threshold
    setNames(threshold, cancers())
  })

  table <- reactive({
    load("data/dict.rda")
    if( input$all ) cancers <- raki else cancers <- cancers()
    sapply( cancers, function(cancer){
      fileName <- paste0(cancer, '_signifPval')
      out <- get(fileName)[-c(1:7)]
      cbind(cancer = cancer, feature = names(out), p.val = format(out, digits=2, scientific = FALSE))
    }, USE.NAMES=TRUE) -> signifFeatures
    df <- do.call(rbind, signifFeatures)
    data.frame(df, row.names = NULL) %>%
      group_by( feature ) %>%
      dplyr::summarize( 
                 Common_for = paste0(cancer, "(", p.val, ")", collapse=';\n '),
                 Sum_of_common = n()) -> df    
    df <- merge(df, dict, by.x='feature', by.y="REF")
    df <- df[,c(1,4,2,3)]
    colnames(df) <- c("Biomarker name", "Gene name", "Common for (w/ p.val for survival)", "Number of common cancers")
    DT::datatable(df, options = list(pageLength = 100), 
                  rownames=FALSE)
    df
  })
  
  boxplotData <- eventReactive(input$ok, {
    lapply( cancers(), function(cancer){
      temp <- get(paste0( cancer, ".final_sig"))
      temp <- temp[, features() ]
      temp <- cbind(reshape2::melt(temp, measure.vars = features()), cancerType = cancer)
    }) -> plotData
    plotData <- do.call(rbind,plotData)
    if( length(features()) == 1 )
      plotData <- cbind(variable = features(), plotData)
    plotData
  })

  survPlotList <- reactive({
    features <- features()
    cancers <- cancers()
    ploty <- as.list(rep(0,length(cancers)*length(features)))
    names(ploty) <- outer(cancers,features,function(x,y) paste0(x,"&",y))
    for(cancer in cancers){
      for( feature in features ){
        data <- get(paste0(cancer, ".final_sig"))[, c('status','time',feature) ]
        threshold <- thresholds()[[cancer]][feature]
        plt <- survival::survfit(survival::Surv(time, status == "dead") ~ (data[,feature] > threshold), 
                                 data = data)
        if( !is.null(names(plt$strata)) ){
          pm_part <- ifelse(grepl("FALSE",names(plt$strata)), '-', '+')
          kind_part <- ifelse( input$co!='manual', input$co, threshold)
          names(plt$strata) <- paste(kind_part,pm_part)
        }
        plt <- survMisc::autoplot(plt, yLab = cancer)$plot
        plt <- plt +
          ggtitle(feature) +
          theme(legend.position = 'bottom',
                legend.text=element_text(size=15),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=15),
                title = element_text(size=10)) 
        ploty[[ paste0(cancer,'&',feature)]] <- plt
      }
    }
  ploty
})

  OR <- eventReactive(c(input$days,input$co, input$threshold), {
    days <- input$days
    lapply(cancers(), function(cancer){
      sapply(features(), function(feature){
        data <- get(paste0(cancer, ".final_sig"))[, c('status','time',feature) ]
        status2 <- ifelse( data$time > days, 
                           "alive",
                           ifelse( !is.na(data$time < days), 'dead', NA))
        thrs <- thresholds()[[cancer]][feature]
        strata1 <- (data[,feature] > thrs)
        if( length(unique(strata1))==2 ){
          m <- glm( as.factor(status2) ~ strata1, family=binomial)
          OR <- as.numeric(format(exp(coef(m))[2], digits=2, scientific = F))
          OR_reverse <-  as.numeric(format(1/exp(coef(m))[2], digits=2, scientific = F))
          if( OR>1 )
            prt <- paste0("Dying risk:\n", OR, "x HIGHER for '+'")
          if( OR<1 )
            prt <- paste0("Dying risk:\n", OR_reverse, "x LOWER for '+'")
          if( OR==1 )
            prt <- paste("Equal odds")
          prt
        } else print('Dying risk:\n NA (one stratum)')
#           validate(need(FALSE,
#                         message="I have no capability to calculate odds ratio with only one group available.\nPlease, change the threshold so as to obtain two groups to compare with each other.\nIf you are not sure about appropriate value, take advantage of 'Features distribution' panel."))   
      })
    }) -> OR
    setNames(OR, cancers())
  })
  
  ksPValues <- reactive({
    plotData <- boxplotData()
    ########### KS-test
    X <- split(plotData,plotData$cancerType)
    X2 <- lapply(X, function(x) split(x, x$variable))
    X2 <- lapply(X2, function(x) lapply(x, function(y) y$value))
  
    lapply(features(), function(feature){
      sapply(cancers(), function(cancer1){
        sapply(cancers(), function(cancer2){
            t=ks.test(X2[[cancer1]][[feature]], X2[[cancer2]][[feature]], alternative = "less")
            t$p.value
        })
      })
    }) -> PValues 
    names(PValues) <- features()
    PValues
  })

  preparePlot <- reactive({
    plotData <- boxplotData() 
    ### BOXPLOT
    g_box <- ggplot(data=plotData, aes(x=as.factor(variable), y=as.numeric(value), fill=cancerType)) +
      geom_boxplot() + xlab("") + ylab("") +
      theme(legend.position='none',
            text = element_text(size=14)) +
      coord_flip() + scale_x_discrete(limits=rev(features()))

    ### DENSITY
    g_dens <- ggplot(plotData, aes(x=value, fill=cancerType)) +
      geom_density( alpha=0.5, stat='density') + ylab("") + xlab("") +
      theme(legend.position='bottom',
            text = element_text(size=14)) +
      facet_grid(variable ~ ., scales = "free") +
      guides(fill = guide_legend(override.aes= list(alpha = .8)))
    return(list(boxplot=g_box, density=g_dens))
  })
##################################################################
############################# OUTPUT #############################
##################################################################
  #### TAB_PANEL ####
  output$downloadTable <- downloadHandler(
    filename = function() { paste("table", '.csv', sep='') },
    content = function(file) {
      write.csv(table(), file)
    }
  )
  output$featuresIntersect <- renderDataTable({
    validate(
      need( (length(input$cancers)>1 | input$all==TRUE), "There is nothing to see. Select more cancer types and press Go! button.")
    )
    table()
  })
  
  #### PLOT_PANEL 2 ####
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0(input$plot_kind,"_",Sys.Date(),".rda") },
    content = function(file) {
        plt <- preparePlot()[[input$plot_kind]]
        save(plt, file=file)
      }
  )

  output$boxplots <- renderPlot({
    validate(
      need( length(input$cancers)>0, "There is nothing to see. Select more cancer types and press Go! button.")
    )
    validate(
      need( length(input$features)>0, "There is nothing to see. Select more biomarkers and press Go! button.")
    )
    plots <- preparePlot()
    opts <- ggplotGrob(plots$density)$grobs
    leg <- opts[[which(sapply(opts, function(x) x$name=='guide-box'))]]
    lheight <- sum(leg$height)
    
    PValues <- ksPValues()
    if( input$ks == 'none' | length(input$cancers)<2 ){
      arrangeGrob(plots$boxplot + theme(legend.position='none'),
                  plots$density + theme(legend.position='none'),
                  ncol=2) -> arrange_grob
    }else{
      n <- dim(PValues[[1]])
      if( input$ks == 'pval' & length(input$cancers)>1 ){
        ksPVals <- lapply(PValues, function(x){
          tab <- format(x, digits=2) %>% as.table
          stars <- ifelse(x<0.05, "***","") %>% as.table
          arr <- array(c(tab,stars), c(n,2), dimnames=dimnames(tab))
          out <- apply(arr, 1:2, function(a) paste0(a[1],a[2]))
          rownames(out) <- tolower(rownames(out))
          out
        })
      }
      if( input$ks == 'decision' & length(input$cancers)>1 ){
        ksPVals <- lapply(PValues, function(tab){
          ifelse( tab<.05, '<', '=') -> tab
          rownames(tab) <- tolower(rownames(tab))
          tab
        })
      }
      tables_arrange <- do.call(arrangeGrob, 
                                list(grobs=lapply(ksPVals,tableGrob),ncol=1))
      arrangeGrob(plots$boxplot + theme(legend.position='none'),
                  plots$density + theme(legend.position='none'),
                  tables_arrange,
                  ncol=3) -> arrange_grob
    }
  
    grid.arrange(
      arrange_grob,
      leg, nrow=2, heights = unit.c(unit(1, "npc") - lheight, lheight))
    
  })
  
  #### SURVIVAL ####
  output$downloadSurv <- downloadHandler(
    filename = function() { paste0("plotSurvList_",Sys.Date(),".rda") },
    content = function(file) {
      pltSurvList <- survPlotList()
      save(pltSurvList, file=file)
    }
  )

  output$survival <- renderPlot({
    validate(need( length(input$cancers)>0, "There is nothing to see. Select more cancer types and press Go! button."))
    validate(need( length(input$features)>0, "There is nothing to see. Select more biomarkers and press Go! button."))
    validate(need( !is.na(input$days), "Waiting for days value..."))
    
    k <- length(input$features)
    n <- length(cancers())
    
    lapply(names(survPlotList()), function(plt){
      cancer <- strsplit(plt, '&')[[1]][1]
      feature <- strsplit(plt, '&')[[1]][2]
      out <- survPlotList()[[plt]]
      if( input$days > 0 ){
        or <- OR()[[cancer]][feature]
        out <- out +
        geom_vline(data=data.frame(days=input$days),
                   aes(xintercept = days ), linetype='dashed', color='darkgray' ) +
        annotate(geom="label", x=Inf, y=Inf, label=or, vjust = "inward", hjust = "inward",
                 color='black', fontface='bold', family='Courier')
      }
      if( input$pv == 'show' ){
        pval <- get(paste0(cancer, '_signifPval'))[feature]
        pval <- format(pval, scientific = FALSE, digits=2)
        pval <- paste0("P-value:\n", pval)
        out <-  out +
          annotate('label', x=0, y=0, label=pval, vjust = "inward", hjust = "inward",
                   color='black', fontface='bold', family='Courier')
      }
      out + theme(legend.position='none', axis.text.x = element_text(size=13))
    }) -> plotList
    
    opts <- ggplotGrob(survPlotList()[[1]])$grobs
    leg <- opts[[which(sapply(opts, function(x) x$name=='guide-box'))]]    
    lheight <- sum(leg$height)

    names(plotList) <- names(survPlotList())

    grid.arrange( arrangeGrob(
      do.call('arrangeGrob', c(grobs=plotList, list(ncol=n))),
                  left=textGrob("Survival (fraction)", gp=gpar(fontsize=10, col='dimgray'), rot=90),
                  bottom=textGrob("Time (days)", gp=gpar(fontsize=10, col='dimgray'),
                 )),
                  leg, nrow=2, heights = unit.c(unit(1, "npc") - lheight, lheight))
  })
})