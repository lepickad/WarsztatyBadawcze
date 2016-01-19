library(gridExtra)

raki <- c("BRCA", "COAD", "GBM", "KIPAN", 'KIRC', "LAML",
          "LUAD", "LUSC", "UCEC")
### Ładowanie pvaluesów ###
for(cancer in raki){
  load(paste0('data/pvalues/',cancer, '.rda'))
  load(paste0('data/final_sig/', cancer, '.final_sig.rda'))
  assign( paste0(cancer, '_signifPval'),
          l$pvalues[l$pvalues<0.05])
}

shinyServer(function(input, output, session) {
    
  #### Selecting common features from given types of cancer ####  
  #### OK BUTTON ####
  observe({
    cancerTypes <- input$cancers
    isolate({
      lapply( cancerTypes, function(cancer){
        fileName <- paste0(cancer, '_signifPval')
        names(get(fileName))[-c(1:7)]
      }) -> allFeatures
      commonFeatures <- Reduce(intersect, allFeatures)
      cF = commonFeatures
      updateSelectInput(session, "features", choices = cF)
    })
  })

  features <- eventReactive(input$ok, {
    input$features
  })
  cancers <- eventReactive(input$ok, {
    input$cancers
  })
  
  table <- eventReactive(input$ok, {
    load("data/dict.rda")
    sapply( input$cancers, function(cancer){
      fileName <- paste0(cancer, '_signifPval')
      out <- names(get(fileName))[-c(1:7)]
      out#[out %in% input$features]
    }, USE.NAMES=TRUE) -> signifFeatures
    df = reshape2::melt(signifFeatures)
    colnames(df) <- c('FeatureName', 'CancerType')
    df$FeatureName <- as.character(df$FeatureName)
    df %>%
      group_by( FeatureName ) %>%
      summarise( Freq = n(),
                 CancerTypes = paste(CancerType, collapse=';')) -> df    
    df <- merge(df, dict, by.x='FeatureName', by.y="REF")
    df <- df[,c(1,4,2,3)]
    DT::datatable(df, options = list(pageLength = 100), 
                  rownames=FALSE)
  })
  
  boxplotData <- eventReactive(input$ok, {
    lapply( input$cancers, function(cancer){
      temp <- get(paste0( cancer, ".final_sig"))
      temp <- temp[, input$features ]
      temp <- cbind(reshape2::melt(temp, measure.vars = input$features), cancerType = cancer)
    }) -> plotData
    plotData <- do.call(rbind,plotData)
    if( length(input$features) == 1 )
      plotData <- cbind(variable = input$features, plotData)
    plotData
  })
  
  #### TAB_PANEL 1 ####
  output$featuresIntersect <- renderDataTable({
    validate(
      need( length(input$cancers)>1, "There is nothing to see. Select more cancer types and press Go! button.")
    )
    table()
  })
  
  #### BOXPLOT_PANEL 2 ####
  output$boxplots <- renderPlot({
    validate(
      need( length(input$cancers)>0, "There is nothing to see. Select more cancer types and press Go! button.")
    )
    validate(
      need( length(input$features)>0, "There is nothing to see. Select more features and press Go! button.")
    )
    plotData <- boxplotData()
    library(ggplot2)
    ggplot(data=plotData, aes(x=variable, y=as.numeric(value), fill=cancerType)) +
      geom_boxplot() +
      xlab("") +
      ylab("methylation value") +
      theme(text = element_text(size=14)) +
      ggtitle("Features distribution in RTCGA.methylation")
  })
  
  #### DENSITY_PANEL 3 ####
  output$density <- renderPlot({
    validate(need( input$cancers>0, "There is nothing to see. Select more cancer types and press Go! button."))
    validate(need( input$features>0, "There is nothing to see. Select more features and press Go! button."))
    features <- features()
    cancers <- cancers()
    
    cbPalette <- rep(
      c("#E69F00", "#56B4E9", "#009E73", "#CC79A7",
        "#F0E442", "#0072B2", "#D55E00", "#999999"),
      10e5)
    names(cbPalette) <- cancers
    for(cancer in cancers){
      data <- data.frame(get(paste0(cancer, ".final_sig"))[, features ])
      colnames(data) <- features
      data <- tidyr::gather(data)
      data %>%
        group_by(key) %>%
        summarise(mean = mean(value),
                  median = median(value))  %>% 
        mutate( manual = rep(input$threshold, nrow(.)) ) -> mu
      ggplot(data, aes(x = value)) + 
        geom_density(fill = cbPalette[cancer]) + 
        scale_x_continuous(limits=c(0,1)) +
        xlab('') + ylab(cancer) +
        theme(plot.margin = grid::unit(c(1,1,1,1), 'mm')) +
        geom_vline(data = mu, aes_string(xintercept = input$co))+
#                                          ,linetype="dashed")) + 
        facet_wrap(~ key, scales = "free") -> plott
      assign(paste0("plot", cancer), plott)
    }
    ploty <- paste0("plot", cancers)
    expression <- paste0("grid.arrange(", 
                         paste(ploty, collapse = ","),
                         ",nrow = ",
                         length(input$cancers),
                         ",ncol = ", 1, ")")
    eval(parse(text = expression))
  })
  
  #### DENSITY_PANEL 4 ####
  output$survival <- renderPlot({
    validate(need( input$cancers>0, "There is nothing to see. Select more cancer types and press Go! button."))
    validate(need( input$features>0, "There is nothing to see. Select more features and press Go! button."))
    features <- features()
    cancers <- cancers()
    if( input$co == 'median' )  FUN = median
    if( input$co == 'mean' )    FUN = mean

    for(cancer in cancers){
      data <- get(paste0(cancer, ".final_sig"))[, c('status','time',features) ]
      if( length(features) == 1 ){
        d <- data.frame(data[, features])
        colnames(d) <- features
      } else d <- data[, features]
      if( input$co != 'manual' )
        thresholds <- apply(d,2,FUN) else
        thresholds <- input$threshold
      for( feature in features ){
        assign( feature, data[,feature] )
        threshold <- ifelse( input$co=='manual', thresholds, thresholds[feature] )
        plt <- survival::survfit(survival::Surv(time, status == "dead") ~ (get(feature) > threshold), data = data)
        if( !is.null(names(plt$strata)) )
          if( input$co != 'manual')
            names(plt$strata) <- paste0(input$co, c('-','+')) else
              names(plt$strata) <- paste0(input$threshold, c('-','+'))
        pval <- get(paste0(cancer, '_signifPval'))[feature]
        pval <- format(pval, scientific = T, digits=3)
        plt <- survMisc::autoplot(plt, title = paste(cancer, "-", feature, "\n (p-val=", pval, ")"))$plot
        plt <- plt + 
          theme(legend.justification = c(0,0), legend.position = c(0,0),
               text = element_text(size=4))
#               scale_y_continuous(limits = c(0, 1))
        assign( paste0(cancer,feature), plt )
      }
    }
    ploty <- outer(cancers,features, FUN="paste0")
    expression <- paste0("grid.arrange(", paste(ploty, collapse = ","), 
                         ",nrow = ", length(cancers), ",ncol = ", length(features), ")")
    eval(parse(text = expression))

  })
})