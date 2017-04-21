library(shiny)
library(rworldmap)
# Define server logic for random distribution application
function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  selectedData <- reactive({
    ramka_1_w_s[ramka_1_w_s$ST004D01T==as.character(input$sex) &ramka_1_w_s$item_short==as.character(input$subject) &ramka_1_w_s$result==as.character(input$result),]
  })
  
  
  clusters <- reactive({
    kmeans(selectedData()[-1,4:13], input$n)
  })
  
  dane <- reactive({ cbind(selectedData()[-1,],clus=clusters()[1])})
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
  
    n <- input$n
    dan<-dane()
    danee<-dan[order(dan$clus),]
    
    
    
    
    
    malDF <- data.frame(country = danee$CNT,
                        cluster = danee$clus)
    # malDF is a data.frame with the ISO3 country names plus a variable to
    # merge to the map data
    
    malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                                  nameJoinColumn = "country")
    # This will join your malDF data.frame to the country map data
    
    mapCountryData(malMap, nameColumnToPlot="cluster", catMethod = "categorical",mapTitle="MAP",
                   missingCountryCol = gray(.9))
    
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(selectedData())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame( dane()[order(dane()$clus),])
  })
  
}