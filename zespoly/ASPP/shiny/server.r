#library(survMisc)
library(ggplot2)
library(stringi)
library(xtable)


load("data/data2.rda")
load("data/signif_probes.rda")
load("data/istotne2.rda")
load("data/istotne3.rda")
load("data/istotne4.rda")
load("data/istotne5.rda")
name = dir('km')
for (i in seq_along(name)){
  load(paste0('km/',name[i]))
}

shinyServer(
   function(input, output) {
      markerName <- reactive({unlist(stri_extract_all_regex(input$marker, "^.+(?=_)"))})
      signifCancers <- reactive({as.character(signif_probes$cancer[signif_probes$probe==markerName()])})
      chosen <- reactive({chosen <- signifCancers()[signifCancers() %in% input$cancer ==TRUE]})
      signifMarkers <- reactive({signif_probes$probe[signif_probes$cancer==input$cancer_table]})
      quantile=stats::quantile
       output$histogram <- renderPlot({
         if (length(input$cancer)==0){
            return(NULL)
         } else if (length(chosen())==0){
            return(NULL)
         } else {
            #hist(data2$methylation[data2$marker==markerName() & (data2$cancer %in% chosen())])
           ggplot2::ggplot(data2[data2$marker==markerName() & (data2$cancer %in% input$cancer),], 
                   aes(x=methylation)) + geom_histogram(fill="red", alpha=0.5)
         }
      })
      output$compHistograms <- renderPlot({
         if (length(chosen())==0){
            return(NULL)
         } else {
           ggplot2::ggplot(data2[data2$marker==markerName() & (data2$cancer %in% input$cancer),], 
                   aes(x=methylation, fill=cancer, color=cancer)) + geom_histogram(alpha = 0.2)
         }
      })
      output$boxplot <- renderPlot({
         if (length(chosen())==0){
            return(NULL)
         } else {
           quantile=stats::quantile
           ggplot2::ggplot(data2[data2$marker==markerName() & (data2$cancer %in% input$cancer),], 
                   aes(x=marker, y=methylation)) + ggplot2::geom_boxplot()
         }
      })
      output$compBoxplots <- renderPlot({
         if (length(chosen())==0){
            return(NULL)
         } else {
           quantile=stats::quantile
           ggplot2::ggplot(data2[data2$marker==markerName() & (data2$cancer %in% input$cancer),], 
                   aes(x=marker, y=methylation, fill=cancer)) + 
               geom_boxplot(position = position_dodge(width = 0.9))
         }
      })
      output$signifCancer <- renderTable({
         #data.frame(marker = signifMarkers())[1:20,]
         head(data.frame(marker = signifMarkers()), 20)
      })
      
      output$intersect2 <- renderTable({
      data.frame(istotne2)
      })
      output$intersect3 <- renderTable({
        data.frame(istotne3)
      })
      output$intersect4 <- renderTable({
        data.frame(istotne4)
      })
      output$intersect5 <- renderTable({
        data.frame(istotne5)
      })
      output$km <- renderPlot({
        marker = stri_sub(input$marker2,1,10)
        zbior = get(input$cancer2)
        n = which(colnames(zbior)==marker)
        x = sort(zbior[,n])
        k = (input$methylation)/100
        l = length(zbior[,n])
        y = x[ceiling(k*l)]
        test = survival::survfit(survival::Surv(time, status == "dead")~(zbior[,n]>y), data = zbior)
        survMisc::autoplot(test,legend=FALSE)$plot +ylim(0,1)
      })
      
   }
)
