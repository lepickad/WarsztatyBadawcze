library(shiny)
library(ggplot2)
library(survMisc)
library(stringi)
sciezka = '/E/szkola/methylation/'
setwd(sciezka)
# zapis wszystkich inofrmacji w aplikacji po angielsku
# pliki maja byc w tym miejscu gdzie sa pliki ui.
# nie uzywac setwd

# dodac stopien mrtyacji
# zmiana zamiast mediane niech uzytkownik wybiera stopien metylacji
# czy stpien powinine byc jeden globalny cyz nie
# pokazac rozklad metylacji w roznych nowotrowach
# rozklad biomakera w danym nowotworze np. hist

# nazwe genu zawrzec w nazwie biomarkera

# do nowotworow dodac mozlwisoc multiwyboru
# ale zeby nie bylo to sklejenie tylko zeby senswone ocenic
# kilka nowotowor naraz - cos zamiast KM

# przygotowac jeden sensonwy zbior danych i wczytywac przed shnyServer()

# wyslac na githuba 
#dodac opis nowotoworow np. ze w danym nowotworze jest 500 obs

# legenda do wykresu
# jaki procent osob doyżje np 5 lat (survdiff)


shinyServer(function(input, output) {
  
 
# zmienic os y (skkala od )  
  output$wykres <- renderPlot({
    sciezka = stri_paste('pliki_rda/','all_', input$rak,'.rda')
    zbior = get(load(sciezka))
    # jak to zrobic aby wczytywac plik przed output, zeby nie trzeba bylo
    # za kazdym razem wczytywac?
    n=which(colnames(zbior)==input$marker)
    if (length(n)!=0){
      mediana <- median(zbior[,n], na.rm = TRUE)
      test <- survfit(Surv(time, status == "dead")~(zbior[,n]>mediana), data = zbior)
      autoplot(test,legend=FALSE)$plot +ylim(0,1)
    }else{
      cat("Wybrany przez Ciebie marker nie występuje w danym nowotrze")
    }
   
  })
  
  output$podsumowanie <- renderPrint({
    cat("Podsumowanie się robi...")
  })
  
})