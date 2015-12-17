##### WARSZTATY BADAWCZE - ETAP 1 #####

library(devtools)
library(RTCGA.clinical) #cechy kliniczne
library(RTCGA.methylation) #metylacja
library(ggplot2)
library(dplyr)
library(survMisc)

## u mnie
#all <- list.files("~/WarsztatyBadawcze/JF/data.methylation")
## u Ciebie
# data <- data(package='RTCA.methylation')
# all <- data$result[,"Item"]

### SKRYPT DO LICZENIA P-WARTOSĆI DLA KAŻDEGO NOWOTWORU I ZAPISU DO PLIKU
for(set in all){
  print(set)
  ## u mnie
  #load(file.path("~/WarsztatyBadawcze/JF","data.methylation",lf[k]))
  #dataset <- substring(set,1, nchar(set)-4)
  ## u Ciebie
  #zamiast tego wczytanie kolejnego zbioru z listy.
  #nie wiem, data(set) czy jakoś tak.
  nowotwor <- strsplit(set, "\\.")[[1]][1]
  n <- dim(get(set))[1]/4
  # wybieramy tylko te zbiory, w których mamy co najmniej 100 obserwacji.
  if( n < 100 ){
    cat("Za malo obserwacji w zbiorze",set)
  } else {
    dataset.clinical <- paste(nowotwor, "clinical", sep='.')
    cliS <- data.frame(time1 = as.numeric(as.character(get(dataset.clinical)[,'patient.days_to_death'])),
                     time2 = as.numeric(as.character(get(dataset.clinical)[,'patient.days_to_last_followup'])),
                     status = get(dataset.clinical)[,'patient.vital_status'],
                     barcode = get(dataset.clinical)[,'patient.bcr_patient_barcode'])
    cliS$time <- ifelse(is.na(cliS$time1), cliS$time2, cliS$time1)
  
    patients <- rownames(get(set))
    patient_id <- tolower(substring(patients, 1, 12))
    patient_rest <- tolower(substring(patients, 14))
    a <- data.frame(patient_id, patient_rest, row.names = NULL)
    b <- data.frame(get(set), row.names = NULL)
    do_zlaczenia <- cbind(a, b)
    final <- merge(cliS, do_zlaczenia, by.x = "barcode", by.y = "patient_id")
    # zwalniam pamiec
    rm(list=c(set,"a","b","do_zlaczenia","cliS"))
    gc(verbose=T)
    #do każdego pacjenta mamy 4 wiersze, ten z pomiarami to ten, gdzie mamy "Beta_value"
    final <- final %>% filter(Composite.Element.REF == "Beta_value")
  
    # 7 pierwszych kolumn to dane pacjenta (czas przeżycia itd.), dopiero od 8 mamy markery
    ile <- ncol(final)
    pvalue <- numeric(ile)
    names(pvalue) <- colnames(final)
    start.time <- Sys.time()
    for (i in 8:ile){
      if (length(unique(final[, i])) <= 1){
        pvalue[i] <- 100
      } else {
        if(i%%100==0) cat(i, "/", ile-7, "\n")
        predyktor <- as.numeric(as.character(final[, i]))
        mediana <- median(as.numeric(as.character(final[, i])), na.rm = TRUE)
        model <- survdiff(Surv(time, status == "dead") ~ (predyktor > mediana), data = final)
        stat <- model$chisq
        pvalue[i] <- 1 - pchisq(stat, df = 1)
      }
    }
    end.time <- Sys.time()
    cat(nowotwor,":")
    print(end.time-start.time)
    l <- list(pvalues=pvalue, n=n)
    save(l, file=paste0("~/WarsztatyBadawcze/JF/pvalues/",nowotwor,".rda"))
  }
}

### FUNCKAJ DO RYSOWANIA 100 NAJISTOTNIEJSZYCH P-WARTOSCI
plt_survival_curves <- function(pvalue, nowotwor){
    # przypisujemy rosnący porządek pvalue (najmniejsze p-value tzn. najbardziej istotny marker)
    o <- order(pvalue, decreasing = FALSE)
    #bierzemy 100 najbardziej istotnych 
    #(niektóre mogą być zmierzone tylko dla jednego pacjenta, więc takich nie będziemy uważać za istotne dla całej grupy pacjentów)
    j = 1
    i = 1
    curve.path <- "~/WarsztatyBadawcze/JF/survival_curves.methylation"
    if(!dir.exists(file.path(curve.path,nowotwor)))
      dir.create(file.path(curve.path,nowotwor))
    while (i <= 100){
      print(i)
      ktory <- which(o == j)
      predyktor <- as.numeric(as.character(final[, ktory + 7]))
      j <- j + 1
      if (length(unique(predyktor)) == 1 | all(is.na(predyktor))) {
        next 
      } else {
        mediana <- median(as.numeric(as.character(final[, ktory + 7])), na.rm = TRUE)
        do_wykresu <- survfit(Surv(time, status == "dead") ~ (predyktor > mediana), data = final)
        nazwa <- colnames(final)[ktory + 7]
        #pomarańczowe predyktor > mediana = FALSE (legenda troche przeszkadza, na razie usuwam)
        wykres <- autoplot(do_wykresu, title = paste("Survival for ", nazwa, "(", i, ")"), legend = FALSE)
        ggsave(file = file.path(curve.path, nowotwor, paste0(i, "_", nazwa, ".png")), plot = wykres$plot)
        i <- i + 1
      }
    }
  }
}
