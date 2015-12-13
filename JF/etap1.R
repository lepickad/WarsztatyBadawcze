##### WARSZTATY BADAWCZE - ETAP 1 #####

library(stringi)
library(RTCGA.clinical) #cechy kliniczne
library(RTCGA.methylation) #metylacja

data("BRCA.clinical")
data("BRCA.methylation")

cliS <- data.frame(time1 = as.numeric(as.character(BRCA.clinical$patient.days_to_death)),
                   time2 = as.numeric(as.character(BRCA.clinical$patient.days_to_last_followup)),
                   status = BRCA.clinical$patient.vital_status,
                   barcode = BRCA.clinical$patient.bcr_patient_barcode)
cliS$time <- ifelse(is.na(cliS$time1), cliS$time2, cliS$time1)
head(cliS)  

rows <- rownames(BRCA.methylation)
rows_id <- stri_trans_tolower(stri_sub(rows, from = 1, to = 12))
rows_rest <- stri_trans_tolower(stri_sub(rows, from = 14, to = -1))
a <- data.frame(rows_id, rows_rest, row.names = NULL)
b <- data.frame(BRCA.methylation, row.names = NULL)
do_zlaczenia <- cbind(a, b)

final <- merge(cliS, do_zlaczenia, by.x = "barcode", by.y = "rows_id")

#do każdego pacjenta mamy 4 wiersze, prawdopodobnie ten z pomiarami to ten, gdzie mamy "Beta_value"
library(dplyr)
final <- final %>% filter(Composite.Element.REF == "Beta_value")

library(ggplot2)
library(survMisc)

# 7 pierwszych kolumn to dane pacjenta (czas przeżycia itd.), dopiero od 8 mamy markery
ile <- ncol(final)
pvalue <- numeric(ile)
for (i in 8:ile){
  if (length(unique(final[, i])) <= 1){
    pvalue[i] <- 100
  } else {
    print(i)
    predyktor <- as.numeric(as.character(final[, i]))
    mediana <- median(as.numeric(as.character(final[, i])), na.rm = TRUE)
    model <- survdiff(Surv(time, status == "dead") ~ (predyktor > mediana), data = final)
    stat <- model$chisq
    pvalue[i] <- 1 - pchisq(stat, df = 1)
  }
}

#porządkujemy wg p-value rosnąco (najmniejsze p-value tzn. najbardziej istotny marker)
o <- order(pvalue, decreasing = FALSE)

#bierzemy 100 najbardziej istotnych 
#(niektóre mogą być zmierzone tylko dla jednego pacjenta, więc takich nie będziemy uważać za istotne dla całej grupy pacjentów)
j = 1
i = 1
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
    ggsave(file = paste0(i, "_", nazwa, ".png"), plot = wykres$plot)
    i <- i + 1
  }
}

data("COAD.methylation")
data("COADREAD.methylation")
data("GBMLGG.methylation")
data("KIPAN.methylation")
data("KIRC.methylation")
data("KIRP.methylation")
data("LAML.methylation")
data("LUAD.methylation")
data("LUSC.methylation")
data("OV.methylation") #może się nie wczytać, bo jest bardzo duże
data("READ.methylation")
data("STAD.methylation")
data("STES.methylation")
data("UCEC.methylation")
