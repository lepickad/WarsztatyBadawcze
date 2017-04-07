library(foreign)
library(stringi)
library(tidyr)
library(dplyr)

home_path <- "C://Users//Witek//Documents//10sem//pisa//"
row.number <- 0

files <- list.files(paste0(home_path, "cog_data_cntpartition"))
quest_pos <- read.csv(paste0(home_path, "zad_klast_form_poz.csv"))
quest_pos <- quest_pos[,!(colnames(quest_pos) == "klaster")]

qqq <- read.csv(paste0(home_path, "qqq.csv"))

gc()

for(s in files){
  
  print(s)
  dane <- read.csv(paste0(home_path, "cog_data_cntpartition//", s))
  
  #selekcja interesujacych informacji o zadaniach
  # SCORY
  scory <- stri_detect_regex( colnames(dane),"^[CD][SMR][0-9]{3,3}Q[0-9]{2,2}[SC]$")
  scory[4]<- TRUE 
  dane_scory<- dane[,scory]
  
  # TIME 
  czasy <- stri_detect_regex( colnames(dane),"^[CD][SMR][0-9]{3,3}Q[0-9]{2,2}T$")
  czasy[4] <- TRUE 
  dane_czasy <- dane[,czasy]
  
  # Action
  akcje <- stri_detect_regex( colnames(dane),"^[CD][SMR][0-9]{3,3}Q[0-9]{2,2}A$")
  akcje[4]<- TRUE 
  dane_akcje<- dane[,akcje]
  
  dane <- dane[,c(2,3,4,20,21)]
  dane$BOOKID <- as.numeric(unlist(stri_extract_all_regex(dane$BOOKID, "[0-9]{2}")))
  class(dane$BOOKID) <- "numeric"
  
  dane$CBASCI <- as.numeric(unlist(stri_extract_all_regex(dane$CBASCI, "[0-9]")))
  class(dane$CBASCI) <- "numeric"
  
  #tworzymy pare student-zadanie
  dane_scory <- gather(dane_scory,"item", "result", 2:ncol(dane_scory))
  dane_czasy <- gather(dane_czasy,"item", "timing", 2:ncol(dane_czasy))
  dane_akcje <- gather(dane_akcje,"item", "n.actions", 2:ncol(dane_akcje))
  
  
  ##dodajemy zmienna item_short
  dane_scory$item_short <- substr(dane_scory$item,2,8)
  dane_czasy$item_short <- substr(dane_czasy$item,2,8)
  dane_akcje$item_short <- substr(dane_akcje$item,2,8)
  
  #stosuje left_join dla poprawy wydajnosci;
  #ma to sens gdy i tak zakładamy usuniecie wierszy z brakami danych dla ustalonej zmiennej
  
  dane_scory <- left_join(dane_scory, dane_czasy[-2], by = c("CNTSTUID","item_short"))
  dane_scory <- left_join(dane_scory, dane_akcje[-2], by = c("CNTSTUID","item_short"))
  
  dane <- left_join(dane, dane_scory, by = "CNTSTUID")
  dane <- dane[!is.na(dane$result),]
  
  #dodaje pozycje zadan z matematyki i czytania
  dane <- left_join(dane, quest_pos, by = c("item_short","BOOKID"))
  
  #dodaje odpowiedzi z kwestionariusza
  dane <- left_join(dane, qqq, by = "CNTSTUID")
  
  #ujednolicam zmienna 'result' 
  dane$result[stri_detect_fixed(dane$result, "Full")] <- "Full credit"
  dane$result[stri_detect_fixed(dane$result, "No")] <- "No credit"
  dane$result[stri_detect_fixed(dane$result, "Partial")] <- "Partial credit"
  
  s <- paste0(substr(s,1,3),"_sq.csv")
  write.csv(dane,
            file = paste0(home_path,"student_question_cntpart//", s),
            row.names = FALSE)
  
  row.number <- row.number + nrow(dane)
}

#ostateczna liczba obserwacji: 15735947
  