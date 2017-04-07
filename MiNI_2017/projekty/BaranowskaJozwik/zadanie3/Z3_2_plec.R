setwd("C:/Users/E540/Desktop/PISA 2015/zadanie0")
load("Z0_matem_w.rda")
load("Z0_read_w.rda")
load("Z0_science_w.rda")
GII <- read.csv2("GII.csv", header = T, stringsAsFactors = F) 
memory.limit(200000)


library(jsonlite)


daneDoWykresowPlec <- function(zbior, indeks_GII){
   
   require(dplyr)
   require(tidyr)
   
   # statystyki ogolne poprawnych odpowiedzi w podziale na plec   
   zbior %>% group_by(ST004D01T) %>%
      summarise( ProcFullCredit = sum( W_FSTUWT[result == 3] ) * 100 /  sum(W_FSTUWT)) -> total
   
   
   
   # statystyki poprawnych odpowiedzi w podziale na plec per kraj
   zbior %>% group_by(CNT, ST004D01T) %>% 
      summarise( ProcFullCredit = sum( W_FSTUWT[result == 3] ) * 100 / sum(W_FSTUWT)) %>% spread(ST004D01T,ProcFullCredit) -> per_kraj
   
   colnames(per_kraj) <-c("CNT", "F", "M") 
   
   # kolumna odpowiadajaca za roznice miedzy plciami
   per_kraj$ile_roznicy <- per_kraj$F -per_kraj$M
   
   ######################## obliczanie  ważonych procentów dla każdego kraju
   
   # liczenie dla kolumn wagowych sumy wag
   zbior %>% select(-CNTSCHID, -CNTSTUID, -BOOKID, -klaster, -item_short, -CBASCI, -ST118Q01NA, -ST118Q02NA, -ST118Q03NA,
                    -ST118Q04NA, -ST118Q05NA, -item, -n.actions, -timing, -pozycja, -result) %>% group_by(CNT, ST004D01T) %>% 
      summarise_each(funs(sum)) %>% 
      gather(key=wagi, value=sumy_razem,-CNT, -ST004D01T)-> wazone
   
   # liczenie dla kolumn wagowych sumy wag dla poprawnych odpowiedzi
   zbior %>% filter(result == 3) %>%
      select(-CNTSCHID, -CNTSTUID, -BOOKID, -klaster, -item_short, -CBASCI, -ST118Q01NA, -ST118Q02NA, -ST118Q03NA,
                    -ST118Q04NA, -ST118Q05NA, -item, -n.actions, -timing, -pozycja, -result) %>% group_by(CNT, ST004D01T) %>%
      summarise_all( funs(sum) ) %>%
      gather(key=wagi, value=sumy_czesc,-CNT, -ST004D01T)-> wazone2
   
   wazone <- merge(wazone2, wazone, by=c("CNT", "ST004D01T", "wagi"))
   
   # liczenie procentu ważonego dla kazdego panstwa, plci i wagi
   wazone %>% group_by(CNT, ST004D01T, wagi) %>% summarise(procent = sumy_czesc*100/sumy_razem
                                                           ) %>% spread(ST004D01T,procent) -> wazone
   
   colnames(wazone) <-c("CNT","wagi", "F", "M") 
   
   # liczenie roznicy procentu miedzy plciami dla kazdego kraju i wagi
   wazone %>% group_by(CNT, wagi) %>% mutate( roznica = F - M) -> wazone
   
   # obliczanie odchylenia standardowego na podstawie 80 wag
   wazone %>% group_by(CNT) %>% summarise(
      
      std = sqrt((1/20)*sum( (roznica[wagi=="W_FSTUWT"] - roznica )^2  ))
      
   ) -> wazone

   # dodanie odchylenia do glownego zbioru
   per_kraj <- merge(per_kraj, wazone,all.x=T, by=c("CNT"))
   rm(wazone)
   
   # obliczanie statystyki testowej 
   # sprawdzanie istotności testu na poziomie 0.05 (dwustronny)
   per_kraj %>% mutate(z= ile_roznicy/std,
                       czyIstotny = (z < qnorm(0.025)) | (z  > qnorm(0.975) ) ) %>%
      select(-z,-std, -ile_roznicy) -> per_kraj
   

   # dolaczanie danych z indeksem GII  
   indeks_GII$GII <- as.numeric(indeks_GII$GII)
   indeks_GII$GII <- ifelse(is.na(indeks_GII$GII),0,indeks_GII$GII) # wstawiamy 0 za NA
   
   per_kraj<- merge(per_kraj, indeks_GII, by=c("CNT"))
   
   
   return(list(total=total, dane=per_kraj))
}

#### dane matematyczne
daneMatem <- daneDoWykresowPlec(Z0_matem, GII)
rm(Z0_matem)
# zapis do struktur JavaScriptowych
jsonik <- toJSON(daneMatem$dane)
total <- toJSON(daneMatem$total$ProcFullCredit)
   
#### dane czytelnicze
daneRead <- daneDoWykresowPlec(Z0_read, GII)
rm(Z0_read)
# zapis do struktur JavaScriptowych
jsonik <- toJSON(daneRead$dane)
total <- toJSON(daneRead$total$ProcFullCredit)

#### dane matematyczne
daneScience <- daneDoWykresowPlec(Z0_science, GII)
rm(Z0_science)
# zapis do struktur JavaScriptowych
jsonik <- toJSON(daneScience$dane)
total <- toJSON(daneScience$total$ProcFullCredit)



 