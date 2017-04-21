library(readxl)   
library(dplyr)
library(stringi)
library(jsonlite)
setwd("C:/Users/E540/Desktop/PISA 2015/zadanie0")

# funkcja do wczytywania wszystkich arkuszy excela w jedną ramkę danych
read_excel_allsheets <- function(filename) {
   
   sheets <- excel_sheets(filename)
   x <-    do.call("rbind",
                   lapply(sheets, function(X) read_excel(filename, sheet = X))
   )

   return(x)
   
}

# pliki ze statystykami
stat_matem <- read_excel_allsheets("stat_matem_3.xlsx")
stat_read <- read_excel_allsheets("stat_read_3.xlsx")
stat_science <- read_excel_allsheets("stat_science_3.xlsx")

#  wczytywanie nazw pytan z Codebook
codebook <- read_excel("Codebook_CMB.xlsx", sheet="Cognitive")
codebook %>% filter(!is.na(NAME)) %>% mutate(NAME = substr(NAME,2,8)) %>% 
   select(NAME, VARLABEL)-> codebook

# usuwanie śmieci z nazw pytań
for(slowo in c("(Coded Response)", "(Timing)", "(Number of Actions)","(Scored Response)","(Raw Response)",
               "(Raw Paper Response)", "(Paper Scored Response)", "[Part A]", " [Part B]", " [Part C]", 
               " [Part D]", "(Coded Paper Response)" ,  "[Part E]", 
               " [Part F]", " [Part G]", "[Part H]", " [Part I]",
               "(Number of Trials)", "(Number of Selected Rows)", "(Number of Rows)", " (All Rows Correct)",
               "[1-L2]", "[1-L1]", " [2-L1]", "[3-L3]", "[2-L2]", "[1-L3]",
               " (Solar Panels) - Q02", " (Solar Panels) - Q07", " (Solar Panels) - Q08")){
   
   codebook$VARLABEL <- stri_replace_all_fixed(codebook$VARLABEL,
                                               pattern = slowo, "")

}

codebook$VARLABEL <- stri_replace_all_regex(codebook$VARLABEL,
                                            pattern = "[ABCDE] $", "")

codebook$VARLABEL <- trimws(codebook$VARLABEL)
codebook %>% distinct(NAME, VARLABEL) -> codebook
colnames(codebook) <- c("item_short", "VARLABEL")


# funkcja do tworzenia potrzebnych zmiennych
doWykresuPytania <- function(zbior, codebook){
   
   # licze odchylenie standardowe dla proc popr. odpow
   zbior %>% filter(CNT != "TOTAL") %>% group_by(item_short) %>% summarise(std =sqrt(var(ProcFullCredit))) -> odch_stand
   
   # licze pozycje Polski w kazdym pytaniu
   zbior %>% filter(CNT != "TOTAL") %>% group_by(item_short) %>% arrange(item_short,desc(ProcFullCredit)) %>%
            mutate(rankingWPytaniu = 1:n()) %>% filter(CNT == "POL") %>% select(item_short,rankingWPytaniu) %>%
      rename(rankingPolska =  rankingWPytaniu )-> top_Polska
   
   # wyciagam top3 krajów dla każdego pytania
   zbior %>% filter(CNT != "TOTAL") %>% group_by(item_short) %>% arrange(item_short,desc(ProcFullCredit)) %>%
      mutate(rankingWPytaniu = 1:n(),
             top1 = CNT[ rankingWPytaniu == 1],
             top2 = CNT[ rankingWPytaniu == 2],
             top3 = CNT[ rankingWPytaniu == 3]) %>% distinct(top1, top2, top3)-> top3
   
   # wybieram tylko ogolne statystyki
   zbior %>% filter(CNT == "TOTAL") %>% select(item_short, ProcFullCredit, N, TimeAvgMin) -> zbior
   
   # dodaje nazwe pytania
   zbior <- merge(zbior, codebook, all.x = T, by=c("item_short"))
   
   # dodaje poprzednio obliczone zmienne
   zbior <- merge(zbior, odch_stand, all.x = T, by=c("item_short"))
   zbior <- merge(zbior, top_Polska, all.x = T, by=c("item_short"))
   zbior <- merge(zbior, top3, all.x = T, by=c("item_short"))

   return(zbior)
   
}

daneMatem <- doWykresuPytania(stat_matem,codebook)
daneRead <- doWykresuPytania(stat_read,codebook)
daneScience <- doWykresuPytania(stat_science,codebook)

daneMatem <- toJSON(daneMatem)
daneRead <- toJSON(daneRead)
daneScience <- toJSON(daneScience)

#save(daneMatem, file="daneMatem_pytania.rda")

