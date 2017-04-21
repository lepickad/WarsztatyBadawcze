# makrem Z0_QQQ_wagi.sas tworzę zbiór z wagami

############################################################################
################ dodawanie wag do zbioru
############################################################################
setwd("C:/Users/E540/Desktop/PISA 2015/zadanie0")
load("Z0_matem.rda")
load("Z0_read.rda")
load("Z0_science.rda")
wagi <- read.csv("CY6_MS_CMB_STU_QQQ_wagi.csv")
memory.limit(200000)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # writeWorkbook

Z3_statystyki <- function(zbior_in = Z0_matem, zbior_wagi=wagi, stat_out = "stat_matem.xlsx"){
   
   require(dplyr)
   require(isotone) # do wazonej mediany
   require(openxlsx)
   require(data.table)
   
   # dolaczanie wag do zbioru i zapis danych wraz z wagami 
   zbior_in <- data.table(zbior_in)
   zbior_wagi <- data.table(zbior_wagi)
   setkey(zbior_in, CNTSCHID, CNTSTUID)
   setkey(zbior_wagi, CNTSCHID, CNTSTUID)
   zbior_in <- as.data.frame(zbior_wagi[zbior_in])
   cat("Zmergowano zbior z wagami","\n")
   
   
   
   # statystyki dla calego pytania
   
   zbior_in %>% group_by(item_short) %>% summarise( suma_pytanie = sum(W_FSTUWT),
                                                    ProcFullCredit = sum( W_FSTUWT[result == 3] ) * 100 / suma_pytanie,
                                                    ProcNoCredit = sum( W_FSTUWT[result == 1] )  * 100 / suma_pytanie,
                                                    N = n(),
                                                    TimeAvgSek = weighted.mean(timing/60000, W_FSTUWT),
                                                    TimeMedSek= weighted.median(timing/60000, W_FSTUWT),
                                                    TimeMinSek = min(timing)/60000,
                                                    TimeMaxSek = max(timing)/60000
   )  %>%
      select(-suma_pytanie) -> per_pytanie
   
   # dodanie kolumnt CNT = "TOTAL"
   per_pytanie <- cbind(CNT="TOTAL",per_pytanie)
   per_pytanie$CNT <- as.character(per_pytanie$CNT)
   
   # statystyki per pytanie per kraj
   
   zbior_in %>% group_by(CNT, item_short) %>% summarise( suma_pytanie = sum(W_FSTUWT),
                                                         ProcFullCredit = sum( W_FSTUWT[result == 3] ) * 100 / suma_pytanie,
                                                         ProcNoCredit = sum( W_FSTUWT[result == 1] )  * 100 / suma_pytanie,
                                                         N = n(),
                                                         TimeAvgSek = weighted.mean(timing/60000, W_FSTUWT),
                                                         TimeMedSek= weighted.median(timing/60000, W_FSTUWT),
                                                         TimeMinSek = min(timing)/60000,
                                                         TimeMaxSek = max(timing)/60000
   ) %>% select(-suma_pytanie) %>% 
      arrange(item_short, desc(ProcFullCredit))-> per_kraj
   
   # zapis statystyk do pliku 
   pytania <- sort(unique(zbior_in$item_short))
   n_pytania <- length(pytania)
   
   wb <- createWorkbook()
   
   for(i in 1:n_pytania){
      cat("Zapisano ", i, "/", n_pytania, "pytan \n")
      dane <- bind_rows(per_pytanie %>% filter(item_short == pytania[i]),
                        per_kraj %>% filter(item_short == pytania[i]))
      addWorksheet(wb, pytania[i])
      writeData(wb, sheet = pytania[i], x = dane)
   }
   
   saveWorkbook(wb, file=stat_out, overwrite = FALSE)
   
   
   return(zbior_in)
   
}

Z0_matem <- Z3_statystyki(zbior_in = Z0_matem, zbior_wagi = wagi, stat_out = "stat_matem_3.xlsx")
save(Z0_matem, file="Z0_matem_w.rda")
rm(Z0_matem)

Z0_read <- Z3_statystyki(zbior_in = Z0_read, zbior_wagi = wagi, stat_out = "stat_read_3.xlsx")
save(Z0_read, file="Z0_read_w.rda")
rm(Z0_read)

Z0_science <- Z3_statystyki(zbior_in = Z0_science, zbior_wagi = wagi, stat_out = "stat_science_3.xlsx")
save(Z0_science, file="Z0_science_w.rda")
rm(Z0_science)