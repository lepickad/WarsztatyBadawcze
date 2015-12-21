
signif <- function(nazwa_met, nazwa_kli, kat_wej, kat_wej_kli, kat_wyj){
   
   library(survMisc)
   
   setwd(kat_wej)
   myenv <- new.env()
   ladowanie <- load(paste0(nazwa_met, ".rda"), envir = myenv)[1]
   zbior <- myenv[[ladowanie]]
   
   met <- zbior[zbior$'Composite Element REF' == "Beta_value", ]
   
   sondy <- data.frame(id = tolower(substr(rownames(met), 1, 12)),
                       sample = tolower(substr(rownames(met), 14, 15)),
                       met[,2:ncol(met)],
                       row.names = 1:nrow(met))
   
   setwd(kat_wej_kli)
   myenv_cli <- new.env()
   cli_ladowanie <- load(paste0(nazwa_kli, ".rda"), envir = myenv_cli)[1]
   zbior_cli <- myenv_cli[[cli_ladowanie]]
   
   cli <- data.frame(time1=as.numeric(as.character(zbior_cli$patient.days_to_death)),
                     time2=as.numeric(as.character(zbior_cli$patient.days_to_last_followup)),
                     status = zbior_cli$patient.vital_status,
                     barcode = zbior_cli$patient.bcr_patient_barcode,
                     row.names = 1:nrow(zbior_cli))
   cli$time <- ifelse(is.na(cli$time1), cli$time2, cli$time1)
   
   all <- merge(sondy, cli, by.x = "id", by.y = "barcode")
   for(i in 3:27580){
      all[,i] <- as.numeric(as.character(all[,i]))
   }
   setwd(kat_wyj)
   save(all, file=paste0("all_", tolower(gsub("\\..*","", nazwa_met)), ".rda"))
   
   n <- ncol(all)
   liczba_sond <- n-6
   pwartosci <- numeric(liczba_sond)
   for(i in 1:liczba_sond){
      mediana <- median(all[,i+2], na.rm = TRUE)
      if(length(unique(all[,i+2])) != 1){
         test <- survdiff(Surv(time, status == "dead")~(all[,i+2]>mediana), data = all)
         pwartosci[i] <- 1 - pchisq(test$chi, 1)   
      } else {
         pwartosci[i] <- NA
      }
      if(i %% 100 == 0){
         print(i)
      }
   }
   names(pwartosci) <- names(all[,3:27580])
   
   save(pwartosci, file=paste0("pwartosci_", tolower(gsub("\\..*","", nazwa_met)), ".rda"))
   
   pwartosci_s <- sort(pwartosci, decreasing = FALSE)
   istotne <- pwartosci_s[1:200]
   save(istotne, file=paste0("istotne_", tolower(gsub("\\..*","", nazwa_met)), ".rda"))
}   
 

signif("KIRP.methylation", "KIRP.clinical", 
       "C:\\Dane\\Pawel\\PW\\WarsztatyB\\pliki_rda\\methylation",
       "C:\\Dane\\Pawel\\PW\\WarsztatyB\\pliki_rda\\clinical",
       "C:\\Dane\\Pawel\\PW\\WarsztatyB\\pliki_rda\\kirp_test")
