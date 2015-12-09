#
# Pobieramy dane o ekspresji genów i o cechach klinicznych
#
library(RTCGA.clinical)
library(RTCGA.rnaseq)
data("BRCA.rnaseq")
data("BRCA.clinical")

#
# Sklejamy dane z obu eksperymentów
#
brca <- BRCA.rnaseq[-1,]

grep(brca[,1], pattern = "MDM", value = TRUE)

which(brca[,1] == "MDM2|4193")

tylkoMDM <- brca[10744,-1]

rnaS <- data.frame(sampID = tolower(substr(names(tylkoMDM), 1, 12)), 
                   val = as.numeric(as.character(t(tylkoMDM))))

cliS <- data.frame(time1=as.numeric(as.character(BRCA.clinical$patient.days_to_death)),
      time2=as.numeric(as.character(BRCA.clinical$patient.days_to_last_followup)),
     status = BRCA.clinical$patient.vital_status,
     barcode = BRCA.clinical$patient.bcr_patient_barcode)
cliS$time <- ifelse(is.na(cliS$time1), cliS$time2, cliS$time1)

head(rnaS)
head(cliS)

allBRCA <- merge(rnaS, cliS, by.x = "sampID", by.y = "barcode")

#
# Wykonujemy prostą analizę przeżycia
#

library(ggplot2)
library(survMisc)

ob1 <- survfit(Surv(time, status == "dead")~1, data=allBRCA)
autoplot(ob1)

survdiff(Surv(time, status == "dead")~(val>1925), data=allBRCA)

ob2 <- survfit(Surv(time, status == "dead")~(val>1925), data=allBRCA)
autoplot(ob2)

