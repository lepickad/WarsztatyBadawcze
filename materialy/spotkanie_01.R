library(devtools)
install_github("RTCGA/RTCGA.clinical")

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
                   sample = tolower(substr(names(tylkoMDM), 14, 15)), 
                   val = as.numeric(as.character(t(tylkoMDM))))

cliS <- data.frame(time1=as.numeric(as.character(BRCA.clinical$patient.days_to_death)),
      time2=as.numeric(as.character(BRCA.clinical$patient.days_to_last_followup)),
     status = BRCA.clinical$patient.vital_status,
     barcode = BRCA.clinical$patient.bcr_patient_barcode)
cliS$time <- ifelse(is.na(cliS$time1), cliS$time2, cliS$time1)

head(rnaS)
head(cliS)

allBRCA <- merge(rnaS, cliS, by.x = "sampID", by.y = "barcode")

library(tidyr)
allBRCA2 <- spread(allBRCA, key = sample, value = val)

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

#
# rasa i terapia

cliS <- data.frame(time1=as.numeric(as.character(BRCA.clinical$patient.days_to_death)),
                   time2=as.numeric(as.character(BRCA.clinical$patient.days_to_last_followup)),
                   status = BRCA.clinical$patient.vital_status,
                   barcode = BRCA.clinical$patient.bcr_patient_barcode,
                   race=BRCA.clinical$patient.race,
                   therapy=BRCA.clinical$patient.drugs.drug.therapy_types.therapy_type)
cliS$time <- ifelse(is.na(cliS$time1), cliS$time2, cliS$time1)

allBRCA <- merge(rnaS, cliS, by.x = "sampID", by.y = "barcode")

ob <- survfit(Surv(time, status == "dead")~therapy, data=allBRCA)
autoplot(ob)$plot + ylim(0.7,1) + xlim(0,3000)

ob <- survfit(Surv(time, status == "dead")~race, data=allBRCA)
autoplot(ob)$plot + ylim(0.7,1) + xlim(0,3000)
