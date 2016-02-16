# The following function creates a numeric vector containing the top 200 significant 
# probe names for a given cancer type, along with the responding p-values resulting 
# from a conducted log-rank survival test between two groups of patients, with a 
# median of methylation degree as a threshold value.
#
# function arguments:
# name_meth: the name of the RTCGA.methylation .rda file
# name_cli: the name of the RTCGA.clinical .rda file
# dir_meth: directory of the RTCGA.methylation file
# dir_cli: directory of the RTCGA.clinical file
# dir_out: directory where the final .rda file is saved


signif_pvalues <- function(name_meth, name_cli, dir_meth, dir_cli, dir_out){
   
   library(survMisc)
   
   setwd(dir_meth)
   myenv <- new.env()
   loading <- load(paste0(name_meth, ".rda"), envir = myenv)[1]
   set <- myenv[[loading]]
   
   meth <- set[set$'Composite Element REF' == "Beta_value", ]
   
   probes <- data.frame(id = tolower(substr(rownames(meth), 1, 12)), 
                        sample = tolower(substr(rownames(meth), 14, 15)), 
                        meth[,2:ncol(meth)], 
                        row.names = 1:nrow(meth))
   
   setwd(dir_cli)
   myenv_cli <- new.env()
   cli_loading <- load(paste0(name_cli, ".rda"), envir = myenv_cli)[1]
   set_cli <- myenv_cli[[cli_loading]]
   
   cli <- data.frame(time1=as.numeric(as.character(set_cli$patient.days_to_death)),
                     time2=as.numeric(as.character(set_cli$patient.days_to_last_followup)),
                     status = set_cli$patient.vital_status,
                     barcode = set_cli$patient.bcr_patient_barcode,
                     row.names = 1:nrow(set_cli))
   cli$time <- ifelse(is.na(cli$time1), cli$time2, cli$time1)
   
   all <- merge(probes, cli, by.x = "id", by.y = "barcode")
   n <- ncol(all)
   for(i in 3:(n-4)){
      all[,i] <- as.numeric(as.character(all[,i]))
   }
   
   setwd(dir_out)
   save(all, file=paste0("all_", tolower(gsub("\\..*","", name_meth)), ".rda"))
   
   nprobes <- n-6
   pvalues <- numeric(nprobes)
   for(i in 1:nprobes){
      meth_median <- median(all[,i+2], na.rm = TRUE)
      if(length(unique(all[,i+2])) != 1){
         test <- survdiff(Surv(time, status == "dead")~(all[,i+2]>meth_median), data = all)
         pvalues[i] <- 1 - pchisq(test$chi, 1)   
      } else {
         pvalues[i] <- NA
      }
      if(i %% 100 == 0){
         print(i)
      }
   }
   names(pvalues) <- names(all[,3:(n-4)])
   
   pvalues_s <- sort(pvalues, decreasing = FALSE)
   istotne <- pvalues_s[1:200]
   file_name <- paste0("istotne_", tolower(gsub("\\..*","", name_meth)))
   full_name <- paste0(file_name, ".rda")
   assign(file_name, istotne)
   save(file_name, file = full_name)

}   
 
