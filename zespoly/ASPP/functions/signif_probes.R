# The following function creates a data frame with two columns:
# probe: names of the top 200 significant probes for each of the 16 cancer types
# cancer: cancer type abbreviations
# 
# function arguments:
# dir_signif: directory with the "istotne_(...).rda" files
# dir_out: directory where the final .rda file is saved


signif_probes <- function(dir_signif, dir_out){

   setwd(dir_in)
   all_files <- list.files(dir_in)
   
   loaded <- list()
   for(i in seq_along(all_files)){
      loaded[[i]] <- get(load(all_files[i]))
   }
   
   cancerNames <- c("brca", "coad", "coadread", "gbm", "gbmlgg", "kipan", "kirc", 
                    "kirp", "laml", "luad", "lusc", "ov", "read", "stad", "stes", 
                    "ucec")
   cancerProbes <- list() 
   for(i in seq_along(cancerNames)){
      probeNames <- names(loaded[[i]])
      cancerProbes[[i]] <- data.frame(probe = probeNames, cancer = cancerNames[i])
   }
   signif_probes <- do.call("rbind", cancerProbes)
   
   setwd(dir_out)
   save(signif_probes, file="signif_probes.rda")

}

