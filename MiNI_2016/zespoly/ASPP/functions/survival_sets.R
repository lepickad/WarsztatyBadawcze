# The following function creates 16 data frames containing methylation values
# for each of the 2452 significant probes, as well as the status and the time of
# death/censoring of patients suffering from one of the 16 cancer types
# 
# function arguments:
# path_signif: path of the "significant.rda" file
# dir_meth: directory with the processed RTCGA.methylation files
# dir_out: directory where the final .rda files are saved


survival_sets <- function(path_signif, dir_meth, dir_out){
   
   library(stringi)
   
   load(path_signif)
   
   setwd(dir_meth)
   files <- dir()
   for(i in seq_along(files)){
      load(files[i])
   }
   
   extract_signif <- function(set){
      markers <- names(set)
      logic <- markers %in% significant
      n <- which(logic==TRUE | (markers %in% c("status", "time")))
      newSet <- set[, n]
      return(newSet)
   }
   
   names <- unlist(lapply(files, function(x) stri_sub(x, 5, stri_length(x)-4)))
   oldNames <- unlist(lapply(files, function(y) stri_sub(y, 1, stri_length(y)-4)))
   
   for(i in seq_along(oldNames)){
      extracted <- extract_signif(get(oldNames[i]))
      assign(names[i], extracted)
   }
   
   setwd(dir_out)
   
   save(brca, file = "brca.rda")
   save(coad, file = "coad.rda")
   save(coadread, file = "coadread.rda")
   save(gbm, file = "gbm.rda")
   save(gbmlgg, file = "gbmlgg.rda")
   save(kipan, file = "kipan.rda")
   save(kirc, file = "kirc.rda")
   save(kirp, file = "kirp.rda")
   save(laml, file = "laml.rda")
   save(luad, file = "luad.rda")
   save(lusc, file = "lusc.rda")
   save(ov, file = "ov.rda")
   save(read, file = "read.rda")
   save(stad, file = "stad.rda")
   save(stes, file = "stes.rda")
   save(ucec, file = "ucec.rda")
   
}

