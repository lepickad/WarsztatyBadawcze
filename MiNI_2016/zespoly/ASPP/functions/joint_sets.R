# The following function creates a data frame containing three columns:
# marker: names of probes from the set of 2452 significant probes used in the analysis
# methylation: degree of methylation for a given probe
# cancer: cancer type abbreviation
#
# function arguments:
# dir_in: path to the directory containing processed RTCGA.methylation files
# dir_joint: path to the directory where the files created by the "divide" function are saved
# dir_out: path to the directory where the final .rda file is saved
# path signif: path to the "significant.rda" file (with the significant probe names)


joint_sets <- function(dir_meth, dir_joint, dir_out, path_signif){
   
   library(tidyr)
   library(stringi)
   
   setwd(dir_meth)
   allNames <- dir()
   for(i in 1:length(allNames)){
      load(allNames[i])
      print(i)
   }
   
   load(path_signif)
   
   divide <- function(set){
      markers <- names(set)
      logic <- markers %in% significant
      n <- which(logic==TRUE)
      newSet <- set[,n]
      newSetg <- gather(newSet)
      return(newSetg)
   }
   
   names <- unlist(lapply(allNames, function(x) stri_sub(x, 5, stri_length(x)-4)))
   newNames <- unlist(lapply(names, function(y) stri_paste(y, "_signif")))
   oldNames <- unlist(lapply(allNames, function(x) stri_sub(x, 1, stri_length(x)-4)))
   
   signifSets <- list()
   for(i in seq_along(allNames)){
      signifSets[[i]] <- divide(get(oldNames[i]))
      assign(newNames[i], signifSets[[i]])
   }
   
   setwd(dir_joint)
   
   save(brca_signif, file = "brca_signif.rda")
   save(coad_signif, file = "coad_signif.rda")
   save(coadread_signif, file = "coadread_signif.rda")
   save(gbm_signif, file = "gbm_signif.rda")
   save(gbmlgg_signif, file = "gbmlgg_signif.rda")
   save(kipan_signif, file = "kipan_signif.rda")
   save(kirc_signif, file = "kirc_signif.rda")
   save(kirp_signif, file = "kirp_signif.rda")
   save(laml_signif, file = "laml_signif.rda")
   save(luad_signif, file = "luad_signif.rda")
   save(lusc_signif, file = "lusc_signif.rda")
   save(ov_signif, file = "ov_signif.rda")
   save(read_signif, file = "read_signif.rda")
   save(stad_signif, file = "stad_signif.rda")
   save(stes_signif, file = "stes_signif.rda")
   save(ucec_signif, file = "ucec_signif.rda")
   
   signifNames <- dir()
   for(i in seq_along(signifNames)){
      load(signifNames[i])
      print(i)
   }
   
   data <- data.frame()
   for(i in seq_along(signifNames)){
      assign("set", get(newNames[i]))
      data <- rbind(data, set)
      print(i)
   }
   
   cancer_signif <- list()
   for(i in seq_along(signifNames)){
      assign("set", get(newNames[i]))
      n <- nrow(set)
      cancer_signif[[i]] <- rep(newNames[i], n)
   }
   cancer <- unlist(cancer_signif)
   data$set <- cancer
   
   key_sort <- data[order(data$key),]
   names(key_sort) <- c("marker", "methylation", "cancer")
   data2 <- key_sort
   rownames(data2) <- 1:nrow(data2)
   data2$cancer <- unlist(stri_extract_all_regex(as.character(data2$cancer), ".+(?=_)"))
   data2$cancer <- stri_trans_toupper(data2$cancer)
   setwd(dir_out)
   save(data2, file="data2.rda")
   
}

