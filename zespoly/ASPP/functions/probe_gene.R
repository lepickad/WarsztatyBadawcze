# The following function concatenates the names of the 2452 significant probes with
# the names of the genes containing them. If a given probe doesn't lie on any gene, 
# the name of the gene is replaced by the "???" expression. 
# 
# function arguments:
# path_signif: path to the "significant.rda" with the significant probe names
# path_meth: path to the "KIRP.methylation" file
# dir_out: directory where the final .rda file is saved


probe_gene <- function(path_signif, path_meth, dir_out){

   library(tidyr)
   
   load(path_signif)
   load(path_meth)
   row2 <- KIRP.methylation[2,2:ncol(KIRP.methylation)]
   row2g <- gather(row2, probe, gene)
   
   for(i in 1:length(row2g$gene)){
      if(is.na(row2g$gene[i])){
         row2g$gene[i] <- "???"
      }
   }
   
   probes <- as.character(row2g$probe)
   genes <- as.character(row2g$gene)
   
   which_signif <- which(probes %in% significant)
   signif_probes <- probes[which_signif]
   signif_genes <- genes[which_signif]
   probe_gene <- paste(signif_probes, signif_genes, sep = "_")
   setwd(dir_out)
   save(probe_gene, file="probe_gene.rda")
   
}

