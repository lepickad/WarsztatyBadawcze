# The following function creates six character vectors containing the names of
# the following probes:
# significant: 2452 probes significant to at least one cancer type
# significant1: probes significant to exactly 1 cancer type
# significant2: probes significant to exactly 2 cancer types
# significant3: probes significant to exactly 3 cancer types
# significant4: probes significant to exactly 4 cancer types
# significant5: probes significant to exactly 5 cancer types
#
# function arguments:
# dir_signif: directory with the "istotne_(...).rda" files
# dir_out: directory where the final .rda files are saved


significant <- function(dir_signif, dir_out){
   
   setwd(dir_in)
   
   n <- dir()
   for (i in 1:16){
      load(n[i])
   }
   
   brca <- names(istotne_brca)
   coad <- names(istotne_coad)
   coadread <- names(istotne_coadread)
   gbm <- names(istotne_gbm)
   gbmlgg <- names(istotne_gbmlgg)
   kipan <- names(istotne_kipan)
   kirc <- names(istotne_kirc)
   kirp <- names(istotne_kirp)
   laml <- names(istotne_laml)
   luad <- names(istotne_luad)
   lusc <- names(istotne_lusc)
   ov <- names(istotne_ov)
   read <- names(istotne_read)
   stad <- names(istotne_stad)
   stes <- names(istotne_stes)
   ucec <- names(istotne_ucec)
   
   all <- c(brca, coad, coadread, gbm, gbmlgg, kipan, kirc, kirp, laml, luad, 
            lusc, ov, read, stad, stes, ucec) 
   all_s <- sort(all)
   r <- rle(all_s)
   
   significant <- r$values
   significant1 <- r$values[which(r$lengths == 1)]
   significant2 <- r$values[which(r$lengths == 2)]
   significant3 <- r$values[which(r$lengths == 3)]
   significant4 <- r$values[which(r$lengths == 4)]
   significant5 <- r$values[which(r$lengths == 5)]
   
   setwd(dir_out)
   save(significant, file = "significant.rda")
   save(significant1, file = "significant1.rda")
   save(significant2, file = "significant2.rda")
   save(significant3, file = "significant3.rda")
   save(significant4, file = "significant4.rda")
   save(significant5, file = "significant5.rda")
   
}

