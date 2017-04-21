library(foreign)
library(stringi)
library(tidyr)

################## pocięcie danych o zadaniach ze względu na kraje ################## 

gc()
dane <- read.spss("/home/samba/kocinskiw/warsztaty_pisa/Cy6_ms_cmb_stu_cog.sav",
                 use.value.labels = TRUE, to.data.frame = TRUE)

papier <- stri_detect_regex( colnames(dane),"^P")
dane <- dane[,!papier]

path <- "/home/samba/kocinskiw/warsztaty_pisa/cog_data_cntpartition/"
ext <- ".csv"

for(s in unique(dane$CNT)){
  ind <- which(dane$CNT == s)
  file <- path %s+% stri_trim(s) %s+% ext 
  write.csv(dane[ind,], file = file, row.names = FALSE)
}

################## selekcja danych z kwestionariusza ################## 

remove(dane)
dane<- read.spss(paste0(home_path,"Cy6_ms_cmb_stu_qqq.sav"),
                 use.value.labels = TRUE, to.data.frame = TRUE)

odp <- stri_detect_regex(colnames(dane), "ST118")
odp[c(which(colnames(dane) == "CNTSTUID"), which(colnames(dane) == "ST004D01T"))] <- TRUE
dane <- dane[,odp]
write.csv(dane, 
          file = paste0(home_path, "qqq.csv"),
          row.names = FALSE)