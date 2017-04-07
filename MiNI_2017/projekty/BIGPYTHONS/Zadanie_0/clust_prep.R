#install.packages("readxl")
library(readxl)
library(stringi)
library(tidyr)
library(dplyr)

gc()

## 1. rozważenie formularzy 55-60 pozwala przyporzadkowac zadaniom odpowiednie klastry
##    dla zadan z mateamtyki i czytania

############################### --- 1 --- ###############################

############## odpowiednie scalenie plikóW .csv dla poszczególnych krajóW ############## 

#uwaga: w pliku data_cut.R tworze wczytywane ponizej pliki. 
#       Już tam odrzucone zostają zadania robione na papierze

home_path <- "C://Users//Witek//Documents//10sem//pisa//cog_data_cntpartition//"
fl <- list.files(home_path)

tmp.data1 <- read.csv(paste0(home_path,fl[1]))

#czytanie lub matematyka
col_ind <- stri_detect_regex(names(tmp.data1), "^[CD][RM][0-9]{3}Q[0-9]{2}[SC]{1}$")
col_ind[which(names(tmp.data1) == "BOOKID")] <- TRUE

tmp.data1 <- tmp.data1[,col_ind]
for(f in fl[2:length(fl)]){
  tmp.data2 <- read.csv(paste0(home_path, f))
  tmp.data2 <- tmp.data2[,col_ind]
  tmp.data1 <- rbind(tmp.data1,tmp.data2)
}
write.csv(tmp.data1,
          file = "C://Users//Witek//Documents//10sem//pisa//cog.csv",
          row.names = FALSE)

remove(tmp.data1, tmp.data2)

############## wlasciwe wykonanie pkt. 1 ############## 

home_path <- "C://Users//Witek//Documents//10sem//pisa//"          
dane <- read.csv(paste0(home_path,"cog.csv")) # " ^. ... " 180 zm
#zakładam ze dane zawieraja 180 zm.: BOOKID | scory zadan z matematyki i czytania

forms_names <- sapply(55:60, function(s) paste0("Form ", s, " (CBA)"))

zadM <- list(1)
zadR <- list(1)

for(i in seq_along(forms_names)){
  
  print(i)
  
  r.ind <- which(dane$BOOKID == forms_names[i])
  c.ind <- sapply(colnames(dane), function(c) !all(is.na(dane[r.ind,c])))
  question.names <- colnames(dane[,c.ind])
  
  zadM[[i]] <- question.names[stri_detect_regex(question.names, "^.M")]
  zadR[[i]] <- question.names[stri_detect_regex(question.names, "^.R")]

}

sum(sapply(zadR, length)) + sum(sapply(zadM, length)) 
#178 => jedno zadanie nigdzie nie trafiło

klasterM <- c("M01", "M02", "M03", "M04", "M05", "M06ab")
klasterR <- c("R01", "R02", "R03", "R04", "R05", "R06ab")

remove(dane)

for(i in seq_along(zadM)){
  zadM[[i]] <- data.frame(item_short = zadM[[i]], klaster = klasterM[i], stringsAsFactors = FALSE)
  zadR[[i]] <- data.frame(item_short = zadR[[i]], klaster = klasterR[i], stringsAsFactors = FALSE)
}

zadanie_klaster <- rbind(do.call(rbind, zadM), do.call(rbind, zadR))
zadanie_klaster$item_short <- substr(zadanie_klaster$item_short, 2, 8)

############################### --- 2 --- ###############################

form_clust <- read_excel(paste0(home_path,"sheet_structure.xlsx"))
names(form_clust)[1] <- "BOOKID"
form_clust <- gather(form_clust, "position", "klaster", 2:5)
form_clust$klaster <- stri_trim(form_clust$klaster)
form_clust$position <- as.numeric(unlist(stri_extract_all_regex(form_clust$position, "[0-9]")))

final_tab <- left_join(zadanie_klaster, form_clust,
                       by = "klaster")

write.csv(final_tab, 
          file = paste0(home_path, "zad_klast_form_poz.csv"),
          row.names = FALSE)




