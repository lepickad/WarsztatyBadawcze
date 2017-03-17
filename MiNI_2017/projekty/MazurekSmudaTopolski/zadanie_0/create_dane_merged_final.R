library(xlsx)
library(data.table)
library(stringi)
library(dplyr)


sheet_structure = as.data.table(read.xlsx("./data/raw_data/sheet_structure.xlsx", startRow = 1, colIndex = 1:5,sheetIndex = 1,
                                          encoding = "UTF-8"))

colnames(sheet_structure) = c("BOOKID", as.character(1:4))

sheet_structure_melt = melt(sheet_structure, 
     id.vars = "BOOKID", 
     measure.vars = as.character(1:4), 
     value.name = "cluster", 
     variable.name = "position")

sheet_structure_melt[, cluster := stri_trim_both(cluster)]

sheet_structure_melt[, position := as.numeric(position)]


load("./data/processed_data/dane_cluster_all.RDA")
dane_cluster = as.data.table(dane_cluster_all)

science_cluster_encoding = read.xlsx("./data/raw_data/science_cluster_encoding.xlsx", sheetIndex = 1, header = TRUE)
colnames(science_cluster_encoding)[6:7] = c("X6", "X7")
science_cluster_encoding_2 = tidyr::gather(science_cluster_encoding, CBASCI, combination, -Base) 
science_cluster_encoding_2$CBASCI = stri_sub(science_cluster_encoding_2$CBASCI, from=2)
colnames(science_cluster_encoding_2) = c("BOOKID","CBASCI","combination")
science_cluster_encoding_2$BOOKID = as.numeric(science_cluster_encoding_2$BOOKID)
science_cluster_encoding_2$CBASCI =  as.numeric(science_cluster_encoding_2$CBASCI)
science_cluster_encoding_2 = as.data.table(science_cluster_encoding_2)

science_cluster_combination = read.xlsx("./data/raw_data/science_cluster_combination.xlsx", sheetIndex = 1, header = TRUE)
colnames(science_cluster_combination) = c("combination", "1", "2")
science_cluster_combination_2 = tidyr::gather(science_cluster_combination, position_s, cluster, -combination)
science_cluster_combination_2$cluster = stri_replace_first_regex(science_cluster_combination_2$cluster, "(?<=S)0","")
science_cluster_combination_2 = as.data.table(science_cluster_combination_2)


load("./data/processed_data/dane_pisa_filtered.RDA")
setkey(dane_pisa_filtered, BOOKID, task, CBASCI)
setkey(dane_cluster, task)
setkey(science_cluster_encoding_2, BOOKID, CBASCI)
dane_pisa_merged = merge(dane_pisa_filtered, dane_cluster, all.x = TRUE)
rm(dane_pisa_filtered)
dane_pisa_merged = as.data.table(dane_pisa_merged)

setkey(dane_pisa_merged,BOOKID, task, CBASCI)
dane_pisa_merged_2 = merge(dane_pisa_merged, science_cluster_encoding_2, all.x = TRUE)
rm(dane_pisa_merged)
setkey(sheet_structure_melt, BOOKID, cluster)
dane_pisa_merged_3 = merge(dane_pisa_merged_2, sheet_structure_melt, all.x = T, by = c("BOOKID", "cluster"))
rm(dane_pisa_merged_2)
setkey(dane_pisa_merged_3, cluster, combination)
setkey(science_cluster_combination_2, cluster, combination)
dane_pisa_merged_final = merge(dane_pisa_merged_3, science_cluster_combination_2, all.x = T)
rm(dane_pisa_merged_3)

dane_pisa_merged_final[, position:= ifelse(is.na(position), position_s, position)]
dane_pisa_merged_final = dane_pisa_merged_final[, -c(2,13), with = FALSE]
dane_pisa_merged_final[, result := ifelse(result %in% c(0,1), result, NA)]
dane_pisa_merged_final[, n.actions := as.integer(n.action)]
dane_pisa_merged_final[, n.action := NULL]
setnames(dane_pisa_merged_final, "task", "item")
dane_pisa_merged_final[, item_short := stri_sub(item, from  = 2)]

load("stud2015.rda")
stud2015 <- as.data.table(stud2015)
stud2015 <- stud2015[ADMINMODE=="Computer"]
stud2015_2 <- stud2015[, c("CNTSTUID",
                           "ST004D01T","ST118Q01NA","ST118Q02NA",
                           "ST118Q03NA","ST118Q04NA","ST118Q05NA"), with = FALSE]

stud2015_2$ST004D01T = as.character(stud2015_2$ST004D01T)
stud2015_2$ST118Q01NA = as.character(stud2015_2$ST118Q01NA)
stud2015_2$ST118Q02NA = as.character(stud2015_2$ST118Q02NA)
stud2015_2$ST118Q03NA = as.character(stud2015_2$ST118Q03NA)
stud2015_2$ST118Q04NA = as.character(stud2015_2$ST118Q04NA)
stud2015_2$ST118Q05NA = as.character(stud2015_2$ST118Q05NA)

dane_pisa_merged_final <- dane_pisa_merged_final[, c(5:7, 2:4, 12, 9, 11, 8, 10, 1)]
dane_pisa_merged_final$position <- as.numeric(dane_pisa_merged_final$position)

dane_pisa_merged_final <- left_join(dane_pisa_merged_final, stud2015_2, by = "CNTSTUID")
save(dane_pisa_merged_final, file = "./data/processed_data/dane_pisa_merged_final.RDA")
write.csv(dane_pisa_merged_final[1:100,], file = "./data/processed_data/zadanie0_sample.csv", 
          row.names = FALSE)
