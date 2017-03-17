library(data.table)

#stud2015 <- read.spss("CY6_MS_CMB_STU_QQQ.sav", use.value.labels = TRUE, to.data.frame = TRUE)
#save(stud2015, file = "stud2015.rda")
load("stud2015.rda")
dane <- fread("test.csv")
col_nam <- colnames(dane)

stud2015 <- as.data.table(stud2015)
stud2015 <- stud2015[ADMINMODE=="Computer"]
dane <- dane[ADMINMODE==2]

gc()

library(stringi)
ques_timing <- sapply(col_nam[22:length(col_nam)], function(x){
  computer <- stri_detect_regex(stri_sub(x, from=1, to=1),"C|D")
  subject <- stri_detect_regex(stri_sub(x, from=2, to=2),"S|M|R")
  timing <- stri_detect_regex(stri_sub(x, from=-1),"T")
  quest <- stri_detect_regex(stri_sub(x, from=6, to=6), "Q")
  tmp <- stri_detect_regex(stri_sub(x, from=-2, to=-2),"[0-9]")
  return(ifelse(computer & subject & timing & quest & tmp, TRUE, FALSE))
})

ques_action <- sapply(col_nam[22:length(col_nam)], function(x){
  computer <- stri_detect_regex(stri_sub(x, from=1, to=1),"C|D")
  subject <- stri_detect_regex(stri_sub(x, from=2, to=2),"S|M|R")
  action <- stri_detect_regex(stri_sub(x, from=-1),"A")
  quest <- stri_detect_regex(stri_sub(x, from=6, to=6), "Q")
  tmp <- stri_detect_regex(stri_sub(x, from=-2, to=-2),"[0-9]")
  return(ifelse(computer & subject & action & quest & tmp, TRUE, FALSE))
})

ques_solve <- sapply(col_nam[22:length(col_nam)], function(x){
  computer <- stri_detect_regex(stri_sub(x, from=1, to=1),"C|D")
  subject <- stri_detect_regex(stri_sub(x, from=2, to=2),"S|M|R")
  action <- stri_detect_regex(stri_sub(x, from=-1),"S|C")
  quest <- stri_detect_regex(stri_sub(x, from=6, to=6), "Q")
  tmp <- stri_detect_regex(stri_sub(x, from=-2, to=-2),"[0-9]")
  return(ifelse(computer & subject & action & quest & tmp, TRUE, FALSE))
})


stud2015_2 <- stud2015[, c("CNT", "CNTSCHID","CNTSTUID",
                           "ST004D01T","ST118Q01NA","ST118Q02NA",
                           "ST118Q03NA","ST118Q04NA","ST118Q05NA"), with = FALSE]

stud2015_2$CNT = as.character(stud2015_2$CNT)

setkey(dane, CNT, CNTSCHID, CNTSTUID)
setkey(stud2015_2, CNT, CNTSCHID, CNTSTUID)
dane_join <- merge(dane, stud2015_2, all.x = TRUE)

rm("stud2015")
rm("stud2015_2")

gc()

dane_join_col_action <- dane_join[, c("CNT", "CNTSCHID","CNTSTUID",
                               "CBASCI","BOOKID",
                               names(ques_action[ques_action]),
                               "ST004D01T","ST118Q01NA","ST118Q02NA",
                               "ST118Q03NA","ST118Q04NA","ST118Q05NA"), with = FALSE]
dane_join_col_time <- dane_join[, c("CNT", "CNTSCHID","CNTSTUID",
                                    "CBASCI","BOOKID",
                                    names(ques_timing[ques_timing])), with = FALSE]
dane_join_col_solve <- dane_join[, c("CNT", "CNTSCHID","CNTSTUID",
                                     "CBASCI","BOOKID",
                                     names(ques_solve[ques_solve])), with = FALSE]

rm("dane_join")
gc()
rm("dane")
gc()

dane_all_action = melt.data.table(dane_join_col_action, 
                                  id.vars = c("CNT", "CNTSCHID","CNTSTUID", "CBASCI","BOOKID"),
                                  measure.vars = setdiff(colnames(dane_join_col_action),c("CNT", "CNTSCHID","CNTSTUID", "CBASCI","BOOKID")),
                                  variable.name = "task",
                                  value.name = "n.action")


dane_all_action$task <- stri_sub(dane_all_action$task, from = 1, to=8)

dane_all_time = melt.data.table(dane_join_col_time, 
                                  id.vars = c("CNT", "CNTSCHID","CNTSTUID", "CBASCI","BOOKID"),
                                  measure.vars = setdiff(colnames(dane_join_col_time),c("CNT", "CNTSCHID","CNTSTUID", "CBASCI","BOOKID")),
                                  variable.name = "task",
                                  value.name = "timing")
                                 
dane_all_time$task <- stri_sub(dane_all_time$task, from = 1, to=8)

setkey(dane_all_action, CNT, CNTSCHID, CNTSTUID, CBASCI, BOOKID, task)
setkey(dane_all_time, CNT, CNTSCHID, CNTSTUID, CBASCI, BOOKID, task)

dane_pisa <- merge(dane_all_action, dane_all_time, all = TRUE)

gc()
rm("dane_all_action")
rm("dane_all_time")
gc()

dane_all_solve = melt.data.table(dane_join_col_solve, 
                                id.vars = c("CNT", "CNTSCHID","CNTSTUID", "CBASCI","BOOKID"),
                                measure.vars = setdiff(colnames(dane_join_col_solve),c("CNT", "CNTSCHID","CNTSTUID", "CBASCI","BOOKID")),
                                variable.name = "task",
                                value.name = "result")
dane_all_solve$task <- stri_sub(dane_all_solve$task, from = 1, to=8)

setkey(dane_all_solve, CNT, CNTSCHID, CNTSTUID, CBASCI, BOOKID, task)

dane_pisa <- merge(dane_pisa, dane_all_solve, all = TRUE)
sum(!is.na(dane_pisa$n.action) | !is.na(dane_pisa$timing) | !is.na(dane_pisa$result)) # ok 27 mln wierszy
gc()
rm("dane_all_solve")

dane_pisa_filtered = dane_pisa[(!is.na(n.action) | !is.na(timing) | !is.na(result))]
print(nrow(dane_pisa_filtered))
save(dane_pisa_filtered, file= "dane_pisa_filtered3.RDA")


