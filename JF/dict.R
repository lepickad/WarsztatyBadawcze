# tworzenie slownika
library(RTCGA.methylation)
dict <- t(KIRP.methylation[2,])
dict <- dict[-1,]
dict <- data.frame(REF=names(dict), Gene_names=dict)
rownames(dict) <- NULL
save(dict, file="~/WarsztatyBadawcze/JF/dict.rda")