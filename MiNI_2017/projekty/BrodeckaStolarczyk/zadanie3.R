

setwd('C:/Users/abrodecka001/Desktop/WB')
options(java.parameters = "-Xmx8000m")

library(dplyr)
library(matrixStats)
library(tidyr)
library(knitr)
library(stringr)
library(xlsx) 

data <- readRDS('zadanie0_wynik.rds')
data <- data %>% filter(BOOKID >= 31 & BOOKID <= 61)

newdata <- list()

data$result[data$result == 'Partial credit'] = 'No credit'
data %>% filter(result %in% c('Full credit')) -> items

jgc <- function()
{
  gc()
  .jcall("java/lang/System", method = "gc")
}  


items <- unique(items$item)

for (it in items){

    jgc()
    data %>% filter(item == it) %>% 
      select(CNT, timing, W_FSTUWT) %>%
      group_by(CNT) %>% summarise(TimeAvg = weighted.mean(timing,W_FSTUWT, na.rm = TRUE ),
                                  TimeMin = min(timing),
                                  TimeMax = max(timing),
                                  TimeMed = weightedMedian(timing, W_FSTUWT, na.rm = TRUE)) %>%
      mutate(CNT = str_trim(CNT)) %>%
      as.data.frame() -> time
    
    
    data %>% filter(item == it) %>%  
      select(CNT, result) %>%
      group_by(CNT, result) %>% summarise(value = n()) %>% 
      as.data.frame( )%>% spread(result, value) %>% 
      mutate(FreqFull = round(`Full credit`/rowSums(.[-1]), 2),
             CNT = str_trim(CNT)) %>%
      left_join(time, by = c('CNT' = 'CNT')) %>% arrange(desc(FreqFull)) -> odp
    
    colnames(odp)[colnames(odp) == '<NA>'] <- 'NaValues'
    
    write.xlsx(x = odp, file = "odp.xlsx",
               sheetName = it, row.names = FALSE, append = TRUE)
    
    odp$item <- it
    newdata[[it]] <- odp
}


newdata <- do.call('rbind', newdata)
head(newdata)

saveRDS(newdata, 'PodstawoweStatystyki.rds')
