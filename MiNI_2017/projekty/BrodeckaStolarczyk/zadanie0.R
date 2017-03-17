library(dplyr)
library(tidyr)
library(stringi)
library(data.table)

dane1 <- readRDS("cog.rds")

dane2 <- readRDS("qqq.rds")

dane2 %>%
    select(CNT, CNTRYID, CNTSCHID, CNTSTUID, BOOKID, CBASCI, ST004D01T,
           starts_with("ST118")) ->
        dane2

panstwa <- unique(dane1$CNTRYID) 

wyniki <- list(length(panstwa))

for (i in seq_along(panstwa)) {
    panstwo <- panstwa[i]
    cat(paste0("Panstwo ", i, ": ", panstwo, " "))
    
    t0 <- Sys.time()

    cog <- filter(dane1, CNTRYID == panstwo)
    qqq <- filter(dane2, CNTRYID == panstwo)
    
   
    cog %>% 
        select(CNT, CNTSCHID, CNTSTUID, BOOKID, CBASCI, 
               matches("^[CD][SMR][0-9]{3,3}Q[0-9]{2,2}[SCAT]")) ->
        cog
    
    cog$id <- 1:nrow(cog)
    
    cog %>% 
        select(id, matches("^[CD][SMR][0-9]{3,3}Q[0-9]{2,2}[SC]")) ->
        scores
    cog %>% 
        select(id, matches("^[CD][SMR][0-9]{3,3}Q[0-9]{2,2}A$")) ->
        actions
    cog %>% 
        select(id, matches("^[CD][SMR][0-9]{3,3}Q[0-9]{2,2}T$")) ->
        times
    
    
    f <- function(df) {
        df <- gather(df, nazwa_kolumny, wartosc, -id) %>%
                     mutate(item = stri_sub(nazwa_kolumny, 1, 8)) %>%
                     select(-nazwa_kolumny)
        df <- as.data.table(df)
        setkey(df, item, id)
    } 
    
    scores <- f(scores)
    actions <- f(actions)
    times <- f(times)
    
    names(scores)[2] <- "result"
    names(actions)[2] <- "n.actions"
    names(times)[2] <- "timing"
    
    scores %>%
        merge(actions) %>%
        merge(times) ->
        wynik
    
        
    cog %>% 
        select(id, CNT, CNTSCHID, CNTSTUID, BOOKID, CBASCI) %>%
        left_join(wynik, by = "id") %>%
        select(-id) %>%
        filter(!is.na(n.actions))->
        wynik
   
    
    wynik %>% 
        left_join(qqq, by = c("CNT", "CNTSCHID", "CNTSTUID", "BOOKID", "CBASCI")) ->
        wynik
    
    wyniki[[i]] <- wynik
    
    cat(round(difftime(Sys.time(), t0, unit = "secs")), "s\n", sep="") 
    
} 



wyniki <- do.call(rbind, wyniki)

wyniki$BOOKID <- as.integer(stri_extract_first_regex(wyniki$BOOKID, "[0-9]+"))



# Do³¹czenie tabeli z pozycj¹ zadania


# source("form_klaster_pytanie_pozycja.R")
form_kl_pyt_pos <- readRDS("form_klaster_pytanie_pozycja.rds")


wyniki %>% 
    left_join(form_kl_pyt_pos, by = c("item" = "pytanie", "BOOKID" = "form")) ->
    wyniki
    

saveRDS(wyniki, file = "zadanie0_wynik.rds")

