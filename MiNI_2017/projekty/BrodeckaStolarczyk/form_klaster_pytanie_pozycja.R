library(dplyr)
library(tidyr)
library(readxl)
library(stringi)
cog <- readRDS("cog.rds")

#' Przyporządkowanie zadań do klastrów dla matemtyki i czytania
#' na podstawie formularzy 55-60

formularze <- paste0("Form ", 55:60, " (CBA)")
wynikiM <- list(length(formularze))
wynikiR <- list(length(formularze))

for (i in seq_along(formularze)) {
    
    form <- filter(cog, BOOKID == formularze[i])
    
    matematyka <- select(form, matches("^[CD]M.*"))
    czytanie <- select(form, matches("^[CD]R.*"))
    
    matematyka %>%
        select(matches("^.{8,8}[SC]")) %>%
        sapply(function(x) !all(is.na(x))) -> 
        indM
    czytanie %>%
        select(matches("^.{8,8}[SC]")) %>%
        sapply(function(x) !all(is.na(x))) -> 
        indR
    
    tmpM <- grep("^.{8,8}[SC]", names(matematyka), value = TRUE)[indM]
    tmpR <- grep("^.{8,8}[SC]", names(czytanie), value = TRUE)[indR]
    
    wynikiM[[i]] <- tmpM
    wynikiR[[i]] <- tmpR
}

wynikiM
wynikiR


# Przekształcenie do tabeli:
# nazwa_pytania | nazwa_klastra

klastryM <- c("M01", "M02", "M03", "M04", "M05", "M06ab")
klastryR <- c("R01", "R02", "R03", "R04", "R05", "R06ab")


pytanie_klaster_M <- mapply(function(x, y) {
    data.frame(pytanie = x, klaster = y)
}, wynikiM, klastryM, SIMPLIFY = FALSE)


pytanie_klaster_R <- mapply(function(x, y) {
    data.frame(pytanie = x, klaster = y)
}, wynikiR, klastryR, SIMPLIFY = FALSE)


pytanie_klaster <- rbind(
    do.call(rbind, pytanie_klaster_M), 
    do.call(rbind, pytanie_klaster_R)
)

pytanie_klaster$pytanie <- substr(pytanie_klaster$pytanie, 1, 8)




#' przekształcenie excela sheet_structure.xlsx do postaci tabeli
#' id_formularza | pozycja | nazwa_klastra   

klastry <- read_excel("sheet_structure.xlsx")
names(klastry)[1] <- "form"

klastry %>% 
    gather(position, klaster, -form) %>%
    mutate(position = as.numeric(stri_extract_first_regex(position, "[0-9]")),
           klaster = stri_trim(klaster)) ->
    form_pos_kl


#' złączenie powyższych dwóch tabel
form_kl_pyt_pos <- left_join(pytanie_klaster, form_pos_kl,
                             by = "klaster")

saveRDS(form_kl_pyt_pos, "form_klaster_pytanie_pozycja.rds")



