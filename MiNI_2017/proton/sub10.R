library(BetaBit)
proton()

#zad1
head(employees)

library(dplyr)

employees %>% filter(name =="John", surname=="Insecure")

proton(action = "login", login="johnins")

#zad2
top1000passwords

czy_sie_udalo <- numeric(length(top1000passwords))

for(i in 1:length(czy_sie_udalo )){
  
  czy_sie_udalo[i] <- proton(action = "login", login="johnins", password=top1000passwords[i])
  
}

haslo <- top1000passwords[which(czy_sie_udalo == "Success! User is logged in!")]

#zad3
a <- employees

head(logs)


logi <- logs[logs$login == "slap",]
host_posz <- names(sort(table(logi$host), decreasing = T)[1])

proton(action = "server", host=host_posz)

#zad4

bash_history2 <- gsub(" ", "",bash_history, fixed = TRUE)

unique(bash_history[which(bash_history == bash_history2)])

haslo <- "DHbb7QXppuHnaXGN"

proton(action = "login", login="slap", password=haslo)



