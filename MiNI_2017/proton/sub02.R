#install.packages("BetaBit")
library(BetaBit)
library(dplyr)

# 1

proton()
employees <- employees
login <- employees %>% filter(surname == "Insecure") %>% select(login) %>% as.character()

proton(action = "login", login = login)

# > login
# [1] "johnins"

# 2

top1000passwords <- top1000passwords

for(i in top1000passwords){
  if(proton(action = "login", login=login, password=i) == "Success! User is logged in!"){
    password <- i
  }
}

# > password
# [1] "q1w2e3r4t5"

# 3

logs <- logs

# log_freq <- logs %>% filter(login == "johnins") %>% group_by(host) %>% summarise(count = n()) %>%
#   arrange(-count)
# 
# proton(action = "server", host = as.character(log_freq$host[1]))

login_pietraszko <- employees %>% filter(surname == "Pietraszko") %>% select(login) %>% as.character()

log_freq <- logs %>% filter(login == login_pietraszko) %>% group_by(host) %>% summarise(count = n()) %>%
  arrange(-count)

proton(action = "server", host = as.character(log_freq$host[1]))

#[1] "194.29.178.16"

# 4

bash_history <- bash_history
password_pietraszko <- names(head(sort(table(bash_history))))[1]

proton(action = "login", login=login_pietraszko, password=password_pietraszko)

# > password_pietraszko
# [1] "DHbb7QXppuHnaXGN"