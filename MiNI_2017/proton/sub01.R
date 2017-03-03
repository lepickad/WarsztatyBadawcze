install.packages("BetaBit")
library(BetaBit)

proton()

# 1.login

employees[employees$surname == "Insecure", ]

#johnins

proton(action = "login", login="johnins")

# 2. password

top1000passwords
t = proton(action = "login", login="johnins", password="ABC")

for (pass in top1000passwords){
  message = proton(action = "login", login="johnins", password=pass)
  if(message != t){
    break
    true_password = pass
  }
} 

# 3. logs

library(data.table)
logs_dt = as.data.table(logs)
logs_dt[login == "slap"][, .N, host]

proton(action = "server", host = "194.29.178.16")

# 4. password 
library(stringi)
unique(unlist(stri_extract_all_regex(bash_history, "[^ ]+")))
# jedno wygladalo podejrzanie
proton(action = "login", login="slap", password = "DHbb7QXppuHnaXGN")

