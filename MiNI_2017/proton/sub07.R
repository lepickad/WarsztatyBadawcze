install.packages("BetaBit")
library("BetaBit")
proton()
-------------------
head(employees)
employees[employees$name == 'John' & employees$surname=='Insecure',3]

proton(action = "login", login="johnins")
--------------------
head(top1000passwords)
length(top1000passwords)

for(i in 1:1000){
proton(action = "login", login="johnins", password = top1000passwords[i])
}
---------------------
head(logs)
library("dplyr")

head(employees)
employees[employees$surname=='Pietraszko',]

z <- logs[logs$login == 'slap',]
z %>% group_by(host) %>% summarize(n = n())

proton(action = "server", host="194.29.178.16")
----------------------
for(i in bash_history){
  proton(action = "login", login="slap", password = i)
}
