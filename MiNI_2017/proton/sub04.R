library(BetaBit)
library(dplyr)
proton()

#Problem 1
head(employees)
employees[employees$name=="John" & employees$surname=="Insecure",]

proton(action = "login", login="johnins")

#Problem 2
top1000passwords

for (p in top1000passwords)
{
  proton(action = "login", login="johnins", password=p)
}

#Problem 3
employees[employees$surname=="Pietraszko",]

dane<-logs%>%filter(login=="slap")%>%group_by(host)%>%count(host)
dane
proton(action = "server", host="194.29.178.16")

#Problem 4

for (p in bash_history)
{
  proton(action = "login", login="slap", password=p)
}