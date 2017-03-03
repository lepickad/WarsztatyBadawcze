
library(BetaBit)
library(dplyr)

proton()

# Z1
z1 <- employees[employees$name == 'John' & employees$surname == 'Insecure',3]
proton(action = "login", login=z1)



# z2
for(i in 1:length(top1000passwords)){
  ans <- proton(action = "login", login=z1, password=top1000passwords[i])

  if (ans != "Password or login is incorrect") print(i)  
}

z2 <- top1000passwords[120]
proton(action = "login", login=z1, password=z2)

a <- which(unique(employees$surname) == 'Pietraszko')
login <- employees[a,3]

logs %>% filter(login == login) %>% group_by(host, login) %>% summarize(n = n()) %>% arrange(desc(n))

proton(action = "server", host="194.29.178.16")


pass <- sub(".* ", "",bash_history)

for(i in 1:length(pass)){
    ans <- proton(action = "login", login=login, password=pass[i])
  if (ans != "Password or login is incorrect") print(i)  
}


proton(action = "login", login=login, password=pass[10050])
