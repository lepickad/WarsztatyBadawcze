proton()
which(employees$surname=='Insecure')
employees[217,]
proton(action = "login", login="johnins")

for( i in 1:1000)
{
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

pom<-logs[which(logs$login=="johnins"),]
pom1<-as.factor(logs$host)
pom2<- levels(pom1)

for(i in 1:length(pom2)){
  print(i)
  proton(action = "server", host=pom2[i])
}
# 194.29.178.16.


library(stringi)
table(unlist(stri_match_first_regex(bash_history, "([A-Za-z0-9])+[ ]?$", omit_no_match = TRUE)[,1]))
# DHbb7QXppuHnaXGN

employees[which(employees$surname=='Pietraszko'),]
# slap
proton(action = "login", login="slap", password='DHbb7QXppuHnaXGN')
