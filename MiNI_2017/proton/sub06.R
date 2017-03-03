library(BetaBit)

proton()

#1
employees %>% filter(name=='John', surname=='Insecure')
proton(action="login", login='johnins')
top1000passwords

#2
haslo <- lapply(top1000passwords, function(x){
  tmp <- proton(action = "login", login="johnins", password=x)
  return(ifelse(tmp=='Success! User is logged in!',1, 0))
})
which(haslo>0.5) #120

#3
logs$data 

logs %>% filter(login=="johnins") %>% group_by(host) %>% summarise(n=n())

logs %>% group_by(login) %>% summarise(n=n()) ->tmp
logs %>% filter(login=="slap") %>% group_by(host) %>% summarise(n=n())

proton(action = "server", host="194.29.178.16")

#4

library(stringi)
unique(stri_extract_first_words(bash_history))
proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")