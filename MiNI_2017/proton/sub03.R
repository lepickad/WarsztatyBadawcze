library("BetaBit")

proton(hint = TRUE)

employees %>%
  filter(surname == "Insecure")

proton(action = "login", login="johnins")

for (pass in top1000passwords) {
  x <- proton(action = "login", login="johnins", password=pass)
  if (x != 'Password or login is incorrect') {
    break
  }
}

pass

proton(action = "login", login="johnins", password="q1w2e3r4t5")

employees %>%
  filter(surname == "Pietraszko")

logs %>% 
  filter(login == "slap") %>%
  group_by(host) %>%
  summarise(ile = n()) %>%
  arrange(desc(ile)) %>%
  head()

proton(action = "server", host = "194.29.178.16")

unlist(strsplit(bash_history, split = " ")) %>%
  table %>% 
  sort


proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
