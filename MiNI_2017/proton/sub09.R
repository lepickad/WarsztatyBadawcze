install.packages("BetaBit")
library("BetaBit")
proton()

# prob 1
employees
proton(action = "login", login="XYZ")
employees[which(employees$name=="John"), ]
proton(action  ="login", login="johnins")

# prob 2
for (i in 1:1000)
proton(action = "login", login="johnins", password=top1000passwords[i])

#prob 3
employees[which(employees$surname=="Pietraszko"), ]
which.max(table(logs[logs$login=="slap",2]))

proton(action = "server", host="194.29.178.16")

# prob 4
for (i in 1:length(bash_history))
  proton(action = "login", login="slap", password=bash_history[i])

