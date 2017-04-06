library("tidyr")
library("dplyr")
library("ggplot2")
library("openxlsx")
library("stringi")
library("matrixStats")
library(ggplot2)


load("C:/Users/Agnieszka/Downloads/finalS.RData")
colnames(finalS)[14] <-"result"

load("C:/Users/Agnieszka/Downloads/finalMR.RData")


data<-rbind(finalMR, finalS)


colnames(data)
data<-data %>% mutate(exercise=stri_sub(item_short, 1,1), timing=timing/1000)

data$result<-ifelse(data$result=="No credit", 0, ifelse(data$result=="Partial credit", 1,2))
stat<-data %>% group_by(position, CNT) %>% 
  summarize(minresult=min(result),
            medresult_W=weightedMedian(result,W_FSTUWT),
            medresult=median(result),
            meanresult=round(weighted.mean(result, W_FSTUWT),2),
            maxresult=max(result),
            minActions=min(n.actions),
            medActions=median(n.actions),
            meanActions=round(weighted.mean(n.actions, W_FSTUWT),2),
            maxActions=max(n.actions),
            minTiming=min(timing),
            medTiming_W=weightedMedian(timing,W_FSTUWT),
            medTiming=median(timing),
            meanTiming=round(weighted.mean(timing, W_FSTUWT),2),
            maxTiming=max(timing))%>%mutate_if(is.factor, as.character) %>% as.data.frame()




ggplot(stat, aes(x=factor(position), y=meanTiming))+geom_violin(fill="#3366FF")

#tabelka np dla Polski
stat<-stat %>% mutate(CNT=stri_trim(CNT))

stat2<-stat %>% filter(CNT %in% c("POL", "GBR", "FIN", "DEU", "HKG", "JPN", "KOR", "USA", "MEX", "PER", "BRA", "CAN", "AUS", "ITA","CZE") )

g<-ggplot(stat2, aes(x=position, y=meanTiming, col=CNT))+geom_line()
library(plotly)
g %>% ggplotly(originalData = FALSE, legend=FALSE) 

stat2$meanTiming2<-ifelse(stat2$position==1,stat2[stat2$position==1,15]-stat2[stat2$position==1 & stat2$CNT=="POL",15],
                          ifelse(stat2$position==2,stat2[stat2$position==2,15]-stat2[stat2$position==2 & stat2$CNT=="POL",15],
                                 ifelse(stat2$position==3,stat2[stat2$position==3,15]-stat2[stat2$position==3 & stat2$CNT=="POL",15],
                                        stat2[stat2$position==4,15]-stat2[stat2$position==4 & stat2$CNT=="POL",15])))


stat2 %>% filter(CNT=="POL")

ggplot(stat2[stat2$CNT!="POL",], aes(x=CNT, y=meanTiming2))+geom_bar(stat="identity")+
  facet_grid(.~position)+coord_flip()
