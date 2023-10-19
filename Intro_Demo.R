#install.packages("here")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("ggplot2")

library(here)
library(dplyr)
library(reshape2) 
library (ggplot2)

##All files saved in the same github folder, including data downloaded from box
here()
wq_apa<-read.csv("Data/wq_apa.csv")


wq_apa_max<-select(wq_apa,contains(c("station","year","month","mean")))
wq_apa_mean_long<-melt(wq_apa_max, na.rm = FALSE, value.name = "value",
                      id = c("station","year","month"))

ggplot(wq_apa_mean_long)+
  geom_point(aes(x=as.factor(year),y=value))+
  facet_grid(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
