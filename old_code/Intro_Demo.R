#install.packages("here")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("lubridate")

library(here)
library(dplyr)
library(reshape2) 
library(ggplot2)
library(lubridate)
library(SWMPr)
library(readr)

#Set your working directory

##All files saved in the same github folder, including data downloaded from box
here()
wq_lks<-read_csv("input_data/wq_grb.csv")
met_lks<-read_csv("input_data/met_grb.csv")
nut_lks<-read_csv("input_data/nut_grb.csv")

wq_lks_mean<-select(wq_lks,contains(c("station","year","month","mean")))
met_lks_mean<-select(met_lks,contains(c("station","year","month","mean")))


by(wq_lks_mean, wq_lks_mean$station, summary)
by(met_lks_mean, met_lks_mean$station, summary)
by(nut_lks, nut_lks$station, summary)



##change it from this icky wide format to long

wq_lks_mean_long<-melt(wq_lks_mean, na.rm = FALSE, value.name = "value",
                      id = c("station","year","month"))

### Alright let's see what's in here, nothing fancy
ggplot(wq_lks_mean_long)+
  geom_point(aes(x=as.factor(year),y=value))+
  facet_grid(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##Those grids make the scale hard to see, let's try facet

ggplot(wq_lks_mean_long)+
  geom_point(aes(x=as.factor(year),y=value))+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##Select the parameters of interest, let's do it differently than above

wq_lks_mean_subset<-dplyr::filter(wq_lks_mean_long, 
        grepl('temp|spcond|do_mgl|ph|turb', variable))

unique(wq_lks_mean_subset$variable)

##And let's make a date we can get more granular with
wq_lks_mean_subset$Date <- make_date(year = wq_lks_mean_subset$year, 
                                     month = wq_lks_mean_subset$month)

##Okay let's look at some more data now
ggplot(wq_lks_mean_subset,aes(x=Date,y=value))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b %Y") +ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##Change the labels to the actual station names - can be found on CDMO
wq_lks_mean_subset$station<-factor(wq_lks_mean_subset$station, 
       labels=c("Great Bay","Lamprey River", "Oyster River", "Squamscott River"))

ggplot(wq_lks_mean_subset,aes(x=Date,y=value))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b %Y") +ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##More plotting
fixed_year <- 1991
fixed_month<-10
wq_lks_mean_subset$Date_month <- make_date(year=fixed_year, month = wq_lks_mean_subset$month)
wq_lks_mean_subset$Date_year <- make_date(year=wq_lks_mean_subset$year, month = fixed_month)


ggplot(wq_lks_mean_subset,aes(x=Date_month,y=value,group=month))+
  geom_boxplot()+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(wq_lks_mean_subset)+
  geom_boxplot(aes(x=Date_month,y=value,group=month))+
  geom_smooth(aes(x=Date_month,y=value))+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(wq_lks_mean_subset,aes(x=Date_year,y=value,group=year))+
  geom_boxplot()+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(wq_lks_mean_subset)+
  geom_boxplot(aes(x=Date_year,y=value,group=year))+
  geom_smooth(aes(x=Date_year,y=value))+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

 
####
##Nutrient Data
nut_lks_clean<-nut_lks[,c(2:10)]
nut_lks_long<-melt(nut_lks_clean, na.rm = FALSE, value.name = "value",
                       id = c("station","year","month"))

ggplot(nut_lks_long)+
  geom_point(aes(x=as.factor(year),y=value))+
  facet_grid(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nut_lks_long)+
  geom_point(aes(x=as.factor(year),y=value))+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


nut_lks_long_subset<-dplyr::filter(nut_lks_long, 
                                  grepl('po4|nh4|no23|chla', variable))

unique(nut_lks_long_subset$variable)


nut_lks_long_subset$Date <- make_date(year = nut_lks_long_subset$year, 
                                     month = nut_lks_long_subset$month)


ggplot(nut_lks_long_subset,aes(x=Date,y=value))+
  geom_point()+
  geom_smooth()+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b %Y") +  ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

nut_lks_long_subset$station<-factor(nut_lks_long_subset$station, 
                                   labels=c("Great Bay","Lamprey River", "Oyster River", "Squamscott River"))

ggplot(nut_lks_long_subset,aes(x=Date,y=value))+
  geom_point()+
  geom_smooth()+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b %Y") +  ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##More plotting
fixed_year <- 1991
fixed_month<-10
nut_lks_long_subset$Date_month <- make_date(year=fixed_year, month = nut_lks_long_subset$month)
nut_lks_long_subset$Date_year <- make_date(year=nut_lks_long_subset$year, month = fixed_month)


ggplot(nut_lks_long_subset,aes(x=Date_month,y=value,group=month))+
  geom_boxplot()+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nut_lks_long_subset)+
  geom_boxplot(aes(x=Date_month,y=value,group=month))+
  geom_smooth(aes(x=Date_month,y=value))+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nut_lks_long_subset,aes(x=Date_year,y=value,group=year))+
  geom_boxplot()+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nut_lks_long_subset)+
  geom_boxplot(aes(x=Date_year,y=value,group=year))+
  geom_smooth(aes(x=Date_year,y=value))+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
