#install.packages("here")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("lubridate")

library(here)
library(dplyr)
library(reshape2) 
library (ggplot2)
library(lubridate)
library(SWMPr)
library(EnvStats)
library(trend)

#Set your working directory

##All files saved in the same github folder, including data downloaded from box
here()
wq_lks<-read.csv("LKS/wq_lks.csv")
met_lks<-read.csv("LKS/met_lks.csv")
nut_lks<-read.csv("LKS/nut_lks.csv")

wq_lks_mean<-select(wq_lks,contains(c("station","year","month","mean")))
met_lks_mean<-select(met_lks,contains(c("station","year","month","mean")))



##change it from this icky wide format to long

wq_lks_mean_long<-melt(wq_lks_mean, na.rm = FALSE, value.name = "value",
                       id = c("station","year","month"))

wq_lks_mean_subset<-dplyr::filter(wq_lks_mean_long, 
                                  grepl('temp|spcond|do_mgl|ph|turb', variable))

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


#This dataset has Na for winter, so there will be missing values to deal with

#Let's just do a regular Mann-Kendall

#Start by taking the annual average

wq_lks_annual_mean<-wq_lks_mean_subset %>%
  group_by(station,year,variable) %>%
  summarize(Mean = mean(value, na.rm=TRUE))

#plot it
ggplot(wq_lks_annual_mean,aes(x=year,y=Mean))+
  geom_point()+
  geom_line()+
  geom_smooth() +ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Mann Kendall Trend test using {trend}

lksba_temp_annual<-wq_lks_annual_mean %>%
  filter(station == "lksbawq" & variable =="temp_mean")

mk.test(lksba_temp_annual$Mean, alternative = c("two.sided"), continuity = TRUE)

#no trends in temperature present

#let's try conductivity
lksba_spcond_annual<-wq_lks_annual_mean %>%
  filter(station == "lksbawq" & variable =="spcond_mean")

mk.test(lksba_spcond_annual$Mean, alternative = c("two.sided"), continuity = TRUE)

#Bingo! We have a trend! tau is negative, so trend is neg. 
#This tracks with the plot

#okay let's calculate the sen slope now

sens.slope(lksba_spcond_annual$Mean, conf.level = 0.95)

##Most of the data is seasonal, so let's do a seasonal trend analysis using 
#the monthly data

# We will use the Env stats package bc it is friendly to missing values
#This package also allows us to test for heterogeneity in trends across seasons
# If the p-value for vanBelle Hughs heterogeneity of monotonic trends test is significant (< 0.05), then the null hypothesis that
#seasonal trends are monotonic can be rejected (van Belle and Hughes 1984) and we can assume that
#monotonic trends are different across seasons

#In other words, if p>0.05, then you are good to go

lksba_temp_monthly<-wq_lks_mean_subset %>%
  filter(station == "lksbawq" & variable =="temp_mean")


kendallSeasonalTrendTest(value ~ month + year,
                         data = lksba_temp_monthly,na.action=na.pass)

#p-value for heterogeneity is greater than 0.05, so we can assume that trends are
#monotonic across seasons, but slope p value is 0.06, so not significant at a=0.05

lksba_spcond_monthly<-wq_lks_mean_subset %>%
  filter(station == "lksbawq" & variable =="spcond_mean")


kendallSeasonalTrendTest(value ~ month + year,
                         data = lksba_spcond_monthly,na.action=na.pass)

#p-value for heterogeneity is greater than 0.05, so we can assume that trends are
#monotonic across seasons; slope p value is 0.005, so it is significant at a=0.05

#How does this compare to the mk.test above with aggregated yearly data?


##################BREAKPOINT ANALYSIS#####################
#We will be using {BreakPoints} to apply Buishand and Pettit's tests

bubreak_lksba_spcond<-Buishand_R(lksba_spcond_monthly$value,n_period=5,dstr='self',simulations = 1000)
ptbreak_lksba_spcond<-pettit(lksba_spcond_monthly$value,n_period=5)

plot(lksba_spcond_monthly$value)
abline(v = bubreak_lksba_spcond$breaks,lwd=6,col="blue")
abline(v = ptbreak_lksba_spcond$breaks, lty=2,col="red",lwd=4)

#Cool, they are in agreement on where the break is located

bubreak_lksba_spcond
ptbreak_lksba_spcond

#But only one test is significant at the a=0.05 level. What should you do?

###Let's try doing these processes sequentially, Breakpoint->Trend

bubreak_lksba_spcond<-Buishand_R(lksba_spcond_monthly$value,n_period=5,dstr='self',simulations = 1000)
ptbreak_lksba_spcond<-pettit(lksba_spcond_monthly$value,n_period=5)

bubreak_lksba_spcond
ptbreak_lksba_spcond

#Breakpoint is positioned at row 65

lksba_spcond_monthly_seg1<-lksba_spcond_monthly %>% slice(1:65)
lksba_spcond_monthly_seg2<-lksba_spcond_monthly %>% slice(66:128)

plot(lksba_spcond_monthly$value)
points(lksba_spcond_monthly_seg1$value,col="red")
x<-(66:128)
points(x,lksba_spcond_monthly_seg2$value,col="blue")

#okay, looks like everything is where is should be

lksba_spcond_annual_mean1<-lksba_spcond_monthly_seg1 %>%
  group_by(station,year,variable) %>%
  summarize(Mean = mean(value, na.rm=TRUE))

lksba_spcond_annual_mean2<-lksba_spcond_monthly_seg2 %>%
  group_by(station,year,variable) %>%
  summarize(Mean = mean(value, na.rm=TRUE))


mk.test(lksba_spcond_annual_mean1$Mean, alternative = c("two.sided"), continuity = TRUE)
mk.test(lksba_spcond_annual_mean2$Mean, alternative = c("two.sided"), continuity = TRUE)

#No sig trends on either side. When we considered this all together, there
#was a negative overall trend using annual data

#Lets try this with seasonal components

kendallSeasonalTrendTest(value ~ month + year,
                         data = lksba_spcond_monthly_seg1,na.action=na.pass)

#trends are not the same across seasons and slop is not significant

kendallSeasonalTrendTest(value ~ month + year,
                         data = lksba_spcond_monthly_seg2,na.action=na.pass)

#trend are not heterogeneous, but slope is not significant

