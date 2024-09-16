#Title: Breakpoint analysis
#Project: NSC SWMP Synthesis Catalyst Class
#Funding: NERRS Science Collaborative
#Author(s): Dr. Kait Reinl; kreinl@wisc.edu
#Lake Superior National Estuarine Research Reserve, UW-Madison Division of Extension

#Data originated from NERRS CDMO: https://cdmo.baruch.sc.edu/ and 
#Data was compiled into csvs for all Reservesusing code from 
#https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Data_processing
#This repo is currently private until analyses are complete, but will be made public after
#publication. 

#This code uses data from Lake Superior Reserve. This folder has been updated to the
#repo. Code showing how data was split out for Reserves is in 'Data Processing'. 


#####################################################################################
# Install necessary packages 
# install.packages("package name")

# Load required libraries
library(here)        # Provides an easy way to construct file paths
library(dplyr)       # Provides data manipulation functions
library(reshape2)   # Provides functions to reshape data
library(ggplot2)     # Provides functions for data visualization
library(lubridate)   # Provides functions to work with dates and times
library(SWMPr)       # Provides tools for water quality data analysis
library(EnvStats)    # Provides functions for environmental statistics
library(trend)       # Provides functions for trend analysis
library(BreakPoints) # Provides functions for detecting breakpoints in time series data

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

#We will be using {BreakPoints} to apply Buishand and Pettit's tests

# Assumptions for Breakpoint Tests:
# 1. **Continuity**: The time series data is assumed to be continuous and univariate,
#  with no missing values or abrupt changes unrelated to the breakpoints being tested.
# 2. **Independence**: Observations should be independent; autocorrelation in the data 
# could affect the results.
# 3. **Normality**: While Buishand's test can work with non-normal data if `dstr` is set to 'self', 
# Pettit's test assumes that the data follows a normal distribution.
# 4. **Appropriate Period**: The `n_period` parameter should be chosen based on the length of the 
# time series and the expected frequency of changes.
# 5. **Significance of Breakpoints**: Both tests assume that detected breakpoints represent 
# significant changes in the time series data and are not due to random fluctuations.

##See MannKendall_SensSlope.R for assumptions for those tests

# Apply Buishand's test for detecting breakpoints in the time series data
bubreak_lksba_spcond <- Buishand_R(
  lksba_spcond_monthly$value, # Input time series data
  n_period = 5,               # Number of periods for test (e.g., years)
  dstr = 'self',              # Distribution type
  simulations = 1000          # Number of simulations for testing
)

# Apply Pettit's test for detecting breakpoints in the time series data
ptbreak_lksba_spcond <- pettit(
  lksba_spcond_monthly$value, # Input time series data
  n_period = 5                # Number of periods for test (e.g., years)
)

# Plot the time series data with detected breakpoints
plot(lksba_spcond_monthly$value) # Plot the original time series
abline(v = bubreak_lksba_spcond$breaks, lwd = 6, col = "blue") # Add Buishand's breakpoints in blue
abline(v = ptbreak_lksba_spcond$breaks, lty = 2, col = "red", lwd = 4) # Add Pettit's breakpoints in red

# Print detected breakpoints from both tests
bubreak_lksba_spcond
ptbreak_lksba_spcond

# Only one test is significant at the alpha = 0.05 level. Next, perform trend analysis.

# Reapply breakpoint tests to confirm results
bubreak_lksba_spcond <- Buishand_R(
  lksba_spcond_monthly$value, 
  n_period = 5, 
  dstr = 'self', 
  simulations = 1000
)
ptbreak_lksba_spcond <- pettit(
  lksba_spcond_monthly$value, 
  n_period = 5
)

# Print results again to confirm breakpoints
bubreak_lksba_spcond
ptbreak_lksba_spcond

# The detected breakpoint is at row 65
# Split data into segments based on detected breakpoint
lksba_spcond_monthly_seg1 <- lksba_spcond_monthly %>% slice(1:65)
lksba_spcond_monthly_seg2 <- lksba_spcond_monthly %>% slice(66:128)

# Plot the time series with segments colored differently
plot(lksba_spcond_monthly$value) 
points(lksba_spcond_monthly_seg1$value, col = "red") # Segment 1 in red
x <- (66:128)
points(x, lksba_spcond_monthly_seg2$value, col = "blue") # Segment 2 in blue

# Calculate annual means for each segment
lksba_spcond_annual_mean1 <- lksba_spcond_monthly_seg1 %>%
  group_by(station, year, variable) %>%
  summarize(Mean = mean(value, na.rm = TRUE)) # Calculate mean value for each year

lksba_spcond_annual_mean2 <- lksba_spcond_monthly_seg2 %>%
  group_by(station, year, variable) %>%
  summarize(Mean = mean(value, na.rm = TRUE)) # Calculate mean value for each year

# Breakpoints analysis can be combined with trend analysis. Sometimes there is a state shift where
# the slope before and after a disturbance are different. Similarly, there may be a new mean or variance 
# before and after a point in time. Let's try this out below. 

# Apply Mann-Kendall trend test to annual means for each segment
mk.test(lksba_spcond_annual_mean1$Mean, alternative = c("two.sided"), continuity = TRUE)
mk.test(lksba_spcond_annual_mean2$Mean, alternative = c("two.sided"), continuity = TRUE)
# The Mann-Kendall test evaluates the trend in the data. 
# The "two.sided" test checks for both increasing and decreasing trends.

# Note that no significant trends are detected in either segment.
# An overall negative trend was found when considering all data together.

# Seasonal trend analysis for each segment
kendallSeasonalTrendTest(
  value ~ month + year,
  data = lksba_spcond_monthly_seg1,
  na.action = na.pass
)
# Evaluates trends considering seasonal variations in the first segment.
# The trend is not consistent across seasons and the slope is not significant

kendallSeasonalTrendTest(
  value ~ month + year,
  data = lksba_spcond_monthly_seg2,
  na.action = na.pass
)
# Evaluates trends considering seasonal variations in the second segment.
# Trends are not heterogeneous, but the slope is not significant
