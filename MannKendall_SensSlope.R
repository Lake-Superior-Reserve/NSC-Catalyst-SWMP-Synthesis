#Title: Mann Kendall and Sen's Slope
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
library(here)       # For setting the working directory relative to the script location
library(dplyr)      # For data manipulation
library(reshape2)   # For reshaping data between wide and long formats
library(ggplot2)    # For data visualization
library(lubridate)  # For date manipulation
library(SWMPr)      # For data analysis of the System-Wide Monitoring Program
library(EnvStats)   # For environmental statistics, including trend analysis
library(trend)      # For trend analysis
library(BreakPoints) # For detecting breakpoints in time series data


# Set your working directory - there are a number of methods, but you can simply click
# "Session>Set working directory>To Source File Location"


# Set working directory to the script location
here()

# Load datasets (assumed to be saved in a folder named "LKS")
wq_lks <- read.csv("LKS/wq_lks.csv")   # Water quality data
met_lks <- read.csv("LKS/met_lks.csv") # Meteorological data
nut_lks <- read.csv("LKS/nut_lks.csv") # Nutrient data

# Select relevant columns containing "station", "year", "month", and "mean" from water quality and meteorological data
wq_lks_mean <- select(wq_lks, contains(c("station", "year", "month", "mean")))
met_lks_mean <- select(met_lks, contains(c("station", "year", "month", "mean")))

# Change data from wide format to long format
wq_lks_mean_long <- melt(wq_lks_mean, na.rm = FALSE, value.name = "value", id = c("station", "year", "month"))

# Subset data to include specific variables: temperature, specific conductance, dissolved oxygen, pH, and turbidity
wq_lks_mean_subset <- dplyr::filter(wq_lks_mean_long, grepl('temp|spcond|do_mgl|ph|turb', variable))

# Create a date column combining year and month
wq_lks_mean_subset$Date <- make_date(year = wq_lks_mean_subset$year, month = wq_lks_mean_subset$month)

# Plot data with ggplot2 to visualize trends over time
ggplot(wq_lks_mean_subset, aes(x = Date, y = value)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %Y") +
  ylab("") +
  xlab("") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# This dataset has NA for winter, so there will be missing values to deal with

# Calculate annual average for each station and variable
wq_lks_annual_mean <- wq_lks_mean_subset %>%
  group_by(station, year, variable) %>%
  summarize(Mean = mean(value, na.rm = TRUE))

# Plot annual mean data to visualize trends
ggplot(wq_lks_annual_mean, aes(x = year, y = Mean)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  ylab("") +
  xlab("") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Mann-Kendall Trend test using {trend}
# Purpose: Identify monotonic trends in time series data. 
# This non-parametric test evaluates if there is a significant trend 
# (positive or negative) in the data over time.
# tau varies from -1 to 1 and indicates the strength and direction of teh trends, while the p-value 
# indicates statistical significance
# Assumptions: The data should be independent and identically distributed. 
# The test does not assume normality or linearity.

# Perform Mann-Kendall trend test on annual temperature data for station "lksbawq"
lksba_temp_annual <- wq_lks_annual_mean %>%
  filter(station == "lksbawq" & variable == "temp_mean")
mk.test(lksba_temp_annual$Mean, alternative = c("two.sided"), continuity = TRUE)

# No significant trends in temperature at a=0.05 present

# Perform Mann-Kendall trend test on annual specific conductance data for station "lksbawq"
lksba_spcond_annual <- wq_lks_annual_mean %>%
  filter(station == "lksbawq" & variable == "spcond_mean")
mk.test(lksba_spcond_annual$Mean, alternative = c("two.sided"), continuity = TRUE)

# Bingo! We have a trend! Tau is negative, so the trend is negative.
# This tracks with the plot

# Sen's Slope
# Purpose: Estimate the magnitude of the trend in the time series. 
# This non-parametric method provides a robust estimate of the trend's slope.
# Assumptions: The method assumes continuous data with independent errors and 
# does not require normality.

# Calculate Sen's slope for specific conductance data
sens.slope(lksba_spcond_annual$Mean, conf.level = 0.95)

# Most of the data is seasonal, so let's do a seasonal trend analysis using the monthly data

# Seasonal Mann-Kendall Trend Test
# Purpose: Account for seasonal variations in time series data and check if 
#trends are consistent across different seasons. 
# This test extends the Mann-Kendall test by incorporating seasonality into the 
# trend analysis.
# tau varies from -1 to 1 and indicates the strength and direction of teh trends, while the p-value 
# indicates statistical significance
# Assumptions: The test assumes monotonic trends within each season. 
# If the p-value for the van Belle and Hughes heterogeneity test is 
# significant (< 0.05), it indicates variability in trends across seasons.
# In other words, if p > 0.05, then you are good to go.

# We will use the EnvStats package because it is friendly to missing values.
# This package also allows us to test for heterogeneity in trends across seasons.


# Perform seasonal trend analysis using monthly data for temperature
lksba_temp_monthly <- wq_lks_mean_subset %>%
  filter(station == "lksbawq" & variable == "temp_mean")

kendallSeasonalTrendTest(value ~ month + year, data = lksba_temp_monthly, na.action = na.pass)

# Result: p-value for heterogeneity is greater than 0.05, 
#so we can assume that trends are monotonic across seasons,
#but trend v-values is also greater than 0.05, so not statistically significant


# Perform seasonal trend analysis using monthly data for specific conductance
lksba_spcond_monthly <- wq_lks_mean_subset %>%
  filter(station == "lksbawq" & variable == "spcond_mean")

kendallSeasonalTrendTest(value ~ month + year, data = lksba_spcond_monthly, na.action = na.pass)

# Result: p-value for heterogeneity is greater than 0.05, so we can assume that trends are monotonic across seasons; 
#slope p-value is 0.005, so it is significant at α = 0.05

# Compare seasonal trend results to Mann-Kendall test on aggregated yearly data
