#Title: Exploring SWMP Data
#Project: NSC SWMP Synthesis Catalyst Class
#Funding: NERRS Science Collaborative
#Author(s): Dr. Kait Reinl; kreinl@wisc.edu
#Lake Superior National Estuarine Research Reserve, UW-Madison Division of Extension

#Data has been downloaded from NERRS CDMO: https://cdmo.baruch.sc.edu/ and compiled into
#csvs using code from https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Data_processing
#This repo is currently private until analyses are complete, but will be made public after
#publication. Examples data from LKS are uploaded to this repo. 

#Install required packages if not already installed
#install.packages("here")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("lubridate")

# Load necessary libraries
library(here)       # For setting working directories
library(dplyr)      # For data manipulation
library(reshape2)   # For reshaping data
library(ggplot2)    # For plotting
library(lubridate)  # For working with dates
library(SWMPr)      # For SWMP data manipulation
library(EnvStats)   # For environmental statistics
library(trend)      # For trend analysis

# Set your working directory - there are a number of methods, but you can simply click
# "Session>Set working directory>To Source File Location"

# Read in the data, may need to modify some for how your data is stored, but this is 
# set up to pull from a folder called 'LKS' for Lake Superior Data with the files
# named as below
here()
wq_lks <- read.csv("LKS/wq_lks.csv")  # Load water quality data
met_lks <- read.csv("LKS/met_lks.csv")  # Load meteorological data
nut_lks <- read.csv("LKS/nut_lks.csv")  # Load nutrient data

# Select relevant columns containing "station", "year", "month", and "mean"
wq_lks_mean <- select(wq_lks, contains(c("station", "year", "month", "mean")))
met_lks_mean <- select(met_lks, contains(c("station", "year", "month", "mean")))

# Summary statistics by station
by(wq_lks_mean, wq_lks_mean$station, summary)
by(met_lks_mean, met_lks_mean$station, summary)
by(nut_lks, nut_lks$station, summary)

# Change data from wide format to long format
wq_lks_mean_long <- melt(wq_lks_mean, na.rm = FALSE, value.name = "value",
                         id = c("station", "year", "month"))

# Plot the data using ggplot2
ggplot(wq_lks_mean_long) +
  geom_point(aes(x = as.factor(year), y = value)) +
  facet_grid(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Alternative plotting with facet_wrap for better scale visibility
ggplot(wq_lks_mean_long) +
  geom_point(aes(x = as.factor(year), y = value)) +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Select parameters of interest using a different method
wq_lks_mean_subset <- dplyr::filter(wq_lks_mean_long, 
                                    grepl('temp|spcond|do_mgl|ph|turb', variable))

# Display unique variables in the subset
unique(wq_lks_mean_subset$variable)

# Create a date column for more granular plotting
wq_lks_mean_subset$Date <- make_date(year = wq_lks_mean_subset$year, 
                                     month = wq_lks_mean_subset$month)

# Plot the subset data with additional layers
ggplot(wq_lks_mean_subset, aes(x = Date, y = value)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %Y") +
  ylab("") + xlab("") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Change station labels to actual names
wq_lks_mean_subset$station <- factor(wq_lks_mean_subset$station, 
                                     labels = c("Barker's Island", "Blatnik Bridge", "Oliver Bridge", "Pokegama Bay"))

# Replot with new station labels
ggplot(wq_lks_mean_subset, aes(x = Date, y = value)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %Y") +
  ylab("") + xlab("") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# More plotting with fixed years and months
fixed_year <- 1991
fixed_month <- 10
wq_lks_mean_subset$Date_month <- make_date(year = fixed_year, month = wq_lks_mean_subset$month)
wq_lks_mean_subset$Date_year <- make_date(year = wq_lks_mean_subset$year, month = fixed_month)

# Plot monthly data
ggplot(wq_lks_mean_subset, aes(x = Date_month, y = value, group = month)) +
  geom_boxplot() +
  ylab("") + xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot with smooth lines
ggplot(wq_lks_mean_subset) +
  geom_boxplot(aes(x = Date_month, y = value, group = month)) +
  geom_smooth(aes(x = Date_month, y = value)) +
  ylab("") + xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot yearly data
ggplot(wq_lks_mean_subset, aes(x = Date_year, y = value, group = year)) +
  geom_boxplot() +
  ylab("") + xlab("") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot with smooth lines for yearly data
ggplot(wq_lks_mean_subset) +
  geom_boxplot(aes(x = Date_year, y = value, group = year)) +
  geom_smooth(aes(x = Date_year, y = value)) +
  ylab("") + xlab("") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

####
# Nutrient Data

# Select relevant columns from nutrient data
nut_lks_clean <- nut_lks[, c(2:10)]
# Reshape nutrient data to long format
nut_lks_long <- melt(nut_lks_clean, na.rm = FALSE, value.name = "value",
                     id = c("station", "year", "month"))

# Initial plot of nutrient data
ggplot(nut_lks_long) +
  geom_point(aes(x = as.factor(year), y = value)) +
  facet_grid(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Alternative plotting with facet_wrap
ggplot(nut_lks_long) +
  geom_point(aes(x = as.factor(year), y = value)) +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Filter nutrient data for specific parameters
nut_lks_long_subset <- dplyr::filter(nut_lks_long, 
                                     grepl('po4|nh4|no23|chla', variable))

# Display unique variables in the nutrient subset
unique(nut_lks_long_subset$variable)

# Create a date column for the nutrient subset
nut_lks_long_subset$Date <- make_date(year = nut_lks_long_subset$year, 
                                      month = nut_lks_long_subset$month)

# Plot the nutrient subset data with smooth lines
ggplot(nut_lks_long_subset, aes(x = Date, y = value)) +
  geom_point() +
  geom_smooth() +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %Y") +
  ylab("") + xlab("") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Change station labels to actual names for nutrient data
nut_lks_long_subset$station <- factor(nut_lks_long_subset$station, 
                                      labels = c("Barker's Island", "Blatnik Bridge", "Oliver Bridge", "Pokegama Bay"))

# Replot nutrient data with new station labels
ggplot(nut_lks_long_subset, aes(x = Date, y = value)) +
  geom_point() +
  geom_smooth() +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %Y") +
  ylab("") + xlab("") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# More plotting for nutrient data with fixed years and months
fixed_year <- 1991
fixed_month <- 10
nut_lks_long_subset$Date_month <- make_date(year = fixed_year, month = nut_lks_long_subset$month)
nut_lks_long_subset$Date_year <- make_date(year = nut_lks_long_subset$year, month = fixed_month)

# Plot monthly nutrient data
ggplot(nut_lks_long_subset, aes(x = Date_month, y = value, group = month)) +
  geom_boxplot() +
  ylab("") + xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot with smooth lines for monthly nutrient data
ggplot(nut_lks_long_subset) +
  geom_boxplot(aes(x = Date_month, y = value, group = month)) +
  geom_smooth(aes(x = Date_month, y = value)) +
  ylab("") + xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot yearly nutrient data
ggplot(nut_lks_long_subset, aes(x = Date_year, y = value, group = year)) +
  geom_boxplot() +
  ylab("") + xlab("") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot with smooth lines for yearly nutrient data
ggplot(nut_lks_long_subset) +
  geom_boxplot(aes(x = Date_year, y = value, group = year)) +
  geom_smooth(aes(x = Date_year, y = value)) +
  ylab("") + xlab("") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(station ~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
