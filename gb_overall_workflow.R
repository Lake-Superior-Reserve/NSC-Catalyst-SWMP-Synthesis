# Final analysis script for Great Bay NERR
# Lizzie Emch and Lindsay Platt
# April 2024

# Load library dependencies
library(tidyverse)

##### 1 Data loading / preparing #####

source('1_prepare_monthly_grb_data.R')

##### 2 Data exploration and visual validation #####

###### 2A Time series gaps ######

source('2A_explore_data_gaps.R')
print(missing_data_figure) # Print out the missing data figure

###### 2B Basic time series with loess smoothing ######

###### 2C Basic time series with loess smoothing by month ######

##### 3 Scales of variability #####

###### 3A Time series decomposition ######

###### 3B Seasonal signals via wavelet transform ######

##### 4 Time series trend analysis #####

###### 4A Mann-Kendall ######

###### 4B Seasonal Mann-Kendall ######

###### 4C Mann-Kendall by each season ######

##### 5 Time series break-point analysis #####

##### 6 Time series dynamics, linearity and non-linearity #####
