# Prepare daily data in format for plotting
# Except that nutrients aren't available for daily timestep, so still monthly
# Includes step that combines the precip data, even though 
# it moved from GB-->GL in Oct 2005.

library(tidyverse)

wq_gb_filepath <- 'input_data/grbgbwq_daily.csv'
wq_sq_filepath <- 'input_data/grbsqwq_daily.csv'
met_gb_filepath <- 'input_data/grbgbmet_daily.csv' # GB ends in 2005 for precip
met_gl_filepath <- 'input_data/grbglmet_daily.csv' # GL starts in 2005 for precip
nutrient_filepath <- 'input_data/nut_grb.csv' # Nutrient stays as a monthly ts

reserve_abbr <- 'grb'
params_of_interest <- c('temp', 'turb', 'no23f', 'totprcp')
stations_of_interest <- c('gb', 'sq') # Used for monthly data filtering

load_swmp_daily_data <- function(filename, reserve_abbr, col_prefix) {
  read_csv(filename, show_col_types = FALSE) %>% 
    # Keep only station & date columns plus those that 
    # match one of the prefixes passed into the fxn
    select(station, date, 
           # This creates a regular expression that will search for 
           # strings that *start* with any of the prefixes passed in
           matches(sprintf('^(%s)', paste(col_prefix, collapse = '|')))) %>% 
    # Get rid of any of the '_nValid' columns or censored (?) columns
    select(-ends_with('nValid'), -ends_with('cens')) %>% 
    # Pivot to long so we can combine
    pivot_longer(-c(station, date), names_to = 'param') %>% 
    # Fix the 'station' column to have just the 2-digit station code & then
    # add the type of station as a separate column
    mutate(station = gsub(reserve_abbr, '', station)) %>% 
    separate(station, into = c('station', 'type'), sep = 2)
}

load_swmp_monthly_data <- function(filename, reserve_abbr, col_prefix) {
  read_csv(filename, show_col_types = FALSE) %>% 
    # Keep only station & date columns plus those that 
    # match one of the prefixes passed into the fxn
    select(station, year, month, 
           # This creates a regular expression that will search for 
           # strings that *start* with any of the prefixes passed in
           matches(sprintf('^(%s)', paste(col_prefix, collapse = '|')))) %>% 
    # Get rid of any of the '_nValid' columns or censored (?) columns
    select(-ends_with('nValid'), -ends_with('cens')) %>% 
    # Pivot to long so we can combine
    pivot_longer(-c(station, year, month), names_to = 'param') %>% 
    # Fix the 'station' column to have just the 2-digit station code & then
    # add the type of station as a separate column
    mutate(station = gsub(reserve_abbr, '', station)) %>% 
    separate(station, into = c('station', 'type'), sep = 2)
}

# Load each of the datasets
swmp_waterquality_gb <- load_swmp_daily_data(wq_gb_filepath, reserve_abbr, params_of_interest)
swmp_waterquality_sq <- load_swmp_daily_data(wq_sq_filepath, reserve_abbr, params_of_interest)
swmp_waterquality <- bind_rows(swmp_waterquality_gb, swmp_waterquality_sq)
swmp_meteo_gb <- load_swmp_daily_data(met_gb_filepath, reserve_abbr, params_of_interest) %>% 
  # Keep only precip data from GB before Oct 13, 2005 (*not including*)
  filter(date < as.Date('2005-10-13'))
swmp_meteo_gl <- load_swmp_daily_data(met_gl_filepath, reserve_abbr, params_of_interest) %>% 
  # Keep only precip data from GL after or on Oct 13, 2005 
  filter(date >= as.Date('2005-10-13'))
swmp_meteo <- bind_rows(swmp_meteo_gb, swmp_meteo_gl) %>% 
  # Make a new station name that shows it is combined
  mutate(station = 'gb-gl')
swmp_nutrient <- load_swmp_monthly_data(nutrient_filepath, reserve_abbr, params_of_interest) %>% 
  filter(station %in% stations_of_interest) %>% 
  # Add middle of the month as the date for nutrient data
  mutate(date = as.Date(sprintf('%s-%02d-15', year, month)), 
         .before = param) %>% 
  select(-year, -month)

# Bind all data types together to get one big data frame
swmp_data <- swmp_waterquality %>% 
  bind_rows(swmp_meteo) %>% 
  bind_rows(swmp_nutrient) %>% 
  mutate(type = case_when(type == 'nut' ~ 'nutrient', 
                          type == 'wq' ~ 'water quality',
                          type == 'met' ~ 'meteo'))
