# Visualize gaps better for turbidity/temp at GB/SQ

library(tidyverse)

wq_filepath <- 'input_data/wq_grb.csv'
met_filepath <- 'input_data/met_grb.csv'
nutrient_filepath <- 'input_data/nut_grb.csv'

reserve_abbr <- 'grb'
params_of_interest <- c('temp_median', 'turb_median', 'no23f', 'totprcp_total')
stations_of_interest <- c('gb', 'sq', 'gl')

load_swmp_data <- function(filename, reserve_abbr) {
  read_csv(filename, show_col_types = FALSE) %>% 
    # Keep only station & date columns plus those that 
    # match one of the prefixes passed into the fxn
    select(station, year, month, everything()) %>% 
    # Get rid of any of the '_nValid' columns or censored (?) columns
    select(-ends_with('nValid'), -ends_with('cens')) %>% 
    # Pivot to long so we can combine
    pivot_longer(-c(station, year, month), names_to = 'param') %>% 
    # Add a new column that can be used to plot the monthly data as a time series
    mutate(year_frac = year + month/12, .after = 'month') %>% 
    # Fix the 'station' column to have just the 2-digit station code & then
    # add the type of station as a separate column
    mutate(station = gsub(reserve_abbr, '', station)) %>% 
    separate(station, into = c('station', 'type'), sep = 2)
}

# Load each of the datasets
swmp_waterquality <- load_swmp_data(wq_filepath, reserve_abbr)
swmp_meteo <- load_swmp_data(met_filepath, reserve_abbr)
swmp_nutrient <- load_swmp_data(nutrient_filepath, reserve_abbr)

# Bind all data types together to get one big data frame
swmp_data <- swmp_waterquality %>% 
  bind_rows(swmp_meteo) %>% 
  bind_rows(swmp_nutrient) %>% 
  mutate(type = case_when(type == 'nut' ~ 'nutrient', 
                          type == 'wq' ~ 'water quality',
                          type == 'met' ~ 'meteo'))

# Join data into ALL POSSIBLE VALUES.
grb_data_of_interest <- swmp_data %>% 
  filter(param %in% params_of_interest,
         station %in% stations_of_interest) %>% 
  select(station, year, month, param, value, year_frac)

all_possible_years <- min(grb_data_of_interest$year):max(grb_data_of_interest$year)
all_possible_months <- 1:12
all_possible_year_months <- expand.grid(
  year = all_possible_years,
  month = all_possible_months
) %>% as_tibble() %>% 
  mutate(year_month = sprintf('%s-%02d', year, month)) %>% 
  pull(year_month)
all_possible_param_year_months <- expand.grid(
  year_month = all_possible_year_months,
  param = params_of_interest
) %>% as_tibble() %>% 
  mutate(year_month_param = sprintf('%s-%s', year_month, param)) %>% 
  pull(year_month_param)
all_possible_station_param_year_months <- expand.grid(
  year_month_param = all_possible_param_year_months,
  station = stations_of_interest
) %>% as_tibble() %>% 
  separate(year_month_param, into = c('year', 'month', 'param'), sep = '-', convert=TRUE) %>% 
  select(station, year, month, param)

# Which param-dates are missing for GB?
grb_missing <- all_possible_station_param_year_months %>% 
  left_join(grb_data_of_interest) %>%
  mutate(year_frac = year + month/12, .after = 'month') %>% 
  filter(is.na(value)) %>% 
  mutate(isWinter = month %in% c(12, 1, 2, 3))

# Make a table that will add a special symbol to the plot
# to indicate the May 2016 Mother's Day flood
momday_flood <- grb_data_of_interest %>% 
  group_by(station, param) %>% 
  mutate(min_value = min(value, na.rm=TRUE)) %>% 
  filter(year == 2016, month == 5) %>% 
  select(station, param, year_frac, min_value) %>% 
  distinct()  

ggplot(grb_data_of_interest, aes(x = year_frac, y = value)) + 
  facet_grid(param ~ station, scales='free_y') +
  geom_vline(data = grb_missing, 
             aes(xintercept = year_frac, color = isWinter)) +
  scale_color_manual(values = c(`TRUE` = '#e1edef', `FALSE` = '#c38728')) +
  geom_point() +
  geom_point(data = momday_flood, aes(y = min_value), size=3, shape = 8, color = 'salmon') +
  theme_bw() +
  xlab('Date') + ylab('Num values per month') +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face='bold'))

### Now look at gaps with the daily data

source('prepare_daily_and_monthly_data.R')

# Setup all possible combinations of dates that should exist.

# First, do it for monthly nutrient data
possible_monthly_years <- min(year(swmp_data$date)):max(year(swmp_data$date))
possible_monthly_months <- 1:12
possible_monthly_dates <- expand.grid(
  year = possible_monthly_years,
  month = possible_monthly_months
) %>% as_tibble() %>% 
  mutate(date = sprintf('%s-%02d-15', year, month)) %>% 
  pull(date)
possible_monthly_param_dates <- expand.grid(
  date = possible_monthly_dates,
  param = unique(swmp_nutrient$param)
) %>% as_tibble() %>% 
  mutate(date_param = sprintf('%s__%s', date, param)) %>% 
  pull(date_param)
possible_monthly_station_param_dates <- expand.grid(
  date_param = possible_monthly_param_dates,
  station = unique(swmp_nutrient$station)
) %>% as_tibble() %>% 
  separate(date_param, into = c('date', 'param'), sep = '__', convert=TRUE) %>% 
  select(station, date, param)

# Now identify all possible daily water quality data
possible_daily_wq_dates <- seq(min(swmp_data$date), max(swmp_data$date), by = '1 day')
possible_daily_wq_param_dates <- expand.grid(
  date = possible_daily_wq_dates,
  param = unique(swmp_waterquality$param)
) %>% as_tibble() %>% 
  mutate(date_param = sprintf('%s__%s', date, param)) %>% 
  pull(date_param)
possible_daily_wq_station_param_dates <- expand.grid(
  date_param = possible_daily_wq_param_dates,
  station = unique(swmp_waterquality$station)
) %>% as_tibble() %>% 
  separate(date_param, into = c('date', 'param'), sep = '__', convert=TRUE) %>% 
  select(station, date, param)

# Now identify all possible daily meteo data
possible_daily_met_dates <- seq(min(swmp_data$date), max(swmp_data$date), by = '1 day')
possible_daily_met_param_dates <- expand.grid(
  date = possible_daily_met_dates,
  param = unique(swmp_meteo$param)
) %>% as_tibble() %>% 
  mutate(date_param = sprintf('%s__%s', date, param)) %>% 
  pull(date_param)
possible_daily_met_station_param_dates <- expand.grid(
  date_param = possible_daily_met_param_dates,
  station = unique(swmp_meteo$station)
) %>% as_tibble() %>% 
  separate(date_param, into = c('date', 'param'), sep = '__', convert=TRUE) %>% 
  select(station, date, param)

all_possible_station_param_dates <- possible_monthly_station_param_dates %>% 
  bind_rows(possible_daily_wq_station_param_dates) %>% 
  bind_rows(possible_daily_met_station_param_dates) %>% 
  mutate(date = as.Date(date))

# Which param-dates are missing for GB?
swmp_missing <- all_possible_station_param_dates %>% 
  left_join(swmp_data) %>%
  filter(is.na(value)) %>% 
  mutate(isWinter = month(date) %in% c(12, 1, 2, 3))


# Pick out just one of each of the params
swmp_data_toPlot <- swmp_data %>% 
  filter(grepl('median|no23f', param))
swmp_missing_toPlot <- swmp_missing %>% 
  filter(grepl('median|no23f', param))

ggplot(swmp_data_toPlot, aes(x = date, y = value)) + 
  facet_grid(param ~ station, scales='free_y') +
  geom_vline(data = swmp_missing_toPlot, 
             aes(xintercept = date, color = isWinter)) +
  scale_color_manual(values = c(`TRUE` = '#e1edef', `FALSE` = '#c38728')) +
  geom_point() +
  theme_bw() +
  xlab('Date') + ylab('Value') +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face='bold'))

# Plot the precip separately
swmp_data_toPlotPrecip <- swmp_data %>% 
  filter(station == 'gb-gl')
swmp_missing_toPlotPrecip <- swmp_missing %>% 
  filter(station == 'gb-gl')

ggplot(swmp_data_toPlotPrecip, aes(x = date, y = value)) + 
  facet_grid(param ~ station, scales='free_y') +
  geom_vline(data = swmp_missing_toPlotPrecip, 
             aes(xintercept = date, color = isWinter)) +
  scale_color_manual(values = c(`TRUE` = '#e1edef', `FALSE` = '#c38728')) +
  geom_point() +
  theme_bw() +
  xlab('Date') + ylab('Value') +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face='bold'))
