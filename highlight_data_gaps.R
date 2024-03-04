# Visualize gaps better for turbidity/temp at GB/SQ

library(tidyverse)

wq_filepath <- 'input_data/wq_grb.csv'
met_filepath <- 'input_data/met_grb.csv'
nutrient_filepath <- 'input_data/nut_grb.csv'

reserve_abbr <- 'grb'
params_of_interest <- c('temp_median', 'turb_median', 'no23f')
stations_of_interest <- c('gb', 'sq')

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
