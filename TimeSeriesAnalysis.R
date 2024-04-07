# Exploring time series decomposition and seasonal signals

source('prepare_monthly_data.R')

# Replace NAs with the mean value
swmp_data_prep <- swmp_data %>% 
  # Filter to only median values
  filter(param %in% c('temp_median', 'turb_median', 
                      'totprcp_total', 'no23f')) %>% 
  group_by(station, param) %>% 
  mutate(mean_val = mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  # Now replace NAs with the mean value
  mutate(value = ifelse(is.na(value), mean_val, value)) %>% 
  select(-mean_val) %>% 
  mutate(station_param = sprintf('%s_%s', station, param)) 

##### TS Decomposition #####

# Only pass in data for one site and one parameter at a time
# Needs the columns `station`, `param`, `value`, `date`
apply_seasonal_decomposition <- function(data) {
  
  # Prep the timeseries object
  ts_obj <- ts(data = data$value,  
               frequency=12)
  
  # Now decompose timeseries
  ts_decomposed_obj <- decompose(ts_obj)
  
  # Return a nice tibble with all of these
  tibble(station = unique(data$station),
         param = unique(data$param),
         observed = ts_decomposed_obj$x,
         trend = ts_decomposed_obj$trend,
         seasonal = ts_decomposed_obj$seasonal,
         random = ts_decomposed_obj$random) %>% 
    mutate(year_frac = data$year_frac)
  
}

decomposed_ts_all <- swmp_data_prep %>% 
  # filter(param == 'no23f') %>% 
  split(.$station_param) %>% 
  map(~apply_seasonal_decomposition(.x)) %>% 
  bind_rows(.id = 'station_param') %>% 
  pivot_longer(cols = -c(station_param, station, param, year_frac), names_to = 'ts_type')

# Plot everything
decomposed_ts_all %>% 
  ggplot(aes(x = year_frac, y = value, 
             color = ts_type)) +
  geom_line(size=1) + 
  facet_wrap(vars(station, param), scales='free', ncol=2) +
  theme_bw() +
  ggtitle('All decomposed TS components')

# Plot only the trend vs observed
decomposed_ts_all %>% 
  filter(ts_type %in% c('observed', 'trend')) %>% 
  ggplot(aes(x = year_frac, y = value, 
             color = ts_type)) +
  geom_line(size=1) + 
  facet_wrap(vars(station, param), scales='free', ncol=2) +
  theme_bw() +
  ggtitle('Observed signal + trend')

# Plot only the seasonal
decomposed_ts_all %>% 
  filter(ts_type %in% c('seasonal')) %>% 
  ggplot(aes(x = year_frac, y = value, 
             color = ts_type)) +
  geom_line(size=1) + 
  facet_wrap(vars(station, param), scales='free', ncol=2) +
  theme_bw() +
  ggtitle('Seasonal signals')
