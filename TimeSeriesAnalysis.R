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

##### Wavelet analysis #####

library(wsyn)
library(zoo)

# Borrowed these code concepts for using `wsyn` from Danny Szydlowski

# Wilkinson, G. M., Walter, J., Fleck, R., & Pace, M. L. (2020). 
# Beyond the trends: The need to understand multiannual dynamics in aquatic ecosystems. 
# Limnology and Oceanography Letters, 5(4), 281–286. https://doi.org/10.1002/lol2.10153

# Prepare data for calculating wavelets
for(this_station in c('gb', 'sq')) {
  swmp_wavelet_data <- swmp_data_prep %>%
    filter(station %in% c(this_station, 'gb-gl')) %>% 
    select(year, month, year_frac, param, value) %>% 
    pivot_wider(id_cols = c(year, month, year_frac), names_from = 'param', values_from = 'value') %>% 
    # Fill NAs for missing data
    mutate(temp_median = zoo::na.approx(temp_median, na.rm = FALSE),
           turb_median = zoo::na.approx(turb_median, na.rm = FALSE),
           no23f = zoo::na.approx(no23f, na.rm = FALSE)) %>% 
    # Filter out the first three values which cannot be linearly interpolated to fill NAs
    filter(!is.na(temp_median) & !is.na(turb_median) & !is.na(no23f))
  
  # Clean the data to fit wavelet assumptions
  no23f <- cleandat(swmp_wavelet_data$no23f, 1:length(swmp_wavelet_data$no23f), clev=5)$cdat
  precip <- cleandat(swmp_wavelet_data$totprcp_total, 1:length(swmp_wavelet_data$totprcp_total), clev=5)$cdat
  turb <- cleandat(swmp_wavelet_data$turb_median, 1:length(swmp_wavelet_data$turb_median), clev=5)$cdat
  temp <- cleandat(swmp_wavelet_data$temp_median, 1:length(swmp_wavelet_data$temp_median), clev=5)$cdat
  
  # we can then apply the wavelet transform to break the data down into oscillating
  # patterns at different timescales
  wt.no23f <- wt(no23f, 1:length(no23f))
  wt.precip <- wt(precip, 1:length(precip))
  wt.turb <- wt(turb, 1:length(turb))
  wt.temp <- wt(temp, 1:length(temp))
  
  # arrange them side by side
  par(mfrow=c(1,4), mai=c(0.3,0.3,0.3,0.3), omi=c(0.4,0.4,0.2,0.1))
  
  plotmag(wt.precip, title = sprintf("Precip - %s", this_station))
  plotmag(wt.no23f, title = sprintf("NO23f - %s", this_station))
  plotmag(wt.turb, title = sprintf("Turbdity - %s", this_station))
  plotmag(wt.temp, title = sprintf("Temp - %s", this_station))
}
