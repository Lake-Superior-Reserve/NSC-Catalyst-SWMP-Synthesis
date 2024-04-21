# Prepare monthly data in format for plotting
# Includes step that combines the precip data, even though 
# it moved from GB-->GL in Oct 2005.

load_swmp_monthly_data <- function(filename, reserve_abbr, stations_of_interest, col_prefix) {
  suppressMessages(read_csv(filename, show_col_types = FALSE)) %>% 
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
    # Add a new column that can be used to plot the monthly data as a time series
    mutate(year_frac = year + month/12, .after = 'month') %>% 
    # Fix the 'station' column to have just the 2-digit station code & then
    # add the type of station as a separate column
    mutate(station = gsub(reserve_abbr, '', station)) %>% 
    separate(station, into = c('station', 'type'), sep = 2) %>% 
    filter(station %in% c(stations_of_interest))
}

reserve_abbr <- 'grb'
params_of_interest <- c('temp', 'turb', 'no23f', 'totprcp')
stations_of_interest <- c('gb', 'sq') # Used for monthly data filtering

# Read and then combine all data types together to get one big data frame
swmp_data <- 
  # Load monthly meteo data
  load_swmp_monthly_data('input_data/met_grb.csv', reserve_abbr, c('gb', 'gl'), params_of_interest) %>% 
  # Both have a data point for October 2005, so keep only the precip from GB on that date
  filter(!(year == 2005 & month == 10 & station == 'gl')) %>% 
  # Make a new station name that shows it is combined
  # Because GB precip station ends in 2005 and then picks up as GL
  mutate(station = 'gb-gl') %>% 
  bind_rows(load_swmp_monthly_data('input_data/wq_grb.csv', reserve_abbr, stations_of_interest, params_of_interest)) %>% 
  bind_rows(load_swmp_monthly_data('input_data/nut_grb.csv', reserve_abbr, stations_of_interest, params_of_interest)) %>% 
  mutate(type = case_when(type == 'nut' ~ 'nutrient', 
                          type == 'wq' ~ 'water quality',
                          type == 'met' ~ 'meteo')) %>% 
  # Only keep the median values for temp and turb + NO23f + precip
  filter(!grepl('_max|_mean|_min|_sd|_iqr', param)) %>% 
  # Add an ordered factor for station names to make wrapped plots appear better aligned
  mutate(stationf = factor(station, levels = c('gb', 'sq', 'gb-gl'), 
                           labels = c('Great Bay', 'Squamscott', 'GB Weather Stn'),
                           ordered = TRUE),
         paramf = factor(param, levels = c('no23f', 'temp_median', 'turb_median', 'totprcp_total'), 
                         labels = c('Nitrate/Nitrite (mg/L)', 'Temperature (deg C)',
                                    'Turbidity (NTU)', 'Precipitation (mm)'),
                         ordered = TRUE)) %>% 
  mutate(station_param = sprintf('%s, %s', stationf, paramf),
         station_param_f = factor(station_param, ordered = TRUE,
                                  # Reorder so that the GB Weather Stn appears last
                                  levels = sort(levels(interaction(stationf, paramf, sep = ", ", drop = T)))[c(2:7, 1)]))

# Clean up environment since this is likely being sourced to load the data
rm(reserve_abbr, params_of_interest, stations_of_interest, load_swmp_monthly_data)
