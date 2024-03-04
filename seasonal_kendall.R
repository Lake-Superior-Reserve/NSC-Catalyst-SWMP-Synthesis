
library(EnvStats) # This is what USGS hydrologists recommend for Seasonal Kendalls
library(tidyverse)

wq_filepath <- 'input_data/wq_grb.csv'
met_filepath <- 'input_data/met_grb.csv'
nutrient_filepath <- 'input_data/nut_grb.csv'

reserve_abbr <- 'grb'
site_abbr <- 'sq'
# site_abbr <- 'gb'

trend_test_param <- 'temp_mean'
plot_yaxis_name <- 'Mean temperature, deg C'
plot_yaxis_limits <- c(0, 30) # Adjust limits for other params
plot_subtitle <- 'Great Bay Reserve, Squamscott site'
plot_subtitle <- 'Great Bay Reserve, Great Bay site'

# # Uncomment to try turbidity
# trend_test_param <- 'turb_mean'
# plot_yaxis_name <- 'Mean turbidity'
# plot_yaxis_limits <- c(0, 110) # Adjust limits for other params
# plot_subtitle <- 'Great Bay Reserve, Squamscott site'
# # plot_subtitle <- 'Great Bay Reserve, Great Bay site'

##### Load the data #####

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

##### Just show all the data that was loaded ###

# ggplot(swmp_data, aes(x = year_frac, y = value, color = station)) +
#   geom_point() + geom_line() +
#   facet_wrap(vars(param), scales='free') +
#   theme_bw()

##### Apply Seasonal Kendall to the parameter and site specified at the top #####

# Filter the data to just the site and parameter of interest
swmp_data_site_param <- swmp_data %>% 
  filter(station == site_abbr,
         param == trend_test_param) %>% 
  mutate(month_abb = sprintf('%02d-%s', month, month.abb[month]))

# Show just temperature but split by season
ggplot(swmp_data_site_param, aes(x = year_frac, y = value, color = month_abb)) +
  geom_point() + geom_line() +
  facet_wrap(vars(month), scales='free') +
  theme_bw()

# Run the seasonal kendall model
sk_model <- kendallSeasonalTrendTest(value ~ month_abb + year, data = swmp_data_site_param)
# sk_model <- kendallTrendTest(value ~ year, data = swmp_data_site_param)

# View the output
print(sk_model)

# See how many samples per 

# Extract the p-value & test if it is significant
sk_model_pval <- sk_model$p.value['z (Trend)']
sk_model_pval < 0.05

# Look at what data went into the model (should be familiar from our plot earlier)
sk_model$sample.size

# Look at the trends found by season
sk_model_seasonal_trend_info <- sk_model$seasonal.estimates
sk_model_seasonal_slope_yint <- as_tibble(sk_model_seasonal_trend_info) %>% 
  # Add the months as a column so we can plot later
  mutate(month_abb = rownames(sk_model_seasonal_trend_info))

# Extract the overall trend line across all seasons as a 
# table to be able to add to the plot.
sk_model_overall_slope_intercept <- tibble(
  slope = sk_model$estimate['slope'],
  intercept = sk_model$estimate['intercept'])

# Show trend outputs by season
ggplot(swmp_data_site_param, aes(x = year_frac, y = value, color = month_abb)) +
  geom_point(size=2) + #geom_line() +
  facet_wrap(vars(month_abb), scales='free') +
  geom_abline(data = sk_model_seasonal_slope_yint, linewidth = 1.5,
              aes(slope = slope, intercept = intercept, color=month_abb)) +
  theme_bw() +
  theme(strip.text = element_text(face = 'bold', size=16),
        strip.background = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16)) +
  xlab('Date') + ylab(plot_yaxis_name) +
  ggtitle('Seasonal Kendall trends by month',
          sub = plot_subtitle)

# Weirdly I cannot figure out why the overall trend line to appears below all the data ...
# It's sort of close to the mean of all the other lines, so I don't understand why it wouldn't appear
ggplot(swmp_data_site_param, aes(x=year_frac, y=value)) +
  geom_point() +
  geom_line() +
  ylim(plot_yaxis_limits) +
  geom_abline(data = sk_model_overall_slope_intercept, linewidth=1.5, color='blue',
              aes(slope = slope, intercept = intercept)) +
  theme_bw() +
  xlab('Date') + ylab(plot_yaxis_name) +
  ggtitle('Overall Seasonal Kendall trend',
          sub = plot_subtitle)
