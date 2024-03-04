
library(tidyverse)

# Params of interest to our partners: 
#   temperature
#   turbidity
#   total suspended solids (lacking from data)
#   nitrate

load_swmp_data <- function(filename, col_prefix) {
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
    # Add a new column that can be used to plot the monthly data as a time series
    mutate(year_frac = year + month/12, .after = 'month') %>% 
    # Fix the 'station' column to have just the 2-digit station code & then
    # add the type of station as a separate column
    mutate(station = gsub('grb', '', station)) %>% 
    separate(station, into = c('station', 'type'), sep = 2)
}

wq_grb <- load_swmp_data("input_data/wq_grb.csv", c("temp", "turb"))
met_grb <- load_swmp_data("input_data/met_grb.csv", "totprcp")
nut_grb <- load_swmp_data("input_data/nut_grb.csv", "no")

# Bind all data types together to get one big data frame
grb_data <- wq_grb %>% 
  bind_rows(met_grb) %>% 
  bind_rows(nut_grb) %>% 
  mutate(type = case_when(type == 'nut' ~ 'nutrient', 
                          type == 'wq' ~ 'water quality',
                          type == 'met' ~ 'meteo'))

# Calculate winters to add as grey rectangles in the background
winter_year_frac <- expand.grid(
  year = seq(min(grb_data$year), max(grb_data$year)),
  winter_month = c(12, 1, 2, 3)) %>% 
  as_tibble() %>% 
  mutate(year_frac = year + winter_month/12)
all_year_frac <-  expand.grid(
  year = seq(min(grb_data$year), max(grb_data$year)),
  month = 1:12) %>% 
  as_tibble() %>% 
  mutate(year_frac = year + month/12)

# Now we can explore with ggplots :)
# Make a plot per type
grb_data %>% 
  filter(param %in% c('no23f', 'no2f', 'no3f', 'temp_median', 
                      'totprcp_total', 'turb_median')) %>% 
  split(.$type) %>% 
  map(~{
    ggplot(.x, aes(x = year_frac, y = value)) + 
      geom_vline(data = winter_year_frac, aes(xintercept = year_frac), color = '#e1edef') +
      geom_point(aes(color = station), shape=1) + 
      geom_line(aes(color = station)) +
      facet_grid(param ~ station, scales='free_y') +
      scico::scale_color_scico_d(begin = 0.15, end = 0.7) +
      ggtitle('SWMP data for Great Bay', 
              subtitle = sprintf('Data type: %s', unique(.x$type))) +
      xlab('Date') + ylab('') +
      theme_bw() +
      theme(strip.background = element_blank(),
            strip.text = element_text(face='bold'))
  })

grb_data %>% 
  filter(param == 'no23f') %>% 
  right_join(all_year_frac) %>% 
  group_by(station) %>% 
  mutate(daydiff = year_frac - lag(year_frac)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year_frac, y = daydiff)) +
  geom_point() + 
  facet_grid(. ~ station)

# Visualize gaps?? In a really poor way ...
grb_data %>% 
  filter(param %in% c('no23f', 'no2f', 'no3f', 'temp_median', 
                      'totprcp_total', 'turb_median')) %>% 
  group_by(station, param, type, year_frac) %>% 
  right_join(all_year_frac) %>% 
  summarize(n = sum(!is.na(value))) %>% 
  split(.$type) %>% 
  map(~{
    ggplot(.x, aes(x = year_frac, y = n)) + 
      facet_grid(param ~ station) +
      geom_vline(data = winter_year_frac, aes(xintercept = year_frac), color = '#9dc3cc') +
      geom_line() +
      theme_bw() +
      xlab('Date') + ylab('Num values per month') +
      theme_bw() +
      theme(strip.background = element_blank(),
            strip.text = element_text(face='bold'))
  })

# Our team wanted us to focus on SQ & GB (need to treat precip separately):

precip <- grb_data %>% 
  filter(param == 'totprcp_total') %>%
  # Need to decide which Oct 2005 value to use (because that was the station switch)
  filter(!(year == 2005 & month == 10 & station == 'gl')) %>% 
  select(year, month, year_frac, precip_tot = value) 

grb_data_soi <- grb_data %>% 
  filter(param != 'totprcp_total') %>% 
  filter(station %in% c('gb', 'sq')) %>% 
  pivot_wider(names_from = 'param', values_from = 'value') %>% 
  # Join precip in
  left_join(precip, by = c('year', 'month', 'year_frac'))

# Make a plot correlating everything
grb_data_soi %>% 
  split(.$station) %>% 
  map(~{
    .x %>% 
      select(precip_tot, 
           temp_median, turb_median, no23f, 
           -c(year, month, year_frac)) %>% 
      plot(main = sprintf('Plots of vars for site `%s`',
                          unique(.x$station)))
  })

# Annual boxplots  
grb_data_soi %>% 
  select(station, year, month, no23f, precip_tot, temp_median, turb_median) %>% 
  # select(-type, -year_frac) %>% 
  pivot_longer(-c(station, year, month), names_to = 'param') %>% 
  mutate(grp = paste(year, station)) %>% 
  ggplot(aes(x=year, y=value, group=grp, fill=station)) +
  geom_boxplot(position = 'dodge') +
  facet_wrap(vars(param), scales='free_y') +
  scico::scale_fill_scico_d(begin = 0.15, end = 0.85) +
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face='bold'))

# Seasonal boxplots
grb_data_soi %>% 
  select(station, year, month, no23f, precip_tot, temp_median, turb_median) %>% 
  pivot_longer(-c(station, year, month), names_to = 'param') %>% 
  mutate(season = case_when(month %in% c(12, 1, 2, 3) ~ 'winter',
                            month %in% 4:5 ~ 'spring', 
                            month %in% 6:8 ~ 'summer', 
                            month %in% 9:11 ~ 'fall'), 
         grp = paste(year, station)) %>% 
  ggplot(aes(x=year, y=value, group=grp, fill=station)) +
  geom_boxplot(position = 'dodge') +
  facet_grid(param ~ season, scales='free_y') +
  scico::scale_fill_scico_d(begin = 0.15, end = 0.85) +
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face='bold'))
