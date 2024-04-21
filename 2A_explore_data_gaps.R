# Assumes you have sourced `1_prepare_monthly_grb_data.R`

all_possible_years <- min(swmp_data$year):max(swmp_data$year)
all_possible_months <- 1:12
all_possible_year_months <- expand.grid(
  year = all_possible_years,
  month = all_possible_months
) %>% as_tibble() %>% 
  mutate(year_month = sprintf('%s-%02d', year, month)) %>% 
  pull(year_month)
all_possible_param_year_months <- expand.grid(
  year_month = all_possible_year_months,
  param = unique(swmp_data$param)
) %>% as_tibble() %>% 
  mutate(year_month_param = sprintf('%s-%s', year_month, param)) %>% 
  pull(year_month_param)
all_possible_station_param_year_months <- expand.grid(
  year_month_param = all_possible_param_year_months,
  station = unique(swmp_data$station)
) %>% as_tibble() %>% 
  separate(year_month_param, into = c('year', 'month', 'param'), sep = '-', convert=TRUE) %>% 
  select(station, year, month, param)

# Which param-dates are missing for GB?
grb_missing <- all_possible_station_param_year_months %>% 
  left_join(swmp_data, join_by(station, year, month, param)) %>%
  mutate(year_frac = year + month/12, .after = 'month') %>% 
  filter(is.na(value)) %>% 
  mutate(isWinter = month %in% c(12, 1, 2, 3))

# Make a table that will add a special symbol to the plot
# to indicate the May 2016 Mother's Day flood
momday_flood <- swmp_data %>% 
  group_by(station, param) %>% 
  mutate(min_value = min(value, na.rm=TRUE)) %>% 
  filter(year == 2016, month == 5) %>% 
  select(station, param, year_frac, min_value) %>% 
  distinct()  

missing_data_figure <- ggplot(swmp_data, aes(x = year_frac, y = value)) + 
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

# Remove all objects from environment except for the ggplot object
rm(list = c(ls()[grepl('all_possible', ls())], "momday_flood", "grb_missing"))
