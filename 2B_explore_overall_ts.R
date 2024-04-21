# Create figures with time series points and basic loess smoothing

overall_ts_figure <- ggplot(swmp_data, aes(x = year_frac, y = value, color = stationf)) +
  geom_point() +
  geom_smooth() +
  ylab("Parameter Value") + xlab("Year-Month") +
  facet_wrap(~ station_param_f, scales="free_y") +
  scale_color_scico_d(name = "Station", end = 0.80) + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12, face='bold'))
