# Create figures with time series points and basic loess smoothing
# for each station, parameter, and month combination. Figures are
# built for each station + parameter, so there should be 7 total
# figures in the output plot list.

seasonal_ts_figure_list <- swmp_data %>% 
  # For each parameter and station combination, map over the subsetted data
  # and make a grid of plots for time series in each month
  split(.$station_param_f) %>% 
  map(~{
    # Renaming the variable with the data from the special notation that `map()` 
    # is using to store the current subset of data `.x`
    swmp_data_stn_param <- .x %>% 
      mutate(month_abb = month.abb[month],
             month_abbf = factor(month_abb, levels = month.abb, ordered = TRUE,
                                 labels = month.name))
    # Now make a plot of facets with one line plot per month for this single parameter and site
    ggplot(swmp_data_stn_param, aes(x = year_frac, y = value)) +
      geom_point() +
      geom_smooth(color = '#2C5292', linewidth = 2, 
                  fill = '#5B8BA2', alpha=0.15,
                  # Use the default smoothing methods but explicitly
                  # declare them so that messages stop popping up
                  method = 'loess', formula = 'y ~ x') +
      ylab(unique(swmp_data_stn_param$paramf)) + 
      xlab("Year") + 
      ggtitle(unique(swmp_data_stn_param$station_param_f)) + 
      facet_wrap(~ month_abbf, scales="free_y", ncol = 4, drop = FALSE) +
      theme_bw() + 
      theme(plot.title = element_text(size=17, face='bold', hjust=0.5, vjust=2),
            axis.title.y = element_text(vjust=2.5), 
            strip.background = element_blank(),
            strip.text = element_text(size=12, color = '#303030'))
  })

