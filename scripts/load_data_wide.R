data_wide <- data_long %>% 
  spread(station_name_type, station_name)