data_wide <- data_long %>% 
  spread(location_name_type, location_name)