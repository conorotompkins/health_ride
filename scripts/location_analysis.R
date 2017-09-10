library(tidyverse)

theme_set(theme_bw())

rm(list = ls())

source("scripts/load_data_long.R")
source("scripts/load_stations_data.R")

rm(list = c("data", "data_list"))

df_locations <- data_long %>% 
  select(location_name_type, location_name) %>% 
  left_join(locations, by = c("location_name" = "station_name")) %>% 
  select(location_name_type, location_name, latitude, longitude) %>% 
  group_by(location_name_type, location_name, latitude, longitude) %>% 
  summarise(number_of_trips = n())

rm(data_long)
  
df_locations <- df_locations %>% 
  spread(location_name_type, location_name)
