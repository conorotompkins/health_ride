library(tidyverse)

theme_set(theme_bw())

rm(list = ls())

source("scripts/load_data_long.R")
source("scripts/load_stations_data.R")

rm(list = c("data", "data_list"))

data_long <- data_long %>% 
  filter(date >= "2017-06-01") %>% 
  select(date, location_name_type, location_name)

df_locations <- data_long %>% 
  group_by(location_name_type, location_name) %>% 
  mutate(row_number = row_number()) %>%
  spread(location_name_type, location_name)


filter(!is.na(to_station_name), !is.na(from_station_name))

group_by(location_name_type, location_name) %>% 
  summarise(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  
  
  
  

df_locations <- df_locations %>% 
  left_join(locations, by = c("location_name" = "station_name")) %>% 
  select(location_name_type, location_name, latitude, longitude)

rm(data_long)
  
df_locations <- df_locations %>% 
  
