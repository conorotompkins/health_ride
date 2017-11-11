library(tidyverse)
library(modelr)

theme_set(theme_minimal())

#rm(list = ls())

source("scripts/load_data_long.R")
source("scripts/load_stations_data.R")

rm(list = c("data", "data_list"))

data_long

df_long <- data_long %>% 
  select(station_name, station_name_type) %>% 
  left_join(data_station_locations)

df_wide <- data_long %>%
  spread(station_name_type, station_name) %>% 
  select(from_station_name, to_station_name) %>% 
  filter(from_station_name != to_station_name) %>% 
  left_join(data_station_locations, by = c("from_station_name" = "station_name")) %>%
  rename(from_latitude = latitude,
         from_longitude = longitude) %>% 
  left_join(data_station_locations, by = c("to_station_name" = "station_name")) %>% 
  rename(to_latitude = latitude,
         to_longitude = longitude) %>% 
  select(from_latitude, from_longitude, to_latitude, to_longitude)

#https://drsimonj.svbtle.com/a-tidy-model-pipeline-with-twidlr-and-broom
#https://cran.r-project.org/web/packages/broom/README.html