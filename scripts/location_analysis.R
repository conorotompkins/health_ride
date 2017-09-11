install.packages("ggmap")

library(tidyverse)
library(ggmap)

theme_set(theme_bw())

rm(list = ls())

source("scripts/load_data_long.R")
source("scripts/load_stations_data.R")

rm(list = c("data", "data_list"))

data_wide <- data_long %>% 
  select(-c(latitude, longitude)) %>% 
  #select(location_name_type, location_name) %>% 
  spread(location_name_type, location_name) %>% 
  left_join(locations, by = c("from_station_name" = "station_name")) %>%
  rename(from_latitude = latitude,
         from_longitude = longitude) %>% 
  left_join(locations, by = c("to_station_name" = "station_name")) %>% 
  rename(to_latitude = latitude,
         to_longitude = longitude) %>% 
  select(from_station_name, to_station_name, to_longitude, to_latitude, from_longitude, from_latitude)


df_locations <- data_wide %>% 
  group_by(from_station_name, to_station_name, from_longitude, from_latitude, to_longitude, to_latitude) %>% 
  summarise(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips))

pgh_map <- get_map(location = "The Hill Pittsburgh, PA", zoom = 13)
pgh_map <- ggmap(pgh_map)



pgh_map +
  geom_point(data = df_locations, aes(from_longitude, from_latitude)) +
  geom_point(data = df_locations, aes(to_longitude, to_latitude)) +
  geom_segment(data = df_locations, aes(x = from_longitude, xend = to_longitude, y = from_latitude, yend = to_latitude, alpha = number_of_trips, size = number_of_trips)) +
  scale_alpha_continuous(range = c(.01, 1)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

df_location_test <- df_locations %>% 
  filter(from_station_name == "Liberty Ave & Stanwix St")

pgh_map +
  geom_point(data = df_location_test, aes(from_longitude, from_latitude)) +
  geom_point(data = df_location_test, aes(to_longitude, to_latitude, size = number_of_trips)) +
  geom_segment(data = df_location_test, aes(x = from_longitude, xend = to_longitude, y = from_latitude, yend = to_latitude, alpha = number_of_trips)) +
  scale_alpha_continuous(range = c(.5, 1)) +
  #scale_size_continuous(range = c(.1, 3)) +
  #coord_cartesian() +
  theme_minimal()


#think about faceting by month, wday, hour
?arrow
?geom_segment
?geom_curve
