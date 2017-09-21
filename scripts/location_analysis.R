library(tidyverse)
library(ggmap)

theme_set(theme_bw())

rm(list = ls())

source("scripts/load_data_long.R")
source("scripts/load_stations_data.R")

rm(list = c("data", "data_list"))

df_station_totals <- data_long %>% 
  group_by(station_name) %>% 
  summarize(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  left_join(data_stations)

pgh_map <- get_map(location = "The Hill Pittsburgh, PA", zoom = 13)
pgh_map <- ggmap(pgh_map)

pgh_map +
  geom_point(data = df_station_totals, aes(longitude, latitude, size = number_of_trips),
             alpha = .75) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

df_long <- data_long %>% 
  select(station_name, station_name_type) %>% 
  group_by(station_name, station_name_type) %>% 
  summarize(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  left_join(data_stations)
  
pgh_map +
  geom_point(data = df_long, aes(longitude, latitude, size = number_of_trips, color = station_name_type),
             alpha = .75) +
  scale_size_continuous(range = c(.1, 5)) +
  facet_wrap(~station_name_type) +
  theme_minimal()


df_wide <- data_long %>% 
  spread(station_name_type, station_name) %>% 
  select(from_station_name, to_station_name) %>% 
  left_join(data_stations, by = c("from_station_name" = "station_name")) %>%
  rename(from_latitude = latitude,
         from_longitude = longitude) %>% 
  left_join(data_stations, by = c("to_station_name" = "station_name")) %>% 
  rename(to_latitude = latitude,
         to_longitude = longitude) %>% 
  group_by(from_station_name, to_station_name, from_longitude, from_latitude, to_longitude, to_latitude) %>% 
  summarise(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  mutate(from_station_type = ifelse(from_station_name == to_station_name,
                               "Same station", "Different station"))
  
#to-from map
pgh_map +
  geom_point(data = df_wide, aes(from_longitude, from_latitude)) +
  geom_point(data = df_wide, aes(to_longitude, to_latitude)) +
  geom_segment(data = df_wide, aes(x = from_longitude, xend = to_longitude, y = from_latitude, yend = to_latitude, alpha = number_of_trips, size = number_of_trips)) +
  scale_alpha_continuous(range = c(.05, 1)) +
  scale_size_continuous(range = c(.05, 5)) +
  #facet_wrap(~from_station_name) +
  theme_minimal()
ggsave("images/ride_map.png")

top_from_stations <- df_wide %>% 
  group_by(from_station_name) %>% 
  summarize(number_of_trips = sum(number_of_trips)) %>% 
  arrange(desc(number_of_trips)) %>% 
  top_n(6) %>% 
  select(from_station_name) %>% 
  unlist()

df_wide_day <- data_long %>% 
  spread(station_name_type, station_name) %>% 
  select(from_station_name, to_station_name, is_weekday) %>% 
  left_join(data_stations, by = c("from_station_name" = "station_name")) %>%
  rename(from_latitude = latitude,
         from_longitude = longitude) %>% 
  left_join(data_stations, by = c("to_station_name" = "station_name")) %>% 
  rename(to_latitude = latitude,
         to_longitude = longitude) %>% 
  group_by(is_weekday, from_station_name, to_station_name, from_longitude, from_latitude, to_longitude, to_latitude) %>% 
  summarise(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  mutate(from_station_type = ifelse(from_station_name == to_station_name,
                                    "Same station", "Different station"))


pgh_map +
  geom_point(data = df_wide_day, aes(from_longitude, from_latitude)) +
  geom_point(data = df_wide_day, aes(to_longitude, to_latitude)) +
  geom_segment(data = df_wide, aes(x = from_longitude, xend = to_longitude, y = from_latitude, yend = to_latitude, alpha = number_of_trips, size = number_of_trips)) +
  scale_alpha_continuous(range = c(.05, 1)) +
  scale_size_continuous(range = c(.05, 5)) +
  facet_wrap(~is_weekday) +
  theme_minimal()

#identify mismatch station names
df_station_data <- data_long %>% 
  select(station_name) %>% 
  group_by(station_name) %>% 
  summarize(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips))

df_station_names <- data_stations %>% 
  ungroup() %>% 
  #select(station_name) %>% 
  unique()

df_mismatch <- df_station_data %>% 
  left_join(df_station_names) %>% 
  filter(is.na(latitude))

#i want to color by station_name_type, but still be able to select certain from_stations for faceting
df_station_facet <- df_long %>% 
  ungroup() %>% 
  filter(from_station_name %in% top_from_stations) %>% 
  mutate(from_station_name = factor(from_station_name, levels = top_from_stations))

###### OLD CODE ######




data_wide <- data_long %>% 
  #select(station_name_type, station_name) %>% 
  spread(station_name_type, station_name) %>% 
  left_join(data_stations, by = c("from_station_name" = "station_name")) %>%
  rename(from_latitude = latitude,
         from_longitude = longitude) %>% 
  left_join(data_stations, by = c("to_station_name" = "station_name")) %>% 
  rename(to_latitude = latitude,
         to_longitude = longitude) %>% 
  select(from_station_name, to_station_name, to_longitude, to_latitude, from_longitude, from_latitude)
write_csv(data_wide, "data/data_wide.csv")

df_station_visits <- data_long %>% 
  group_by(station_name) %>% 
  summarize(number_of_visits_total = n())

df_stations <- data_wide %>% 
  group_by(from_station_name, to_station_name, from_longitude, from_latitude, to_longitude, to_latitude) %>% 
  summarise(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  mutate(station_type = ifelse(from_station_name == to_station_name,
                                "Same station", "different station")) %>% 
  left_join(df_station_visits, by = c("from_station_name" = "station_name"))






pgh_map +
  geom_point(data = df_stations, aes(from_longitude, from_latitude)) +
  geom_point(data = df_stations, aes(to_longitude, to_latitude)) +
  geom_segment(data = df_stations, aes(x = from_longitude, xend = to_longitude, y = from_latitude, yend = to_latitude, alpha = number_of_trips, size = number_of_trips)) +
  scale_alpha_continuous(range = c(.01, 1)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()
ggsave("images/ride_map.png")



pgh_map +
  geom_point(data = df_station_facet, aes(from_longitude, from_latitude, size = number_of_trips, color = station_type)) +
  geom_point(data = df_station_facet, aes(to_longitude, to_latitude, size = number_of_trips, color = station_type)) +
  geom_segment(data = df_station_facet, aes(x = from_longitude, xend = to_longitude, y = from_latitude, yend = to_latitude, alpha = number_of_trips)) +
  scale_alpha_continuous(range = c(.5, 1)) +
  facet_wrap(~from_station_name) +
  theme_minimal()
ggsave("images/ride_map_top_6_faceted.png")

#need to move to long data format to get color of points correct. 
