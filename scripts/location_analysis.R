library(tidyverse)
library(ggmap)
library(gghighlight)

theme_set(theme_minimal())

rm(list = ls())

source("scripts/load_data_long.R")
source("scripts/load_stations_data.R")

rm(list = c("data", "data_list"))

data_long

df_station_totals <- data_long %>% 
  group_by(year, quarter, station_name) %>% 
  summarize(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips), station_name) %>% 
  left_join(data_station_locations) %>% 
  select(year, quarter, station_name, number_of_trips, longitude, latitude)

df_station_totals

pgh_map <- get_map(location = "The Hill Pittsburgh, PA", zoom = 13)
pgh_map <- ggmap(pgh_map)

pgh_map +
  geom_point(data = df_station_totals, aes(longitude, latitude, size = number_of_trips),
             alpha = .75) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

df_long <- data_long %>% 
  select(station_name, station_name_type) %>% 
  group_by(station_name, station_name_type) %>% 
  summarize(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  left_join(data_station_locations)

#df_from_to <- df_long %>% 
#  spread(station_name_type, number_of_trips) %>% 
#  rename(from_trips = from_station_name,
#         to_trips = to_station_name) %>% 
#  select(from_trips, to_trips) %>% 
#  mutate(differential = abs(from_trips - to_trips))

#use the stations with large differentials as jumping off points in the network map
#df_from_to %>% 
#  gghighlight_point(aes(from_trips, to_trips), label_key = station_name, differential > 4000) +
#  scale_x_continuous(limits = c(0, 45000)) +
#  scale_y_continuous(limits = c(0, 45000)) +
#  coord_equal() +
#  geom_abline() +
#  theme_bw()
  
pgh_map +
  geom_point(data = df_long, aes(longitude, latitude, size = number_of_trips, color = station_name_type),
             alpha = .75) +
  scale_size_continuous(range = c(.1, 5)) +
  facet_wrap(~station_name_type) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

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
  group_by(from_station_name, to_station_name, from_longitude, from_latitude, to_longitude, to_latitude) %>% 
  summarise(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips)) %>% 
  mutate(from_station_type = ifelse(from_station_name == to_station_name,
                               "Same station", "Different station"))
  
#to-from map
pgh_map +
  geom_point(data = df_wide, aes(from_longitude, from_latitude)) +
  geom_point(data = df_wide, aes(to_longitude, to_latitude)) +
  geom_segment(data = df_wide, aes(x = from_longitude, xend = to_longitude, 
                                   y = from_latitude, yend = to_latitude, 
                                   alpha = number_of_trips, size = number_of_trips)) +
  scale_alpha_continuous(range = c(.05, .3)) +
  scale_size_continuous(range = c(.05, 3)) +
  #facet_wrap(~from_station_name) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())
#ggsave("images/ride_map.png")

top_from_stations <- df_wide %>% 
  filter(from_station_name != to_station_name) %>% 
  group_by(from_station_name) %>% 
  summarize(number_of_trips = sum(number_of_trips)) %>% 
  arrange(desc(number_of_trips)) %>% 
  top_n(6) %>% 
  select(from_station_name) %>% 
  unlist()

df_wide_specific_station <- df_wide %>% 
  filter(from_station_name %in% top_from_stations)

pgh_map +
  geom_point(data = df_wide_specific_station, aes(from_longitude, from_latitude), show.legend = FALSE) +
  geom_point(data = df_wide_specific_station, aes(to_longitude, to_latitude), shape = 1, size = 5, show.legend = FALSE) +
  geom_segment(data = df_wide_specific_station, aes(x = from_longitude, xend = to_longitude, 
                                                    y = from_latitude, yend = to_latitude, 
                                                    alpha = number_of_trips, size = number_of_trips),
               arrow = arrow(length = unit(0.03, "npc"))) +
  scale_alpha_continuous(range = c(.3, .5)) +
  scale_size_continuous(range = c(.05, 3)) +
  facet_wrap(~from_station_name,
             ncol = 3) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

df_wide_day <- data_long %>% 
  spread(station_name_type, station_name) %>% 
  select(from_station_name, to_station_name, is_weekday) %>% 
  filter(from_station_name != to_station_name) %>% 
  left_join(data_station_locations, by = c("from_station_name" = "station_name")) %>%
  rename(from_latitude = latitude,
         from_longitude = longitude) %>% 
  left_join(data_station_locations, by = c("to_station_name" = "station_name")) %>% 
  rename(to_latitude = latitude,
         to_longitude = longitude) %>% 
  group_by(is_weekday, from_station_name, to_station_name, from_longitude, from_latitude, to_longitude, to_latitude) %>% 
  summarise(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips))

#doesnt appear to be a difference, maybe because on weekends people to round-trips. those dont appear in this dataset
pgh_map +
  geom_point(data = df_wide_day, aes(from_longitude, from_latitude)) +
  geom_point(data = df_wide_day, aes(to_longitude, to_latitude)) +
  geom_segment(data = df_wide, aes(x = from_longitude, xend = to_longitude, 
                                   y = from_latitude, yend = to_latitude, 
                                   alpha = number_of_trips, size = number_of_trips)) +
  scale_alpha_continuous(range = c(.05, .3)) +
  scale_size_continuous(range = c(.05, 3)) +
  facet_wrap(~is_weekday) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

df_wide_tod <- data_long %>% 
  spread(station_name_type, station_name) %>% 
  select(from_station_name, to_station_name, hour) %>% 
  filter(from_station_name != to_station_name) %>%
  mutate(time_of_day = cut(hour, breaks = c(-Inf, 3, 6, 9, 12, 15, 18, 21, Inf), 
                           labels = c("0-3", "3-6", "6-9", "9-12", "12-15", "15-18", "18-21", "21-24"), 
                           ordered_result = TRUE)) %>% 
  left_join(data_station_locations, by = c("from_station_name" = "station_name")) %>%
  rename(from_latitude = latitude,
         from_longitude = longitude) %>% 
  left_join(data_station_locations, by = c("to_station_name" = "station_name")) %>% 
  rename(to_latitude = latitude,
         to_longitude = longitude) %>% 
  group_by(time_of_day, from_station_name, to_station_name, 
           from_longitude, from_latitude, 
           to_longitude, to_latitude) %>% 
  summarise(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips))

pgh_map +
  geom_point(data = df_wide_tod, aes(from_longitude, from_latitude)) +
  geom_point(data = df_wide_tod, aes(to_longitude, to_latitude)) +
  geom_segment(data = df_wide_tod, aes(x = from_longitude, xend = to_longitude, 
                                   y = from_latitude, yend = to_latitude, 
                                   alpha = number_of_trips, size = number_of_trips)) +
  scale_alpha_continuous(range = c(.05, .3)) +
  scale_size_continuous(range = c(.05, 3)) +
  facet_wrap(~time_of_day) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())
  
  


#identify mismatch station names
df_station_data <- data_long %>% 
  select(station_name) %>% 
  group_by(station_name) %>% 
  summarize(number_of_trips = n()) %>% 
  arrange(desc(number_of_trips))

df_station_names <- data_station_locations %>% 
  ungroup() %>% 
  #select(station_name) %>% 
  unique()

df_mismatch <- df_station_data %>% 
  left_join(df_station_names) %>% 
  filter(is.na(latitude))