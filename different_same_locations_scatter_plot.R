spread_locations <- data_wide %>% 
  select(from_station_name, to_station_name, is_weekday) %>% 
  mutate(is_same = ifelse(from_station_name == to_station_name, "Same station", "Different station")) %>%
  mutate(from_location = "from_location") %>% 
  select(from_station_name, is_same, is_weekday) %>% 
  group_by(from_station_name, is_same, is_weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  spread(is_same, number_of_rides) %>% 
  mutate(different_station = `Different station`,
         same_station = `Same station`,
         is_top = from_station_name %in% top_locations,
         sum_rides = sum(different_station, same_station),
         ratio = different_station / sum_rides,
         diff = ratio - .5,
         diff_abs = abs(diff))

spread_locations %>% 
  ggplot(aes(different_station, same_station,
             label = ifelse(is_top, from_station_name, NA),
             color = diff)) +
  geom_point() +
  geom_label() +
  geom_abline() +
  coord_equal() +
  facet_wrap(~is_weekday) +
  scale_x_continuous(limits = c(0, 13000)) +
  scale_y_continuous(limits = c(0, 13000)) +
  theme_bw()