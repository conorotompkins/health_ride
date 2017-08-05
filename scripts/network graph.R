#install.packages("ggraph")

library(tidyverse)
library(ggraph)
library(igraph)

theme_set(theme_graph())
set_graph_style(foreground = 'grey80')

set.seed(1)

data_wide <- data_long %>% 
  spread(location_name_type, location_name)

top_locations <- data %>% 
  select(from_station_name) %>%
  count(from_station_name, sort = TRUE) %>% 
  top_n(3, n) %>% 
  select(from_station_name) %>% 
  unlist()

network_data <- data_wide %>% 
  select(from_station_name, to_station_name) %>% 
  filter(from_station_name %in% top_locations) %>% 
  count(from_station_name, to_station_name, sort = TRUE)

network_data %>% 
  select(from_station_name) %>% 
  unique()


graph <- graph_from_data_frame(network_data)

ggraph(graph) + 
  geom_edge_fan(aes(edge_alpha = n,
                     edge_width = n)) +
  geom_edge_loop() +
  geom_node_point() +
  geom_node_label(aes(label = name)) +
  scale_edge_alpha_continuous(range = c(.5, 1)) +
  theme_graph()

#?geom_edge_
simple_network <- data_wide %>% 
  select(from_station_name, to_station_name) %>% 
  mutate(is_same = ifelse(from_station_name == to_station_name, "same_location", "different_location")) %>%
  group_by(is_same) %>% 
  count() %>% 
  spread(is_same, n) %>% 
  gather(location_type, n) %>% 
  mutate(from_location = "from_location") %>% 
  select(from_location, location_type, n) %>% 
  graph_from_data_frame()

#get rid of "same location". try just having a loop
ggraph(simple_network, layout = "graphopt") +
  geom_edge_diagonal(aes(width = n),
                     arrow = arrow(length = unit(3, 'mm')), 
                     end_cap = circle(10, 'mm')) +
  geom_node_label(aes(label = name)) +
  scale_edge_width_continuous(range = c(1, 3)) +
  labs(title = "Do most trips end at a different Healthy Ride station?")

top_locations <- data %>% 
  select(from_station_name) %>%
  count(from_station_name, sort = TRUE) %>% 
  top_n(10, n) %>% 
  select(from_station_name) %>% 
  unlist()

top_locations[11:12] <- c("Same station", "Different station")

#from_station_name = ifelse(from_station_name %in% top_locations, from_station_name, "the_rest")

simple_network_2 <- data_wide %>% 
  select(from_station_name, to_station_name, is_weekday) %>% 
  mutate(is_same = ifelse(from_station_name == to_station_name, "Same station", "Different station")) %>%
  mutate(from_location = "from_location") %>% 
  select(from_station_name, is_same, is_weekday) %>% 
  #filter(from_station_name %in% top_locations) %>% 
  group_by(from_station_name, is_same, is_weekday) %>% 
  summarize(number_of_rides = n()) %>% 
  graph_from_data_frame()

data_wide %>% 
  select(from_station_name, to_station_name) %>% 
  mutate(is_same = ifelse(from_station_name == to_station_name, "Same station", "Different station")) %>%
  mutate(from_location = "from_location") %>% 
  select(from_station_name, is_same) %>% 
  group_by(from_station_name, is_same) %>% 
  summarize(number_of_rides = n()) %>% 
  select(from_station_name) %>% 
  unique()


#maybe people that start in The Strip and end somewhere else are parking in the strip and commuting downtowm?
#facet by weekday/weekend
#people might park at a bike trailhead, bike up and back to the same station, and leave. this could explain some heavy loops
ggraph(simple_network_2, layout = "dh") +
  geom_edge_diagonal(aes(edge_alpha = number_of_rides)) +
  #geom_edge_density(aes(fill = is_same)) + 
  geom_node_label(aes(label = ifelse(name %in% top_locations,
                                     V(simple_network_2)$name, "")),
                  size = 3) +
  scale_edge_alpha_continuous(range = c(.1, 1)) +
  facet_edges(~is_weekday,
              ncol = 2) +
    labs(title = "Do most trips start and end at different Healthy Ride stations?",
       subtitle = "Only stations in the top 10 in terms of number of rides are labeled",
       caption = "@conor_tompkins, data from wprdc.org")
ggsave("healthy ride simple network is_weekday.png", width = 20, height = 10)
