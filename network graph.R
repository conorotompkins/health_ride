#install.packages("ggraph")

library(tidyverse)
library(ggraph)
library(igraph)

set.seed(1234)

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

ggraph(simple_network) +
  geom_edge_link(aes(width = n)) +
  geom_node_label(aes(label = name)) +
  theme_graph()
  
  