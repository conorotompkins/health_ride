#install.packages("ggraph")

library(tidyverse)
library(ggraph)
library(igraph)

set.seed(1234)

data_wide <- data_long %>% 
  spread(location_name_type, location_name)

network_data <- data_wide %>% 
  select(from_station_name, to_station_name) %>% 
  count(from_station_name, to_station_name, sort = TRUE) %>% 
  mutate(from_label = ifelse(n > 500, from_station_name, ""))

graph <- graph_from_data_frame(network_data)

#V(graph)$node_label <- network_data$from_label

graph <- graph %>% 
  set_vertex_attr("label", value = network_data$from_label)

ggraph(graph, "kk") + 
  geom_edge_link(aes(edge_alpha = n,
                     edge_width = n)) +
  geom_node_point() +
  geom_node_label(aes(label = node_label))

?geom_node_label
    
?geom_edge_link