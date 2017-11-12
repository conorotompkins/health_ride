library(tidyverse)
library(modelr)
library(broom)
library(ggmap)
#install.packages("tidyselect", dependencies = TRUE)

theme_set(theme_minimal())

#rm(list = ls())

source("scripts/load_data_long.R")
source("scripts/load_stations_data.R")

rm(list = c("data", "data_list"))

data_long

df_long <- data_long %>% 
  select(station_name, station_name_type) %>% 
  left_join(data_station_locations)

df_long %>% 
  count(station_name, sort = TRUE) %>% 
  top_n(50) %>% 
  select(station_name) %>% 
  unlist() -> top_stations

df_wide <- data_long %>%
  filter(station_name %in% top_stations) %>% 
  spread(station_name_type, station_name) %>% 
  select(from_station_name, to_station_name) %>% 
  filter(from_station_name != to_station_name) %>% 
  left_join(data_station_locations, by = c("from_station_name" = "station_name")) %>%
  rename(from_latitude = latitude,
         from_longitude = longitude) %>% 
  left_join(data_station_locations, by = c("to_station_name" = "station_name")) %>% 
  rename(to_latitude = latitude,
         to_longitude = longitude) %>% 
  select(from_station_name, to_station_name, from_latitude, from_longitude, to_latitude, to_longitude) %>% 
  na.omit() %>% 
  select(from_longitude, from_latitude, to_longitude, to_latitude) %>% 
  mutate_all(scale)

rm(list = c("data_long", "data_station_locations", "df_long"))

#https://drsimonj.svbtle.com/a-tidy-model-pipeline-with-twidlr-and-broom
#https://cran.r-project.org/web/packages/broom/README.html

kclust <- kmeans(df_wide, 7)
kclust
summary(kclust)

kclusts <- data.frame(k=1:7) %>% group_by(k) %>% do(kclust=kmeans(df_wide, .$k))

clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]])) %>% 
  rename(from_longitude = x1,
         from_latitude= x2,
         to_longitude = x3,
         to_latitude = x4)
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], df_wide))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

ggplot(clusterings, aes(k, tot.withinss)) + 
  geom_line() +
  scale_x_continuous(breaks = 1:10)

p1 <- clusters %>% 
  filter(k >= 2) %>% 
  ggplot(aes(x = from_longitude, xend = to_longitude, y = from_latitude, yend = to_latitude)) + 
  geom_segment(aes(color = cluster, size = size),
               arrow = arrow(length = unit(0.03, "npc"))) + 
  facet_wrap(~ k)
p1
