library(tidyverse)
library(lubridate)
library(viridis)

theme_set(theme_bw())

rm(list = ls())

source("load_data.R")
data_long

table(data_long$is_weekday, data_long$wday)

data_long %>% 
  ggplot(aes(date, color = date_time_type)) +
  geom_freqpoly(stat = "density") +
  facet_wrap(~date_time_type,
             ncol = 1)
data_long %>% 
  ggplot(aes(hour, color = date_time_type)) +
  geom_freqpoly(stat = "density") +
  facet_wrap(~date_time_type,
             ncol = 1)

data_long %>% 
  ggplot(aes(hour, color = is_weekday)) +
  geom_freqpoly(stat = "density")

#why are the weekends so weird?
data_long %>%
  ggplot(aes(hour, color = wday)) +
  geom_freqpoly(stat = "density") +
  scale_x_continuous(breaks = c(0:23))

data_long %>%
  ggplot(aes(hour, color = wday)) +
  geom_freqpoly() +
  scale_x_continuous(breaks = c(0:23))

data_long %>%
  ggplot(aes(hour, color = is_weekday)) +
  geom_freqpoly(stat = "density") +
  scale_x_continuous(breaks = c(0:23))

data_long %>%
  ggplot(aes(hour, color = is_weekday)) +
  geom_freqpoly() +
  scale_x_continuous(breaks = c(0:23))



data_long %>% 
  filter(location_name_type == "from_station_name") %>% 
  arrange(date) %>% 
  count(date) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  ggplot(aes(date, cum_sum)) +
  geom_line()

data_long %>%
  select(year, yday) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year, yday) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  ggplot(aes(yday, cum_sum,
             color = year,
             group = year)) +
  geom_line()

data_long %>%
  select(month, mday) %>% 
  #mutate(year = as.factor(year)) %>% 
  group_by(month, mday) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(month) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  ggplot(aes(mday, cum_sum,
             color = month,
             group = month)) +
  geom_line()

data_long %>%
  select(year, month, mday) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year, month, mday) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(year, month) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  ggplot(aes(mday, cum_sum,
             color = month)) +
  geom_line() +
  facet_wrap(~year,
             ncol = 1)

#trying violin plot  with Chrissie loves and snuggles. VERY INEFFECTIVE. MUCH DISTRATCTION. SO SNUGGLY.
data_long %>%
  filter(year == 2016) %>% 
  group_by(date) %>% 
  mutate(n = n()) %>% 
  ggplot(aes(x = week, y = n, fill = is_weekday)) +
  geom_point(alpha = .5) +
  geom_violin() +
  facet_wrap(~is_weekday, 
             ncol = 2)

dev.off()

data_long %>% 
  group_by(year, month, mday, hour) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(mday, hour, fill = n)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  facet_grid(year ~ month) +
  coord_equal() +
  scale_fill_viridis()

data_long %>% 
  ggplot(aes(x = week, group = id)) +
  geom_boxplot(stat = "count")
  ?geom_boxplot

data_long %>% 
  ggplot(aes(week, tripduration, group = id)) +
  geom_boxplot()

data_long %>%
  select(date, trip_duration) %>% 
  group_by(date) %>% 
  summarize(trip_duration_sum = sum(trip_duration)) %>% 
  mutate(cum_sum = cumsum(trip_duration_sum)) %>% 
  ggplot(aes(date, cum_sum)) +
  geom_line()

data_long %>%
  select(year, month, mday, hour, trip_duration) %>% 
  group_by(year, month, mday, hour) %>% 
  summarize(trip_duration_sum = sum(trip_duration)) %>% 
ggplot(aes(mday, hour, fill = trip_duration_sum)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  facet_grid(year ~ month) +
  coord_equal() +
  scale_fill_viridis()

data_long %>%
  select(year, month, mday, hour, trip_duration) %>% 
  group_by(year, month, mday, hour) %>% 
  summarize(trip_duration_sum = sum(trip_duration)) %>% 
  ggplot(aes(mday, hour, fill = trip_duration_sum)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  facet_grid(year ~ month) +
  coord_equal() +
  scale_fill_viridis()
