library(tidyverse)
library(lubridate)
library(viridis)

theme_set(theme_bw())

rm(list = ls())

source("scripts/load_data_long.R")
data_long
rm(data)

table(data_long$is_weekday, data_long$wday)

data_long %>% 
  filter(date_time_type == "start_date_time") %>% 
  ggplot(aes(date, color = date_time_type)) +
  geom_freqpoly(stat = "density")

data_long %>% 
  filter(date_time_type == "start_date_time") %>% 
  ggplot(aes(hour)) +
  geom_freqpoly(stat = "count")

data_long %>% 
  filter(date_time_type == "start_date_time") %>% 
  ggplot(aes(hour, color = is_weekday)) +
  geom_freqpoly(stat = "count")

#why are the weekends so weird?
data_long %>%
  filter(date_time_type == "start_date_time") %>% 
  ggplot(aes(hour, color = wday)) +
  geom_freqpoly(stat = "count") +
  scale_x_continuous(breaks = c(0:23))

#duplicate
data_long %>%
  filter(date_time_type == "start_date_time") %>% 
  ggplot(aes(hour, color = wday)) +
  geom_freqpoly(binwidth = 1) +
  scale_x_continuous(breaks = c(0:23))

data_long %>%
  filter(date_time_type == "start_date_time") %>% 
  ggplot(aes(hour, color = is_weekday)) +
  geom_freqpoly(stat = "count") +
  scale_x_continuous(breaks = c(0:23))

data_long %>%
  filter(date_time_type == "start_date_time") %>% 
  select(hour, is_weekday) %>% 
  ggplot(aes(hour, color = is_weekday)) +
  geom_freqpoly(bindwidth = 10) +
  scale_x_continuous(breaks = c(0:23))



data_long %>% 
  filter(station_name_type == "from_station_name") %>% 
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
#takes forever to load and is wrong
#data_long %>%
#  filter(year == 2016) %>% 
#  group_by(date) %>% 
#  mutate(n = n()) %>% 
#  ggplot(aes(x = week, y = n, fill = is_weekday)) +
#  geom_point(alpha = .5) +
#  geom_violin() +
#  facet_wrap(~is_weekday, 
#             ncol = 2)

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

#takes a long time to run
data_long %>% 
  ggplot(aes(x = week, group = id)) +
  geom_boxplot(stat = "count")


#data_long %>% 
#  ggplot(aes(week, tripduration, group = id)) +
#  geom_boxplot()

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

data_long %>% 
  ggplot(aes(trip_duration)) +
  #geom_density() +
  #geom_histogram(bins = 100) +
  geom_freqpoly(binwidth = 1)

locations_sd <- data_long %>% 
  group_by(location_name) %>% 
  summarize(location_sd = sd(trip_duration)) %>% 
  arrange(desc(location_sd)) %>% 
  select(location_name) %>% 
  unlist()

data_long %>% 
  filter(trip_duration >= 1) %>% 
  mutate(location_name = factor(location_name, levels = locations_sd)) %>% 
  ggplot(aes(trip_duration)) +
  #geom_density() +
  #geom_histogram(bins = 100) +
  geom_freqpoly(binwidth = 1) +
  geom_vline(xintercept = 24, color = "red", linetype = 2) +
  #coord_cartesian(ylim = c(0, 10^4)) +
  facet_wrap(~location_name)

summary(data_long$trip_duration)

data_long %>% 
  mutate(trip_duration = round(trip_duration, digits = 0)) %>% 
  count(month, location_name, trip_duration) %>% 
  ggplot(aes(location_name, trip_duration, fill = log10(n))) +
  geom_tile() + 
  facet_wrap(~month,
             nrow = 1) +
  coord_equal() +
  theme(axis.text.x = element_blank())
