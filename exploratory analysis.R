library(tidyverse)
library(lubridate)

theme_set(theme_bw())

rm(list = ls())

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


?lubridate
