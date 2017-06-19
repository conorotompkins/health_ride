library(tidyverse)
library(lubridate)

theme_set(theme_bw())

rm(list = ls())

data_list <- list.files(pattern = ".csv")
data_list <- lapply(data_list, read_csv)
data <- bind_rows(data_list)

colnames(data) <- tolower(colnames(data))
data <- data %>% 
  mutate(start_date_time = starttime,
         stop_date_time = stoptime) %>%
  select(-c(starttime, stoptime)) %>% 
  gather(date_time_type, date_time, c(start_date_time, stop_date_time)) %>% 
  select(date_time_type, date_time, everything()) %>% 
  mutate(date_time_2 = date_time) %>% 
  separate(date_time, " ", into = c("date", "time")) %>% 
  mutate(date = mdy(date),
         time = hm(time),
         hour = hour(time)) %>% 
  select(date_time_type, date, time, date_time_2, everything())

data %>% 
  ggplot(aes(date, color = date_time_type)) +
  geom_freqpoly(stat = "density") +
  facet_wrap(~date_time_type,
             ncol = 1)
data %>% 
  ggplot(aes(hour, color = date_time_type)) +
  geom_freqpoly(stat = "density") +
  facet_wrap(~date_time_type,
             ncol = 1)


?mutate_at
?separate_rows
?starts_with
?substr
