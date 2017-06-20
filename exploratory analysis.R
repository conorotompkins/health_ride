library(tidyverse)
library(lubridate)

theme_set(theme_bw())

rm(list = ls())

data_list <- list.files(pattern = ".csv")
data_list <- lapply(data_list, read_csv)
data <- bind_rows(data_list)

colnames(data) <- tolower(colnames(data))
data_long <- data %>% 
  mutate(start_date_time = starttime,
         stop_date_time = stoptime) %>%
  select(-c(starttime, stoptime)) %>% 
  gather(date_time_type, date_time, c(start_date_time, stop_date_time)) %>% 
  select(date_time_type, date_time, everything()) %>% 
  mutate(date_time_2 = date_time) %>% 
  separate(date_time, " ", into = c("date", "time")) %>% 
  mutate(date = mdy(date),
         time = hm(time),
         hour = hour(time),
         wday = wday(date, label = TRUE),
         is_weekday = ifelse(wday %in% c("Mon", "Tues", "Wed", "Thurs", "Fri"), "weekday", "weekend")) %>% 
  mutate(from_station_id = `from station id`,
         to_station_id = `to station id`,
         from_station_name = `from station name`,
         to_station_name = `to station name`) %>% 
  gather(location_id_type, location_id, c(from_station_id, to_station_id)) %>% 
  gather(location_name_type, location_name, c(from_station_name, to_station_name)) %>% 
  select(date_time_type, 
         is_weekday, 
         date, 
         time, 
         wday, 
         date_time_2, 
         location_id_type, 
         location_id, 
         location_name_type,
         location_name,
         everything())

table(data$is_weekday, data$wday)

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

data %>% 
  ggplot(aes(hour, color = is_weekday)) +
  geom_freqpoly(stat = "density")

data %>%
  ggplot(aes(hour, color = wday)) +
  geom_freqpoly(stat = "density") +
  facet_wrap(~wday, 
             ncol = 1)




?mutate_at
?separate_rows
?starts_with
?substr
