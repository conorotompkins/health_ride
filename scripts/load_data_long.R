library(tidyverse)
library(lubridate)

theme_set(theme_bw())

rm(list = ls())
data_path <- "./data/ride_data"
data_list <- list.files(path = data_path, pattern = ".csv")
data_list <- paste0(data_path, "/", data_list)
data_list <- lapply(data_list, read_csv)
data <- bind_rows(data_list)

colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub(" ", "_", colnames(data))

data_long <- data %>% 
  mutate(start_date_time = starttime,
         stop_date_time = stoptime) %>%
  select(-c(starttime, stoptime)) %>% 
  gather(date_time_type, date_time, c(start_date_time, stop_date_time)) %>% 
  select(date_time_type, date_time, everything()) %>% 
  mutate(date_time_2 = date_time) %>% 
  separate(date_time, " ", into = c("date", "time")) %>% 
  mutate(id = row_number(),
         date = mdy(date),
         year = year(date),
         month = month(date, label = TRUE),
         week = week(date),
         time = hm(time),
         hour = hour(time),
         wday = wday(date, label = TRUE),
         is_weekday = ifelse(wday %in% c("Mon", "Tues", "Wed", "Thurs", "Fri"), "weekday", "weekend"),
         yday = yday(date),
         mday = mday(date)) %>% 
  arrange(date) %>% 
  mutate(trip_duration = (tripduration / 60) / 60) %>% 
  gather(station_id_type, station_id, c(from_station_id, to_station_id)) %>% 
  gather(station_name_type, station_name, c(from_station_name, to_station_name)) %>% 
  select(date_time_type, 
         is_weekday, 
         date, 
         year,
         month,
         time, 
         hour,
         wday,
         yday,
         mday,
         date_time_2, 
         station_id_type, 
         station_id, 
         station_name_type,
         station_name,
         everything())
