library(tidyverse)
library(lubridate)
library(stringr)
library(rwunderground)
rwunderground::set_api_key("52adb4b045871a47")

history(set_location(zip_code = "15222"), date = 20170701)


data_weather_2015 <- history_range(set_location(zip_code = "15222"), date_start = "20150531", date_end = "20151231", limit = 10)
write_csv(data_weather_2015, "data/weather/data_weather_2015")

data_wather_2016 <- history_range(set_location(zip_code = "15222"), date_start = "20160101", date_end = "20161231", limit = 10)
data_weather_2016 <- data_wather_2016
write_csv(data_weather_2016, "data/weather/data_weather_2016")

data_weather_2017 <- history_range(set_location(zip_code = "15222"), date_start = "20170101", date_end = "20170131", limit = 10)
write_csv("data/weather/data_weather_2017")

df_weather <- bind_rows(data_weather_2015, data_weather_2016)


df_weather_2015 <- as_tibble(df_weather_2015)
df_weather_2015 <- df_weather_2015 %>% 
  mutate(date = str_sub(date, 1, 8))

df_weather_2015
?stringr::str_sub()