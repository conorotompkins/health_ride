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

data_weather_2017 <- history_range(set_location(zip_code = "15222"), date_start = "20170101", date_end = "20171031", limit = 10)
write_csv(data_weather_2017, "data/weather/data_weather_2017")


data_weather <- list.files(path = "data/weather/", pattern = "weather")
data_weather_dfs <- lapply(paste0("data/weather/", data_weather), read_csv)

for(i in seq_along(data_weather_dfs)) {
  data_weather_dfs[[i]]$wind_chill <- as.numeric(data_weather_dfs[[i]]$wind_chill)
  data_weather_dfs[[i]]$heat_index <- as.numeric(data_weather_dfs[[i]]$heat_index)
}

df_weather <- bind_rows(data_weather_dfs)


df_weather_2015 <- as_tibble(df_weather_2015)
df_weather_2015 <- df_weather_2015 %>% 
  mutate(date = str_sub(date, 1, 8))

df_weather_2015
?stringr::str_sub()
?rwunderground::
