#load weather data
library(tidyverse)
library(lubridate)
library(stringr)

data_weather <- list.files(path = "data/weather/", pattern = "weather")
data_weather_dfs <- lapply(paste0("data/weather/", data_weather), read_csv)

for(i in seq_along(data_weather_dfs)) {
  data_weather_dfs[[i]]$wind_chill <- as.numeric(data_weather_dfs[[i]]$wind_chill)
  data_weather_dfs[[i]]$heat_index <- as.numeric(data_weather_dfs[[i]]$heat_index)
}

df_weather <- bind_rows(data_weather_dfs) %>% 
  mutate(date = ymd(str_sub(date, 1, 10)),
         year = year(date),
         month = month(date, label = TRUE)) %>% 
  select(date, year, month, everything())

#df_weather %>% 
#  select(date, temp, rain, precip) %>% 
#  mutate(date = ymd(str_sub(date, 1, 10))) %>% 
#  summary()

#df_weather %>% 
#  mutate(date = ymd(str_sub(date, 1, 10))) %>% 
#  count(date, sort = TRUE)

df_weather <- df_weather %>% 
  select(date, temp, rain, precip) %>% 
  group_by(date) %>%
  summarize(temp_mean = mean(temp, na.rm = TRUE),
            temp_hi = max(temp, na.rm = TRUE),
            temp_lo = min(temp, na.rm = TRUE),
            precip = sum(precip, na.rm = TRUE))
summary(df_weather)

