#load weather data

data_weather <- list.files(path = "data/weather/", pattern = "weather")
data_weather_dfs <- lapply(paste0("data/weather/", data_weather), read_csv)

for(i in seq_along(data_weather_dfs)) {
  data_weather_dfs[[i]]$wind_chill <- as.numeric(data_weather_dfs[[i]]$wind_chill)
  data_weather_dfs[[i]]$heat_index <- as.numeric(data_weather_dfs[[i]]$heat_index)
}

df_weather <- bind_rows(data_weather_dfs)