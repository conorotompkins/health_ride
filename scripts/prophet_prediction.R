library(tidyverse)
library(lubridate)
library(prophet)

source("scripts/load_data_long.R")

df <- data_long %>% 
  select(date) %>% 
  mutate(ds = ymd(date)) %>% 
  count(ds) %>% 
  rename(y = n)
df

m <- prophet(df)

set_periods <- 365

future <- make_future_dataframe(m, periods = set_periods, freq = "day")
tail(future)
tail(df)

forecast <- predict(m, future) %>% 
  as_tibble()
forecast

plot(m, forecast)
prophet_plot_components(m, forecast)

forecast %>% 
  left_join(df)

glimpse(forecast)
glimpse(df)

forecast %>% 
  ggplot(aes(ds)) +
  geom_line(aes(y = trend), color = "black") +
  geom_line(aes(y = yhat), color = "red")

forecast %>% 
  ggplot(aes(trend, yhat)) +
  geom_point()
