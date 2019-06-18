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

df_aug <- forecast %>% 
  mutate(ds = ymd(ds)) %>% 
  left_join(df) %>% 
  mutate(.resid = y - yhat)

df_aug %>% 
  ggplot(aes(x = ds)) +
    geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = .2, fill = "blue") +
    geom_line(aes(y = yhat), color = "blue") +
    geom_smooth(aes(y = yhat), color = "red") +
    geom_point(aes(y = y))

max <- 8000

df_aug %>%
  ggplot(aes(y, yhat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_equal(xlim = c(0, max), ylim = c(0, max))

df_aug %>% 
  ggplot(aes(.resid)) +
  geom_density()

glimpse(forecast)
glimpse(df)

df_cv <- cross_validation(m, initial = 730, period = 180, horizon = 365, units = 'days')
performance_metrics(df_cv) %>% 
  as_tibble() %>% 
  ggplot(aes(horizon, rmse)) +
  geom_point()
plot_cross_validation_metric(df_cv, metric = "mape")
