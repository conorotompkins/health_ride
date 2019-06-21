library(tidyverse)
library(lubridate)
library(prophet)
library(janitor)
library(hrbrthemes)

options(scipen = 999)

theme_set(theme_ipsum())

source("scripts/load_data.R")
  
df <- df %>% 
  count(date) %>% 
  rename(ds = date,
         y = n)
  
glimpse(df)

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
  geom_smooth(method = "lm") #+
  #coord_equal(xlim = c(0, max), ylim = c(0, max))

df_aug %>% 
  ggplot(aes(.resid)) +
  geom_density()

glimpse(forecast)
glimpse(df)

df_cv <- cross_validation(m, initial = 365*2, period = 180, horizon = 365, units = 'days')

df_cv

performance_metrics(df_cv) %>% 
  as_tibble() %>% 
  gather(metric, measure, -horizon) %>% 
  ggplot(aes(horizon, measure)) +
  geom_line() +
  facet_wrap(~metric, scales = "free_y",
             ncol = 1)

unique(df_cv$cutoff)

df_cv %>% 
  ggplot(aes(ds,)) +
  geom_line(aes(y = yhat), color = "blue") +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha = .3) +
  geom_point(aes(y = y)) +
  geom_vline(xintercept = as.numeric(as_datetime(unique(df_cv$cutoff))), size = 2)

plot_cross_validation_metric(df_cv, metric = "mape")

test <- predictive_samples(m, df) %>% 
  as_tibble()
