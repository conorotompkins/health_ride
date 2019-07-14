library(tidyverse)
library(lubridate)
library(prophet)
library(janitor)

options(scipen = 999)

theme_set(theme_bw())

source("scripts/load_data.R")
  
df <- df %>% 
  count(date) %>% 
  rename(ds = date,
         y = n) %>% 
  mutate(floor = 0,
         cap = 10^4)
  
glimpse(df)

m <- prophet(df, growth = 'logistic')

set_periods <- 365

future <- make_future_dataframe(m, periods = set_periods, freq = "day") %>% 
  mutate(floor = 0,
         cap = 10^4)
tail(future)
tail(df)

forecast <- predict(m, future) %>% 
  as_tibble()
forecast

plot(m, forecast)
prophet_plot_components(m, forecast)

forecast %>% 
  ggplot(aes(yhat)) +
  geom_freqpoly()

df_aug <- forecast %>% 
  mutate(ds = ymd(ds)) %>% 
  left_join(df) %>% 
  mutate(.resid = y - yhat)

df_aug %>% 
  ggplot(aes(x = ds)) +
    geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = .2, fill = "blue") +
    geom_line(aes(y = yhat), color = "blue") +
    geom_smooth(aes(y = yhat), color = "red") +
    geom_point(aes(y = y)) +
    geom_hline(aes(yintercept = unique(df_aug$cap)), linetype = 2) +
    geom_hline(aes(yintercept = unique(df_aug$floor)), linetype = 2)

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

#df_cv <- cross_validation(m, initial = 365, period = 180, horizon = 365, units = 'days')
df_cv <- cross_validation(m, horizon = 30, units = 'days')

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
  count(ds, sort = TRUE)

df_cv %>% 
  ggplot(aes(x = ds)) +
  geom_line(aes(y = yhat), color = "blue")
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha = .3) +
  geom_point(aes(y = y)) +
  geom_vline(xintercept = as.numeric(as_datetime(unique(df_cv$cutoff))), size = .5)

plot_cross_validation_metric(df_cv, metric = "mape")

test <- predictive_samples(m, df) %>% 
  as_tibble()
test
