library(tidyverse)
library(modelr)
library(broom)

options(scipen = 99)

source("scripts/load_data_long.R")
source("scripts/load_data_wide.R")
rm(list = c("data", "data_long"))

###Daily models
df_holidays <- read_csv("data/holidays.csv") %>% 
  mutate(date = mdy(date))
df_holidays

df_daily <- data_wide %>% 
  mutate(month = factor(month,  ordered = FALSE),
         wday = factor(wday, ordered = FALSE)) %>% 
  #filter(station_name_type == "from_station_name") %>% 
  #filter(!is.na(usertype)) %>% 
  group_by(date, year, yday, month, wday) %>% 
  summarize(number_of_rides = n()) %>% 
  left_join(df_holidays) %>% 
  replace_na(replace = list(holiday = "none"))
df_daily

model1 <- lm(number_of_rides ~ month + wday, data = df_daily)
model2 <- lm(number_of_rides ~ month * wday, data = df_daily)
model3 <- lm(number_of_rides ~ year + month * wday, data = df_daily)
model4 <- lm(number_of_rides ~ year * month * wday + holiday, data = df_daily)
model5 <- lm(number_of_rides ~ year * month * wday * holiday, data = df_daily)

glance(model1)
glance(model2)
glance(model3)
glance(model4)
glance(model5)

tidy(model1)
tidy(model2)
tidy(model3)
tidy(model4)
tidy(model5)

df_daily %>%
  add_predictions(model5)

df_daily_pred <- df_daily %>% 
  gather_predictions(model1, model2, model3, model4, model5) %>% 
  rename(predicted = pred)

df_daily_resid <- df_daily %>%
  gather_residuals(model1, model2, model3, model4, model5) %>% 
  rename(residual = resid)


#plotting daily models
df_daily_resid %>% 
  ggplot(aes(date, residual, color = model)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .1) +
  facet_wrap(~model, nrow = 1)

df_daily_resid %>%
  ggplot(aes(number_of_rides, residual, color = model)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm") +
  facet_wrap(~model,
             nrow = 1)

df_daily_pred %>% 
  ggplot(aes(number_of_rides, predicted, color = model)) +
  geom_point(alpha = .1) +
  #geom_smooth() +
  facet_wrap(~model)

df_daily_resid %>% 
  ggplot(aes(residual, color = model)) +
  geom_freqpoly(bins = 50)
?rm
rm(list = c("df_daily", "df_daily_pred", "df_daily_resid"))
rm(list = c("model1", "model2", "model3", "model4", "model5"))

###Daily Station Models

df_daily_station <- data_wide %>% 
  mutate(month = factor(month,  ordered = FALSE),
         wday = factor(wday, ordered = FALSE)) %>% 
  #filter(station_name_type == "from_station_name") %>% 
  #filter(!is.na(usertype)) %>% 
  mutate(destination_type = if_else(from_station_name == to_station_name, "same_station", "different_station")) %>% 
  group_by(date, wday, from_station_name, to_station_name, destination_type) %>% 
  summarize(number_of_rides = n())
df_daily_station
#class(df_daily_station$month)
#class(df_daily_station$wday)
#unique(df_daily_station$holiday)

#sum(is.na(df$usertype))
model1 <- lm(number_of_rides ~ from_station_name + from_station_name, data = df_daily_station)
model2 <- lm(number_of_rides ~ from_station_name + destination_type, data = df_daily_station)
#model3 <- lm(number_of_rides ~ wday + from_station_name + to_station_name, data = df_daily_station)
#model4 <- lm(number_of_rides ~ wday + from_station_name * to_station_name, data = df_daily_station)
#model5 <- lm(number_of_rides ~ year + month * wday + holiday + from_station_name, data = df_daily_station)

df_daily_station_pred <- df_daily_station %>% 
  gather_predictions(model1, model2) %>% 
  rename(predicted = pred)

df_daily_station_resid <- df_daily_station %>%
  gather_residuals(model1, model2) %>% 
  rename(residual = resid)

glance(model1)
glance(model2)
#glance(model3)
#glance(model4)
#glance(model5)

tidy(model1)
tidy(model2)
#tidy(model3)
#tidy(model4)
#tidy(model5)

df_daily_station_resid %>% 
  ggplot(aes(date, residual, color = model)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .1) +
  facet_wrap(~model, nrow = 1)

df_daily_station_resid %>%
  ggplot(aes(number_of_rides, residual, color = model)) +
  geom_point(alpha = .1) +
  facet_wrap(~model,
             nrow = 1)

df_daily_station_pred %>% 
  ggplot(aes(number_of_rides, predicted, color = model)) +
  geom_point(alpha = .1) +
  #geom_smooth() +
  facet_wrap(~model)

df_daily_station_resid %>% 
  ggplot(aes(residual, color = model)) +
  geom_freqpoly(bins = 50)


rm(list = c("model1", "model2"))
#get weather data using this package http://ram-n.github.io/weatherData/