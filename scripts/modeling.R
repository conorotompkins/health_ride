library(tidyverse)
library(modelr)
library(broom)
library(lubridate)

theme_set(theme_bw())

options(scipen = 99)

#source("scripts/load_data_long.R")
#source("scripts/load_data_wide.R")
#rm(list = c("data", "data_long"))

#df_holidays <- read_csv("data/holidays.csv") %>% 
#  mutate(date = mdy(date))
#df_holidays

data_long <- read_csv("https://raw.githubusercontent.com/conorotompkins/healthy_ride/master/data/data.csv")

colnames(data_long) <- tolower(colnames(data_long))
colnames(data_long) <- gsub(" ", "_", colnames(data_long))

data_long <- data_long %>% 
  rename(start_date_time = starttime,
         stop_date_time = stoptime) %>% 
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


data_wide <- data_long %>%
  spread(station_name_type, station_name)
data_wide


df_holidays <- read_csv("https://raw.githubusercontent.com/conorotompkins/healthy_ride/master/data/holidays.csv") %>% 
  mutate(date = mdy(date))

df_weather <- read_csv("https://raw.githubusercontent.com/conorotompkins/healthy_ride/master/data/df_weather.csv")


###Daily models
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
#model5 <- lm(number_of_rides ~ year * month * wday * holiday, data = df_daily)

glance(model1)
glance(model2)
glance(model3)
glance(model4)
#glance(model5)

tidy(model1)
tidy(model2)
tidy(model3)
tidy(model4)
#tidy(model5)

df_daily %>%
  add_predictions(model1)

df_daily_pred <- df_daily %>% 
  gather_predictions(model1, model2, model3, model4) %>% 
  rename(predicted = pred)

df_daily_resid <- df_daily %>%
  gather_residuals(model1, model2, model3, model4) %>% 
  rename(residual = resid)

df_daily <- df_daily %>% 
  left_join(df_daily_pred) %>% 
  left_join(df_daily_resid)

#plotting daily models
df_daily %>% 
  ggplot(aes(date, predicted, color = model)) +
  geom_point(aes(y = number_of_rides), color = "black", alpha = .5) +
  geom_point(alpha = 1) +
  facet_wrap(~model, nrow = 1)

df_daily %>% 
  ggplot(aes(date, residual, color = model)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .5) +
  facet_wrap(~model, nrow = 1)

df_daily %>% 
  ggplot(aes(number_of_rides, predicted, color = model)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  facet_wrap(~model)

df_daily %>%
  ggplot(aes(number_of_rides, residual, color = model)) +
  geom_point(alpha = .5) +
  facet_wrap(~model,
             nrow = 1)

df_daily %>% 
  ggplot(aes(residual, color = model)) +
  geom_freqpoly(bins = 50)

rm(list = c("df_daily", "df_daily_pred", "df_daily_resid"))
rm(list = c("model1", "model2", "model3", "model4"))

###weather model
#source("scripts/load_weather_data.R")

df_daily <- data_wide %>% 
  mutate(month = factor(month,  ordered = FALSE),
         wday = factor(wday, ordered = FALSE)) %>% 
  #filter(station_name_type == "from_station_name") %>% 
  #filter(!is.na(usertype)) %>% 
  group_by(date, year, yday, month, wday) %>% 
  summarize(number_of_rides = n()) %>% 
  left_join(df_weather) %>%
  left_join(df_holidays) %>% 
  replace_na(replace = list(holiday = "none")) %>% 
  ungroup()

model1 <- lm(number_of_rides ~ year * month * wday + holiday, data = df_daily)
model2 <- lm(number_of_rides ~ year * month * wday + holiday + temp_mean + temp_hi + temp_lo + precip, data = df_daily)

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

df_daily_weather_pred <- df_daily %>% 
  gather_predictions(model1, model2) %>% 
  rename(predicted = pred)

df_daily_weather_resid <- df_daily %>%
  gather_residuals(model1, model2) %>% 
  rename(residual = resid)

df_daily_weather <- df_daily %>%
  left_join(df_daily_weather_pred) %>% 
  left_join(df_daily_weather_resid)

#plotting daily models
df_daily_weather %>% 
  ggplot(aes(date, predicted, color = model)) +
  geom_point(aes(y = number_of_rides), color = "black", alpha = .5) +
  geom_point(alpha = 1) +
  facet_wrap(~model, nrow = 1)

df_daily_weather %>% 
  ggplot(aes(date, residual, color = model)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .5) +
  facet_wrap(~model, nrow = 1)

df_daily_weather %>% 
  ggplot(aes(number_of_rides, predicted, color = model)) +
  geom_point(alpha = .5) +
  #geom_smooth() +
  facet_wrap(~model)

df_daily_weather %>%
  ggplot(aes(number_of_rides, residual, color = model)) +
  geom_point(alpha = .5) +
  facet_wrap(~model,
             nrow = 1)

df_daily_weather %>% 
  ggplot(aes(residual, color = model)) +
  geom_freqpoly(bins = 50)

rm(list = c("df_daily", "df_daily_weather_pred", "df_daily_weather_resid", "df_daily_weather"))
rm(list = c("model1", "model2"))
rm(list = c("data_weather_dfs", "df_weather"))

###Daily Station Models
df_daily_station <- data_wide %>% 
  mutate(month = factor(month,  ordered = FALSE),
         wday = factor(wday, ordered = FALSE)) %>% 
  #filter(station_name_type == "from_station_name") %>% 
  #filter(!is.na(usertype)) %>% 
  mutate(destination_type = if_else(from_station_name == to_station_name, "same_station", "different_station")) %>% 
  group_by(date, year, month, wday, from_station_name, to_station_name, destination_type) %>% 
  summarize(number_of_rides = n()) %>% 
  left_join(df_holidays) %>% 
  replace_na(replace = list(holiday = "none"))
df_daily_station
#class(df_daily_station$month)
#class(df_daily_station$wday)
#unique(df_daily_station$holiday)

#sum(is.na(df$usertype))
model1 <- lm(number_of_rides ~ year * month * wday + holiday, data = df_daily_station)
model2 <- lm(number_of_rides ~ from_station_name + to_station_name, data = df_daily_station)
model3 <- lm(number_of_rides ~ from_station_name + destination_type, data = df_daily_station)
#model3 <- lm(number_of_rides ~ wday + from_station_name + to_station_name, data = df_daily_station)
#model4 <- lm(number_of_rides ~ wday + from_station_name * to_station_name, data = df_daily_station)
#model5 <- lm(number_of_rides ~ year + month * wday + holiday + from_station_name, data = df_daily_station)

df_daily_station_pred <- df_daily_station %>% 
  gather_predictions(model1, model2) %>% 
  rename(predicted = pred)

df_daily_station_resid <- df_daily_station %>%
  gather_residuals(model1, model2) %>% 
  rename(residual = resid)

df_daily_station <- df_daily_station %>%
  left_join(df_daily_station_pred) %>% 
  left_join(df_daily_station_resid)

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

df_daily_station %>% 
  ggplot(aes(date, predicted, color = model)) +
  geom_point(aes(y = number_of_rides), color = "black", alpha = .5) +
  geom_point(alpha = .1) +
  facet_wrap(~model, nrow = 1)

df_daily_station %>% 
  ggplot(aes(date, residual, color = model)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .5) +
  facet_wrap(~model, nrow = 1)

df_daily_station  %>% 
  ggplot(aes(number_of_rides, predicted, color = model)) +
  geom_point(alpha = .5) +
  #geom_smooth() +
  facet_wrap(~model)

df_daily_station %>% 
  ggplot(aes(number_of_rides, predicted, color = model)) +
  geom_point(alpha = .5) +
  facet_wrap(~model,
             nrow = 1)
  
df_daily_station %>%
  ggplot(aes(number_of_rides, residual, color = model)) +
  geom_point(alpha = .5) +
  facet_wrap(~model,
             nrow = 1)

df_daily_station %>% 
  ggplot(aes(residual, color = model)) +
  geom_freqpoly(bins = 50)


rm(list = c("model1", "model2"))
rm(list = c("df_daily_station", "df_daily_station_pred", "df_daily_station_resid"))
