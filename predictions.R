rm(list = ls())

theme_set(theme_bw())

source("load_data.R")





predictions <- data_long %>% 
  group_by(month, date) %>% 
  count()

predictions %>% 
  ggplot(aes(date, n)) + 
  geom_smooth()

predictions <- predictions %>% 
  group_by(month) %>% 
  mutate(lag_1 = lag(n))

month_sums <- data_long %>% 
  group_by(month) %>% 
  count() %>% 
  mutate(month_sum = n) %>% 
  select(-n)
  
predictions %>% 
  ggplot(aes(n, lag_1)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~month)

predictions %>% 
  na.omit() %>% 
  group_by(month) %>% 
  summarize(cor = cor(n, lag_1)) %>% 
  left_join(month_sums) %>% 
  ggplot(aes(cor, month_sum, label = month)) +
  geom_label() +
  geom_smooth()

predictions_clean <- predictions %>% 
  na.omit()
cor(predictions_clean$n, predictions_clean$lag_1)
