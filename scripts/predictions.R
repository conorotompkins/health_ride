rm(list = ls())

source("load_data.R")

theme_set(theme_bw())

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
  facet_wrap(~month,
             scales = "free") +
  coord_equal()

prediction_stats <- predictions %>% 
  na.omit() %>% 
  group_by(month) %>% 
  summarize(cor = cor(n, lag_1)) %>% 
  left_join(month_sums)

prediction_stats %>% 
  ggplot(aes(cor, month_sum, label = month)) +
  geom_label() +
  geom_smooth()

prediction_stats %>% 
  ggplot(aes(month, cor)) +
  geom_col()

predictions_clean <- predictions %>% 
  na.omit()
cor(predictions_clean$n, predictions_clean$lag_1)
