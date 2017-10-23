library(modelr)
library(broom)
sum(is.na(df$usertype))
df <- data_long %>% 
  filter(station_name_type == "from_station_name") %>% 
  #filter(!is.na(usertype)) %>% 
  group_by(year, month, week, yday, wday, station_name) %>% 
  summarize(n = n()) %>% 
  mutate(n_lag1 = lag(n)) %>% 
  filter(!is.na(n_lag1))
df
model1 <- lm(n ~ year + month + week + wday + station_name + n_lag1, data = df)

df <- df %>% 
  add_predictions(model1) %>% 
  add_residuals(model1)
df


df %>% 
  ggplot(aes(yday, resid)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year,
             ncol = 1)

df %>% 
  ggplot(aes(n, pred)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~year)
summary(model1)

df_model <- as_tibble(tidy(model1))

terms %>% 
  filter(term != "(Intercept)") %>% 
  arrange(desc(estimate)) %>% 
  select(term) %>% 
  unique()
  

?dist
  
  
  
  mutate(term = as.factor(term)) %>% 
  ggplot(aes(term, estimate)) +
  geom_point() +
  coord_flip()
