library(tidyverse)
library(modelr)
library(broom)

options(scipen = 999)

df <- data_long %>% 
  filter(station_name_type == "from_station_name") %>% 
  #filter(!is.na(usertype)) %>% 
  group_by(mday, month, wday) %>% 
  summarize(n = n())
df

#sum(is.na(df$usertype))
model1 <- lm(n ~ month + wday, data = df)

df <- df %>% 
  add_predictions(model1) %>% 
  add_residuals(model1)
summary(model1)


df %>% 
  ggplot(aes(mday, resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  #geom_smooth() +
  facet_wrap(~month)

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
