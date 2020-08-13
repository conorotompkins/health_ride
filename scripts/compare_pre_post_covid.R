library(tidyverse)
library(lubridate)
library(janitor)

library(tsibble)
library(feasts)

library(hrbrthemes)

theme_set(theme_ipsum())

data <- list.files("data/ride_data", full.names = TRUE) %>%
  discard(str_detect(., ".xlsx$")) %>% 
  set_names() %>% 
  map_dfr(read_csv, col_types = cols(.default = "c"), .id = "file_name") %>% 
  clean_names()

data %>% 
  count(file_name) %>% 
  View()

data %>% 
  group_by(file_name) %>% 
  summarize(missing_date = mean(is.na(starttime))) %>% 
  View()

data %>% 
  filter(str_detect(file_name, "2019-q3.csv")) %>% 
  View()
  filter(is.na(starttime))

data <- data %>% 
  separate(starttime, " ", into = c("date_orig", "time"), remove = FALSE) %>% 
  mutate(date = mdy(date_orig),
         year = year(date),
         month = month(date, label = TRUE),
         yday = yday(date),
         wday = wday(date, label = TRUE)) %>% 
  select(file_name, date_orig, date, year, month, wday) %>% 
  filter(!(str_detect(file_name, "2019-q3.csv") & is.na(date)))

glimpse(data)

data %>% 
  count(date, sort = TRUE) %>% 
  filter(!is.na(date)) %>% 
  ggplot(aes(date, n)) +
  geom_point(alpha = .5, size = .5) +
  geom_vline(xintercept = ymd("2020-03-17"))

data %>%
  mutate(time = yearmonth(date)) %>% 
  count(time) %>% 
  View()

dcmp <- data %>%
  #mutate(time = yearmonth(date)) %>% 
  mutate(time = date) %>% 
  count(time, name = "number_of_rides") %>% 
  as_tsibble(index = time) %>% 
  tsibble::fill_gaps(number_of_rides = 0) %>% 
  model(STL(number_of_rides),
        STL(number_of_rides ~ season(window = Inf)),
        STL(number_of_rides ~ trend(window=7) + season(window='periodic'),
            robust = TRUE),
        classical_decomposition(number_of_rides, type = "multiplicative"))

components(dcmp)

components(dcmp) %>% 
  filter(str_detect(.model, "STL")) %>% 
  autoplot()

components(dcmp) %>% 
  filter(str_detect(.model, "STL")) %>% 
  pivot_longer(cols = number_of_rides:random) %>% 
  mutate(name = factor(name, levels = c("number_of_rides", "season_adjust",
                                        "trend", "seasonal",
                                        "season_year", "season_week",
                                        "random", "remainder"))) %>% 
  filter(!is.na(value)) %>% 
  filter(name == "trend" | name == "number_of_rides") %>% 
  ggplot(aes(time, value, color = .model)) +
  #geom_point(alpha = .1, size = .3) +
  geom_point(alpha = .5, size = .1) +
  #geom_vline(xintercept = ymd("2020-03-06")) +
  annotate(geom = "rect", 
           xmin = ymd("2020-03-06"), xmax = ymd("2020-12-31"),
           ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = .1) +
  facet_grid(name ~ .model) +
  guides(color = FALSE)
