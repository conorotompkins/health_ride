library(tidyverse)
library(lubridate)

theme_set(theme_bw())

rm(list = ls())

data_list <- list.files(pattern = ".csv")
data_list <- lapply(data_list, read_csv)
data <- bind_rows(data_list)

colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub(" ", "_", colnames(data))

data_wide <- data_long %>% 
  spread(location_name_type, location_name)