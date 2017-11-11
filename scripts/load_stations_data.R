library(tidyverse)
library(lubridate)

data_path <- "./data/stations/"
data_list <- list.files(path = data_path, pattern = "healthyridestations")
data_list <- paste0(data_path, "/", data_list)
data_list <- lapply(data_list, read_csv)

file_names <- c("2015 3", "2015 4", "2016 1", "2016 2", "2016 3", "2016 4", "2017 1")

names(data_list) <- file_names
names(data_list)

for (i in seq_along(data_list)) {
  colnames(data_list[[i]]) <- c("station_number", "station_name", "number_of_racks", "latitude", "longitude")
}

bind_rows(data_list, .id = "file_name") %>% 
  separate(file_name, into = c("year", "quarter"),  sep = " ") %>%  
  distinct() -> data_station_locations

data_station_locations %>% 
  mutate(year = as.numeric(year),
         quarter = as.numeric(quarter)) %>% 
  select(-c(year, quarter)) -> data_station_locations

data_station_locations$station_name[data_station_locations$station_name == "North Shore Trail & Ft Duquesne Bridge"] <- "North Shore Trail & Fort Duquesne Bridge"
data_station_locations$station_name[data_station_locations$station_name == "Fort Duquesne Blvd & 7th"] <- "Fort Duquesne Blvd & 7th St"
data_station_locations$station_name[data_station_locations$station_name == "Atwood St & Bates"] <- "Atwood St & Bates St"
data_station_locations$station_name[data_station_locations$station_name == "Alder St & S Higland Ave"] <- "Alder St & S Highland Ave"
data_station_locations$station_name[data_station_locations$station_name == "42nd St & Penn Ave"] <- "42nd & Penn Ave."
data_station_locations$station_name[data_station_locations$station_name == "Liberty Ave & S Millvale (West Penn Hospital)"] <- "Liberty Ave & S Millvale Ave (West Penn Hospital)"

#write_csv(data_station_locations, "data/station_locations.csv")