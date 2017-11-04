data_path <- "./data/stations/"
data_list <- list.files(path = data_path, pattern = ".csv")
data_list <- paste0(data_path, "/", data_list)
data_list <- lapply(data_list, read_csv)

for (i in seq_along(data_list)) {
  colnames(data_list[[i]]) <- c("station_number", "station_name", "number_of_racks", "latitude", "longitude")
}

data_station_locations <- bind_rows(data_list) %>% 
  distinct()

data_station_locations$station_name[data_station_locations$station_name == "North Shore Trail & Ft Duquesne Bridge"] <- "North Shore Trail & Fort Duquesne Bridge"
data_station_locations$station_name[data_station_locations$station_name == "Fort Duquesne Blvd & 7th"] <- "Fort Duquesne Blvd & 7th St"
data_station_locations$station_name[data_station_locations$station_name == "Atwood St & Bates"] <- "Atwood St & Bates St"
data_station_locations$station_name[data_station_locations$station_name == "Alder St & S Higland Ave"] <- "Alder St & S Highland Ave"
data_station_locations$station_name[data_station_locations$station_name == "42nd St & Penn Ave"] <- "42nd & Penn Ave."
data_station_locations$station_name[data_station_locations$station_name == "Liberty Ave & S Millvale (West Penn Hospital)"] <- "Liberty Ave & S Millvale Ave (West Penn Hospital)"