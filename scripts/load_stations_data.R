data_path <- "./data/stations/"
data_list <- list.files(path = data_path, pattern = ".csv")
data_list <- paste0(data_path, "/", data_list)
data_list <- lapply(data_list, read_csv)

for (i in seq_along(data_list)) {
  colnames(data_list[[i]]) <- c("station_number", "station_name", "number_of_racks", "latitude", "longitude")
}

data_station_locations <- bind_rows(data_list) %>% 
  distinct()