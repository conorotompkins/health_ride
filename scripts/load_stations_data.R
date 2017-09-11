locations <- readr::read_csv("data/healthyridestations2017.csv") %>% 
  rename(station = `Station #`,
         station_name = `Station Name`,
         number_of_racks = `# of Racks`,
         latitude = Latitude,
         longitude = Longitude)

