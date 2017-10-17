#troubleshooting duplicate stations

troubleshoot_map <- get_map(location = "David Lawrence Convention Center Pittsburgh, PA", zoom = 17)
troubleshoot_map <- ggmap(troubleshoot_map)

# c("10th St & Penn Ave", "10th St & Penn Ave (David L. Lawrence Convention Center)") are duplicate 
troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  #geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

#21st St & Penn Ave are not duplicate
troubleshoot_map <- get_map(location = "The Strip Pittsburgh, PA", zoom = 15)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()


#lawrenceville 42nd and butler
troubleshoot_map <- get_map(location = "42nd and Butler Street Pittsburgh, PA", zoom = 17)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  #geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

#lawrenceville c("42nd St & Penn Ave", "42nd St & Penn Ave (Children's Hospital)") are duplicates
troubleshoot_map <- get_map(location = "Children's Hospital of Pittsburgh, Pittsburgh, PA 15224", zoom = 17)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  #geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

#Alder St & S Higland Ave TYPO, very close
troubleshoot_map <- get_map(location = "Shadyside, Pittsburgh, PA 15206", zoom = 17)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  #geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

#Bigelow Blvd & Fifth Ave duplicate
troubleshoot_map <- get_map(location = "Bigelow Blvd and Fifth Ave, Pittsburgh, PA", zoom = 17)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  #geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

#Centre Ave & Kirkpatrick St very close
troubleshoot_map <- get_map(location = "The Hill, Pittsburgh, PA", zoom = 14)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

#c("Fifth Ave & S Bouquet", "Fifth Ave & S Bouquet St") duplicate

#c("First Ave & B St", "First Ave & B St (T Station)") moved
troubleshoot_map <- get_map(location = "PNC Firstside, Pittsburgh, PA", zoom = 17)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  #geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

# c("First Ave & Smithfield St", "First Ave & Smithfield St (Art Institute)") duplicate
troubleshoot_map <- get_map(location = "PNC Firstside Center, Pittsburgh, PA", zoom = 16)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  #geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

#Forbes Ave & Grant St moved
troubleshoot_map <- get_map(location = "Downtown, Pittsburgh, PA", zoom = 16)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  #geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()

# c("Forbes Ave & Market Sq", "Forbes Ave & Market Square") duplicate

#Fort Duquesne Blvd & 7th very close
troubleshoot_map <- get_map(location = "Fort Duquesne Blvd & 7th, Pittsburgh, PA", zoom = 16)
troubleshoot_map <- ggmap(troubleshoot_map)

troubleshoot_map +
  geom_point(data = df_station_names, aes(longitude, latitude)) +
  geom_label(data = df_station_names, aes(longitude, latitude, label = station_name)) +
  scale_size_continuous(range = c(.1, 5)) +
  theme_minimal()
