library(tidyverse)
library(janitor)
library(sf)
library(tigris)
library(raster)
library(fasterize)
library(mapboxapi)
library(mapdeck)
library(leaflet)
library(leaflet.extras)
library(widgetframe)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

pgh <- mb_geocode("Pittsburgh, PA")

pgh

mapbox_map <- leaflet() %>%
  addMapboxTiles(style_id = "streets-v11",
                 username = "mapbox") 

mapbox_map %>%
  setView(lng = pgh[1],
          lat = pgh[2],
          zoom = 12)

# read_csv("data/healthy-ride-station-locations-2020-q2.csv",
#          col_types = cols(
#            `Station #` = col_character(),
#            `Station Name` = col_character(),
#            `# of Racks` = col_double(),
#            Latitude = col_double(),
#            Longitude = col_double()
#          )) %>% 
#   clean_names() %>% 
#   filter(station_number == "49391") %>% 
#   clipr::write_clip()

stations <- read_csv("data/stations/healthy-ride-station-locations-2020-q2.csv",
                     col_types = cols(
                       `Station #` = col_character(),
                       `Station Name` = col_character(),
                       `# of Racks` = col_double(),
                       Latitude = col_double(),
                       Longitude = col_double()
                     )) %>% 
  clean_names() %>% 
  mutate(latitude = case_when(station_number == "49391" ~ latitude * -1,
                              TRUE ~ latitude))

mapbox_map %>% 
  mapdeck() %>% 
  add_scatterplot(data = stations,
                  radius = 100) %>% 
  mapdeck_view(location = c(pgh[1], pgh[2]), zoom = 11)

test_isochrone <- stations %>% 
  slice(1) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mb_isochrone(profile = "cycling", time = c(5, 10, 15))

mapbox_map %>% 
  mapdeck() %>% 
  add_polygon(data = test_isochrone,
              fill_colour = "time",
              fill_opacity = 0.5,
              legend = TRUE)

test_isochrone %>% 
  ggplot() +
  geom_sf(aes(fill = time)) +
  scale_fill_viridis_c()


# station_isochrone <- stations %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   mb_isochrone(profile = "cycling", time = c(10)) %>% 
#   st_transform(3857)
# 
# station_isochrone %>% 
#   st_write("data/stations/station_isochrone/station_isochrone.shp")

station_isochrone <- st_read("data/stations/station_isochrone/station_isochrone.shp")

station_isochrone %>% 
  ggplot() +
  geom_sf(fill = "black", size = NA, alpha = .1) +
  theme_void()
  
#raster
polygons_proj <- station_isochrone %>%
  mutate(test_id = 1) %>% 
  filter(time == 10) %>% 
  st_transform(3857)

template <- raster(polygons_proj, resolution = 25)

raster_surface <- fasterize(polygons_proj, template, field = "test_id", fun = "sum")

raster_values <- tibble(values = values(raster_surface)) %>% 
  filter(!is.na(values)) %>% 
  distinct(values) %>% 
  pull(values)

plot(raster_surface)

custom_pal <- colorNumeric("viridis", 
                           #0:max_bike_stations, 
                           raster_values,
                           na.color = "transparent")

popup_labels <- sprintf("%s 
                        <br>Number of bike racks: %s",
                        stations$station_name, stations$number_of_racks) %>% 
  map(htmltools::HTML)

health_ride_icon <- makeIcon(
  iconUrl = "https://healthyridepgh.com/wp-content/uploads/sites/3/2019/05/NEXTBIKE-LOGO-01.png",
  #iconUrl = "https://healthyridepgh.com/wp-content/uploads/sites/3/2016/09/Healthy-Ride-Logo.Stacked-01.png",
  iconWidth = 50, iconHeight = 50,
  iconAnchorX = 0, iconAnchorY = 0
)

station_heatmap <- mapbox_map %>%
  addRasterImage(raster_surface, colors = custom_pal, opacity = .75,
                 group = "Raster") %>% 
  addLegend(pal = custom_pal, 
            values = raster_values,
            title = "Number of stations<br>within 10-minute bike ride") %>% 
  addMarkers(data = stations, lng = ~longitude, lat = ~latitude,
             popup = popup_labels,
             icon = health_ride_icon,
             clusterOptions = markerClusterOptions(),
             group = "Stations") %>% 
  addLayersControl(overlayGroups = c("Raster", "Stations"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addFullscreenControl()

frameWidget(station_heatmap, options=frameOptions(allowfullscreen = TRUE))

