#save the data
library(sf)
#install.packages("climaemet")
library(climaemet)
library(ggplot2)


bb <- st_read(path("02_outputs", "birds_final_proc","birdwind_all.gpkg"), quiet = TRUE) 

#head(bb)


bb <- bb |>
  select(
    id, filename, datetime, shore_status,Fix, Alt.m., bearing, speed_kmh, geoid_height, ortho_height,
    ws, windangle, geom
  ) |>
  dplyr::mutate(
    bid = stringr::str_split_i(filename, "_", 4),
    bproj = stringr::str_split_i(filename, "-", 2)
  ) |>
  select(-c(filename))

# summary of individuals - duration 

bbs <- bb |> 
  group_by(bid) |>
  st_drop_geometry() |>
  summarise(
    n = n(),
    start = min(datetime),
    end = max(datetime),
    duration = as.numeric(difftime(end, start, units = "days")),
    .groups = "drop"
    
  )


# summary of individuals by type of location data 
bbt <- bb |> 
  group_by(bid, shore_status) |>
  st_drop_geometry() |>
  summarise(
    n = n()
  )





# fix = quality of data 
# Alt.m. = altitude in meters - based on GPS 
# bearing = direction of travel calculated between points using geosphere (-180 to 180)
# speed_kmh = speed in km/h calculated between points
# geoid_height = height above the geoid (sea level) in meters
# ortho_height = height above the ellipsoid in meters
# ws = wind speed in m/s using the modelled CCMP data
# windangle = wind direction in degrees using the modelled CCMP data (-180 to 180)


sort(unique(bb$speed_kmh, na.rm = TRUE)) # outliers
sort(unique(bb$bearing, na.rm = TRUE))
sort(unique(bb$geoid_height, na.rm = TRUE))
sort(unique(bb$ortho_height, na.rm = TRUE))
sort(unique(bb$ws, na.rm = TRUE))


# plot the speed and geoid height in geom_point
ggplot(bb, aes(x = speed_kmh, y = ortho_height, colour = bid)) +
  geom_point() +
  labs(title = "Speed vs Altitude",
       x = "Speed (km/h)",
       y = "Altitude (m)") +
  theme_minimal() + xlim(c(0, 50))

# this needs further attention as the data does not seem correct? 


# plot the speed and geoid height in geom_point by type 
ggplot(bb, aes(x = speed_kmh, y = ortho_height, colour = shore_status)) +
  geom_point() +
  labs(title = "Speed vs Altitude",
       x = "Speed (km/h)",
       y = "Altitude (m)") +
  theme_minimal() + xlim(c(0, 50))





# 
# 
# # summary of all points speeds 
# 
# ggplot(bb, aes(x = speed_kmh)) +
#   geom_histogram(bins = 30) +
#   labs(title = "Histogram of Speeds",
#        x = "Speed (km/h)",
#        y = "Frequency") +
#   theme_minimal() + xlim(c(0, 100)) 




# summary of all points
# TODO: note this does not work currently as need to convert from bearing to azmith? Still to do. 
# need to convert wind angle (0 - 360)

rose <- ggwindrose(
  speed = bb$ws,
  direction = bb$windangle,
  speed_cuts = seq(0, 16, 4),
  legend_title = "Wind speed (m/s)",
  calm_wind = 0,
  n_col = 1,
  plot_title = "Test Red Knot"
)
rose + labs(
  subtitle = "2000-2020",
  caption = "Source: AEMET"
  
  
  
  
)


## example of plot 



####################################################################################
library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
#library (gganimate)
library(ggspatial)
library(leaflet)



## read in compiled data with movements and limit to rufa 
xx <- unique(bb$bid)

bb <- cbind(bb , st_coordinates(bb ))

bb <- bb |> 
  filter(bid == xx[1])

pal <- colorFactor(
  palette = "viridis",
  #palette = "magma",
  domain = unique(bb$ortho_height))


# 
# pal <- colorFactor(
#   palette = "viridis",
#   #palette = "magma",
#   domain = unique(bb$bid))


birdmapall <- leaflet(bb) %>%
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = bb$X, lat = bb$Y, 
                   weight = 4, 
                   #color = ~pal(bb$bid), 
                   color = ~pal(bb$ortho_height),
                   fill = TRUE,
                   label = ~bb$bid,
                   radius = ~2 ,
                   popup = paste("Bird id:", bb$bid, "<br>",
                                 "Date:", bb$datetime)) |> 
  addPolylines(data =bb, lng = bb$X, lat = bb$Y,
               color = "grey",   opacity = 0.1, stroke = TRUE)# %>%

birdmapall






