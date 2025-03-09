#save the data
library(sf)
#install.packages("climaemet")
library(climaemet)
library(ggplot2)



bb <- st_read(path("02_outputs", "birdwind_all.gpkg"), quiet = TRUE) 

head(bb)

bb <- bb |> 
  select(id, datetime, Fix, Alt.m., bearing, speed_kmh, geoid_height, ortho_height, 
         ws, windangle, geom) 


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
ggplot(bb, aes(x = speed_kmh, y = ortho_height)) +
  geom_point() +
  labs(title = "Speed vs Altitude",
       x = "Speed (km/h)",
       y = "Altitude (m)") +
  theme_minimal() + xlim(c(0, 50))









# summary of all points speeds 

ggplot(bb, aes(x = speed_kmh)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Speeds",
       x = "Speed (km/h)",
       y = "Frequency") +
  theme_minimal() + xlim(c(0, 100)) 




# summary of all points
# note this does not work currently as need to convert from bearing to azmith? Still to do. 
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