# Redknot Methodology Conversion to R

# Load R Packages
library(fs)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(geosphere)
library(lubridate)

## DATA PRE-PROCESSING ##
# Read CSV Data


# TODO: add a purrr::map to be able to loop through many files/birds, 
# need to add a value to distinguish the tag id.



birds <- read_csv(path("01_inputs", "DS 2 _P7165_Con_213835_250122134729.csv"), name_repair = "unique", locale = locale(encoding = "latin1"))

# Convert to SF Object
birds_sf <- st_as_sf(birds, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Filter Out Bad Data and Sort by Date Ascending
birds_sf <- birds_sf %>% filter(CRC != "Fail")

birds_sf <- birds_sf |>
  dplyr::mutate(datetime = dmy_hms(paste(Date, Time, sep = " ")))

birds_sf <- birds_sf %>% arrange(datetime)

birds_sf <- birds_sf |>
  mutate(
    location.long_prior = lag(Longitude, 1L),
    location.lat_prior = lag(Latitude, 1L)
  )


# Calculate Distance Between Sequential Ping Locations
birds_dist <- st_distance(birds_sf) # Creates a distance matrix for every point's distance to every other point in the dataset (in metres)

dist_seq <- c() # creates an empty vector to store sequential distances

for (i in 1:nrow(birds_sf)) { # loops through every GPS ping in the dataset
  if (i == 1) { # if it's the very first point in the dataset, it will calculate the very first distance calculation, which will be zero
    dist_seq[i] <- birds_dist[i, i]
  } else { # any other element will be calculating the distance between the current point and the previous ping location
    dist_seq[i] <- birds_dist[i, i - 1]
  }
}

dist_km <- dist_seq / 1000 # converts the distances from metres to kilometres

birds_sf$dist_km <- dist_km # add the distance column to the table



# 2) calculate the time between ping locations

birds_sf$timediff_hrs <- as.numeric(difftime(birds_sf$datetime, lag(birds_sf$datetime), units = "hours"))

# 3) calculate the bearing and speed of travel

birds_sf <- birds_sf |>
  rowwise() |>
  dplyr::mutate(
    bearing = bearing(c(location.long_prior, location.lat_prior), c(Longitude, Latitude)),
    speed_kmh = round((dist_km / timediff_hrs), 1)
  )

# Convert bearings to azimuths
for(i in 2:nrow(birds_sf)){
  if(birds_sf[i,]$bearing < 0){
    birds_sf[i,]$bearing <- birds_sf[i,]$bearing + 360 # adds the difference of 360 to any negative values to get the azimuth
  }
}


# 4) Calculate and Convert Geoid Heights

EGM2008_1 <- rast(path("01_inputs", "us_nga_egm2008_1.tif")) # The Earth Gravitational Geoid Model 2008 1'

birds_vect <- vect(birds_sf) # converting the birds dataset to a SpatVector
birds_vect <- project(birds_vect, "EPSG:4979") # reprojecting birds dataset to the same CRS as the geoid model

geoid_height <- extract(EGM2008_1, birds_vect)  # tested these calculated geoid values against the geoid height calculator and the values matched
geoid_height <- geoid_height$geoid_undulation # the extract function creates a data frame so this function just gets the geoid values as a vector


birds_sf$geoid_height <- geoid_height # adding the geoid height field to the original birds_sf variable

birds_sf$ortho_height <- birds_sf$`Alt(m)` + abs(geoid_height) # uses the formula for geoid height to calculate orthometric height
# abs() function is to convert the geoid heights to positive values

# formula for geoid height: N = h - H
# N: geoid height; h: ellipsoid height; H: orthometric height

# if folder does not exist create it
if (!dir.exists(path("02_outputs"))) {
  dir.create(path("02_outputs"))
}

#saveRDS(birds_sf, path("02_outputs", "birds_raw.rds"))
st_write(birds_sf, path("02_outputs", "birds_raw.gpkg"), driver = "GPKG")
