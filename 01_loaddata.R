
# Redknot Methodology Conversion to R

# Load R Packages
library(fs)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(geosphere)

## DATA PRE-PROCESSING ##
# Read CSV Data
birds <- read_csv(path("01_inputs", "DS 2 _P7165_Con_213835_250122134729.csv"),name_repair = "unique",locale=locale(encoding="latin1"))

# Convert to SF Object
birds_sf <- st_as_sf(birds, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Filter Out Bad Data and Sort by Date Ascending
birds_sf <- birds_sf %>% filter(CRC != 'Fail')

birds_sf <- birds_sf |> 
    dplyr::mutate(datetime = dmy_hms(paste(Date, Time, sep = " ")))

birds_sf <- birds_sf %>% arrange(datetime)

birds_sf <- birds_sf |> 
   mutate(location.long_prior = lag(Longitude, 1L),
          location.lat_prior = lag(Latitude, 1L))


# Calculate Distance Between Sequential Ping Locations
birds_dist <- st_distance(birds_sf) # Creates a distance matrix for every point's distance to every other point in the dataset (in metres)

dist_seq <- c() # creates an empty vector to store sequential distances

for(i in 1:nrow(birds_sf)){ # loops through every GPS ping in the dataset
  if(i == 1){ # if it's the very first point in the dataset, it will calculate the very first distance calculation, which will be zero
    dist_seq[i] <- birds_dist[i,i]
  }
  else{ # any other element will be calculating the distance between the current point and the previous ping location
    dist_seq[i] <- birds_dist[i,i-1]
  }
}

dist_km <- dist_seq / 1000 # converts the distances from metres to kilometres

birds_sf$dist_km <- dist_km # add the distance column to the table



# 2) calculate the time between ping locations

birds_sf$timediff_hrs <- as.numeric(difftime(birds_sf$datetime, lag(birds_sf$datetime), units = "hours"))

# 3) calculate the bearing and speed of travel

birds_sf<- birds_sf|> 
   rowwise() |> 
   dplyr::mutate(bearing = bearing(c(location.long_prior,location.lat_prior), c(Longitude, Latitude)),
                 speed_kmh = round((dist_km /timediff_hrs),1))





# 4) convert the ellipsoid height 

#“Geoid Height Calculator” from the NSF GAGE Facility. 
# https://observablehq.com/@earthscope/geoid-height-calculator/2





# extract first and last date for the bird to get wind data first date and last date?




