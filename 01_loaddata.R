
# Redknot Methodology Conversion to R

# Load R Packages
library(fs)
library(dplyr)
library(readr)
library(sf)
library(terra)

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

timediff_hrs <- c()

for(i in 1:nrow(birds_sf)) {
  if(i == 1){
    timediff_hrs[i] <- 0
  }
  else{
    d <- as.numeric(difftime(birds_sf[i,]$Date, birds_sf[i-1,]$Date, units = "hours")) # time difference for date fields
    t <- as.numeric(difftime(birds_sf[i,]$Time, birds_sf[i-1,]$Time, units = "hours")) # time difference for time fields (need to account for if dates are not equal)
    if(t < 0){
      timediff_hrs[i] <- d + (24 + t) # adjusts time difference where dates differ and i time is a lower value than i-1 time
    }
    else{
      timediff_hrs[i] <- d + t # add the total time difference for date and time fields
    }
  }
}

birds_sf$timediff_hrs <- timediff_hrs

# Calculate Speed of Travel from Distance and Time
speed_kmh <- c()

for(i in 1:nrow(birds_sf)){
  if(i == 1){
    speed_kmh[i] <- 0
  }
  else{
    speed_kmh[i] <- birds_sf[i,]$dist_km / birds_sf[i,]$timediff_hrs
  }
}

birds_sf$speed_kmh <- speed_kmh
  

# 3) calculate the flight distance and speed. 

#https://github.com/ninoxconsulting/REKN_gps/blob/main/02_combine_all_datasets.R

# 
# ############################################################################
# ## Calculate distance between points and bearing
# 
# bdd_det <- out  |> 
#   #filter(tag.id == 230318) |> 
#   group_by(tag.id) |> 
#   mutate(location.long_prior = lag(location.long, 1L),
#          location.lat_prior = lag(location.lat, 1L))
# 
# bdd_det <- bdd_det |> 
#   rowwise() %>%
#   dplyr::mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
#                 bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
#                 speed_mhr = round((gcd_m/diff)/1000,1))%>% 
#   ungroup()
# 
# 
# #length(unique(bdd_det$tag.id))


## see note see code in other repo for this usign GCD 




# 4) convert the ellipsoid height 

#“Geoid Height Calculator” from the NSF GAGE Facility. 
# https://observablehq.com/@earthscope/geoid-height-calculator/2





# extract first and last date for the bird to get wind data first date and last date?




