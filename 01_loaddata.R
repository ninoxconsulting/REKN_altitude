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

### List all bird files in the inputs folder
bird_files <- list.files(path("01_inputs/bird_data"), recursive = TRUE, pattern = ".csv") # Read in a list of all bird CSV data

#bird_files <- bird_files[150:160]

### Loop through each existing bird CSV file and perform the analyses
birds_processed <- purrr::map(bird_files, function(b) {
  # testing
  # b <- bird_files[345]
  
  bname <- gsub(".*/(.*)", "\\1", b)
  
  cli::cli_alert_info("processing {bname}")
  
  ## 0) Data Pre-Processing
  # Read in CSV Data
  birds <- read_csv(path("01_inputs/bird_data", b), name_repair = "unique", locale = locale(encoding = "latin1"),
                    show_col_types = FALSE)
  
  # Filter Out Bad Data and Sort by Date Ascending
  birds <- birds %>% filter(CRC != "Fail")
  
  ## Use an if statement to skip records that no longer have at least 3 records after filtering bad data
  if(nrow(birds) >= 3){
  
  # Filter Out Bad Latitude / Longitude Values
  birds <- birds %>% filter(Latitude >= -90 & Latitude <= 90)
  birds <- birds %>% filter(Longitude >= -180 & Latitude <= 180)
    
  # Convert to SF Object
  birds_sf <- st_as_sf(birds, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  
  birds_sf <- birds_sf |>
    dplyr::mutate(datetime = dmy_hms(paste(Date, Time, sep = " ")))
  
  birds_sf <- birds_sf %>% arrange(datetime)
  
  # add a check to remove points that are obviously wrong but not flagged as fail.
  birds_sf$year <- year(birds_sf$datetime) # add a year column to the dataset
  birds_sf <- birds_sf |>
    filter(year < 2025) |>
    select(-year) |> 
    mutate(Fix = as.character(Fix)) 
  
  
  birds_sf <- birds_sf |>
    mutate(
      location.long_prior = lag(Longitude, 1L),
      location.lat_prior = lag(Latitude, 1L),
      filename = bname
    )
  
  
  ## 1) Calculate Distance Between Sequential Ping Locations
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


  ## 2) calculate the time between ping locations

  birds_sf$timediff_hrs <- as.numeric(difftime(birds_sf$datetime, lag(birds_sf$datetime), units = "hours"))

  ## 3) calculate the bearing and speed of travel

  birds_sf <- birds_sf |>
    rowwise() |>
    dplyr::mutate(
      bearing = bearing(c(location.long_prior, location.lat_prior), c(Longitude, Latitude)),
      speed_kmh = round((dist_km / timediff_hrs), 1)
    )

  # Convert bearings to azimuths
  for (i in 2:nrow(birds_sf)) {
    if (birds_sf[i, ]$bearing < 0) {
      birds_sf[i, ]$bearing <- birds_sf[i, ]$bearing + 360 # adds the difference of 360 to any negative values to get the azimuth
    }
  }
  
  } else {
    cli::cli_alert_danger("skipping {bname} because it has less than 3 records after filtering")
    birds_sf <- data.frame() # still sets birds_sf as an empty data frame to be added to birds_processed list
  }
  
  return(birds_sf)
})

# this includes some empty tibbles - get list of empty 
aa <- purrr::map(birds_processed, function(x) {
  nrow(x) ==0
})

birds_processed1 <- birds_processed[unlist(aa) == FALSE] # remove any empty dataframes from the list

# i kept this as a seperate line so we can test errors in the birds_processed file! 
birds_sf <- birds_processed1 |> bind_rows()
# creates a single variable for the bird data prior to geoid and onshore calculations

## 4) Calculate and Convert Geoid Heights

EGM2008_1 <- rast(path("01_inputs", "us_nga_egm2008_1.tif")) # The Earth Gravitational Geoid Model 2008 1'

birds_vect <- vect(birds_sf) # converting the birds dataset to a SpatVector
birds_vect <- project(birds_vect, "EPSG:4979") # reprojecting birds dataset to the same CRS as the geoid model

geoid_height <- terra::extract(EGM2008_1, birds_vect) # tested these calculated geoid values against the geoid height calculator and the values matched
geoid_height <- geoid_height$geoid_undulation # the extract function creates a data frame so this function just gets the geoid values as a vector


birds_sf$geoid_height <- geoid_height # adding the geoid height field to the original birds_sf variable

birds_sf$ortho_height <- birds_sf$`Alt(m)` + abs(geoid_height) # uses the formula for geoid height to calculate orthometric height
# abs() function is to convert the geoid heights to positive values

# formula for geoid height: N = h - H
# N: geoid height; h: ellipsoid height; H: orthometric height

## 4.5) Determine Onshore, Offshore, Nearshore for bird pings
land <- st_read(path("01_inputs", "ne_10m_admin_0_countries.shp")) # read in the countries dataset
land <- st_make_valid(land) # there may be some duplicate vertex issues that arise from the countries dataset so we resolve this

nearshore <- lengths(st_intersects(birds_sf, st_buffer(land, 300))) > 0 # nearshore is 300 metres from the shoreline

# assign shore comparison columns for onshore/offshore/nearshore
birds_sf$onshore <- lengths(st_intersects(birds_sf, land)) > 0 # onshore is anything over a landmass
birds_sf$nearshore <- nearshore & !birds_sf$onshore # refining nearshore to exclude any onshore pings
birds_sf$offshore <- !birds_sf$onshore & !nearshore # any pings not meeting onshore or nearshore criteria are offshore

shore_status <- c()

pb <- txtProgressBar(min = 0, max = nrow(birds_sf), initial = 0, style = 3) # creating a progress bar for the onshore loop

for (i in 1:nrow(birds_sf)) { # loop through bird data and assign onshore/offshore/nearshore based on shore comparison columns
  setTxtProgressBar(pb, i)
  if (birds_sf[i, ]$onshore == TRUE && birds_sf[i, ]$offshore == FALSE && birds_sf[i, ]$nearshore == FALSE) {
    shore_status[i] <- "Onshore"
  } else if (birds_sf[i, ]$onshore == FALSE && birds_sf[i, ]$offshore == TRUE && birds_sf[i, ]$nearshore == FALSE) {
    shore_status[i] <- "Offshore"
  } else if (birds_sf[i, ]$onshore == FALSE && birds_sf[i, ]$offshore == FALSE && birds_sf[i, ]$nearshore == TRUE) {
    shore_status[i] <- "Nearshore"
  } else {
    shore_status[i] <- "ERROR"
  }
  close(pb)
}

birds_sf$shore_status <- shore_status # add the shore status column to the bird data

birds_sf <- subset(birds_sf, select = -c(onshore, offshore, nearshore)) # drop the shore comparison columns

## Returns the bird_sf as the output for each iteration of the loop

## Create output folder for storing data if folder does not exist
if (!dir.exists(path("02_outputs"))) {
  dir.create(path("02_outputs"))
}

if (!dir.exists(path("02_outputs/birds_raw_proc"))) {
  dir.create(path("02_outputs/birds_raw_proc"))
}

st_write(birds_sf, path("02_outputs/birds_raw_proc", "birds_raw.gpkg"), append = FALSE,driver = "GPKG")
