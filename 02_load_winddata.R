library(RCurl)
library(terra)
library(fs)
library(sf)
library(terra)
library(tidyverse)
library(lubridate)
library(purrr)
library(dplyr)

# 5) Get wind data

# Wind direction can be directly accessed from the website hosted by CCMP
#  Mears, C.; Lee, T.; Ricciardulli, L.; Wang, X.; Wentz, F., 2022: RSS Cross-
#  Calibrated Multi-Platform (CCMP) 6-hourly ocean vector wind analysis on 0.25
#  deg grid, Version 3.0, Remote Sensing Systems, Santa Rosa, CA. Available at
#  www.remss.com https://doi.org/10.56236/RSS-uv6h30


# https://www.remss.com/measurements/ccmp/


# set up to read prepared file in
list.files(path("02_outputs"))
bird <- st_read(path("02_outputs", "birds_raw.gpkg"))

# note TIME has been dropped. but not a huge problem as we have the date_time stamp.

# Add specific time filters to help with the selection of appropriate wind download file
# ie which date to download and then which timestamp to download
#  (based on the closest time to one of 6 hours blocks ; each day has a 0, 6, 12, and 18)

# note for days which the recording is after

bird$date_file <- as.character(unique(ymd_hms(bird$datetime)))
bird$date_file <- gsub("-", "", bird$date_file)
bird$date_file <- sub(" .*", "", bird$date_file)


# Assign a time catergory based on hour

bird <- bird |>
  mutate(hour = hour(datetime)) |>
  mutate(timeclass = case_when(
    hour <= 3 ~ "00",
    hour > 3 & hour <= 9 ~ "06",
    hour > 9 & hour <= 15 ~ "12",
    hour > 15 & hour <= 21 ~ "18",
    hour < 21 ~ "24"
  ))


bird <- bird |>
  mutate(winddate = case_when(
    timeclass %in% c("00", "06", "12","18") ~ date_file,
    timeclass == "24" ~ as.character(as.numeric(date_file) + 1))) %>% 
  mutate(windclass = case_when(
    timeclass == "00" ~ "1",
    timeclass == "06" ~ "2",
    timeclass == "12" ~ "3",
    timeclass == "18" ~ "4",
    timeclass == "24" ~ "1")) |> 
  mutate(id = seq(1, nrow(bird), 1))
    
 

# get a list of the unique dates in the dataset.
unique_dates <- unique(bird$winddate)

#  download relevant wind data files 

# check if folder exists or create it
if (!dir.exists(path("00_downloads"))) {
  dir.create(path("00_downloads"))
}

# step through each unique date and download the corresponding file based on naming 

## CAM NOTES - seems to be a 60 second timeout on the download which has caused a few sporadic failures in the download
## from my end. If a file fails to download due to this timeout, it will still show up in the downloads folder but will
## not be able to be read as a SpatRaster (I guess it's not a fully downloaded .nc file?). Seems to be a possibility that
## we can increase the timeout in our global R options using options(timeout = X). Perhaps we can try to bump up this option
## and see if this mitigates the error. The manual solution I used was removing the file where the download failed and then
## re-running the download section of the code to continue. However it failed multiple times so each failure I had to remove
## the last file and re-run until I got all 20 files downloaded and functioning properly.

# set a larger timeout to prevent file download failure in the code
options(timeout = 1000)

# download relevant wind files

dls <- purrr::map(unique_dates, function(i) {
  #i <- unique_dates[1]

  fyear <- substr(i, 1, 4)
  fmonth <- substr(i, 5, 6)

  fname <- paste0("CCMP_Wind_Analysis_", i, "_V03.1_L4.nc")

  # check if file already exists
  if (file.exists(fs::path("00_downloads", fname))) {
    cli::cli_alert("File already exists")
  } else {
    url <- paste0("https://data.remss.com/ccmp/v03.1/Y", fyear, "/M", fmonth, "/", fname)
    download.file(url, destfile = fs::path("00_downloads", basename(url)), mode = "wb")
  }
})


# match the wind data for each of the bird datas 

# each file contains a stacked rast with 16 layers.
# u – Zonal Wind (west-east) (m/s)
# v – Meridional Wind (north-south) (m/s)
# Positive v wind is blowing from the south, Positive u wind is blowing from the west

# stack to covariates

# [1] "uwnd_1" "uwnd_2" "uwnd_3" "uwnd_4" "vwnd_1" "vwnd_2" "vwnd_3" "vwnd_4" "ws_1"   "ws_2"   "ws_3"
# [12] "ws_4"   "nobs_1" "nobs_2" "nobs_3" "nobs_4"

# TODO: what is the ws bands represent?
# at each time count, 0 , 6, 12, 18 hours.




bird

# cycle through each file and intersect the data with appropriate line of bird data of data then cycle through the time period. 

fls <- list.files("00_downloads")

#fls <- fls[1:2]

birdwind <- purrr::map(fls, function(i) {
#  i <- fls[2]
  # for each file 
  cli::cli_alert("processing file {i}")
  
  fname <- fs::path("00_downloads", i)
  fdate <- sub(".*_(\\d{8})_.*", "\\1", i)
  
  # read in the file and rotate to conver too -180 to 180
  r <- terra::rast(fname)
  #rr <- rotate(r , "test_rotate.tif", left = TRUE, overwrite=TRUE)
  rr <- rotate(r ,  left = TRUE)


  # get the bird data for the date and time
  bird_sub <- bird %>% filter(winddate == fdate)
                            
  # get the values for the bird data
  bird_values <- terra::extract(rr, bird_sub)
  
  # add the values to the bird data
  bird_sub <- cbind(bird_sub, bird_values)
  
  bird_sub <- bird_sub |> 
    select(-c(nobs_1, nobs_2, nobs_3, nobs_4))
  
  return(bird_sub)
  
}) |> bind_rows() 


# once the data is extracted we can now subset the cols based on the windclass and then rename to make 
# a single table. 

winds <- purrr::map(birdwind$id, function(i){ 

 # i = 1
  birdsub <- birdwind[i, ]
  
  wc <- as.character(st_drop_geometry(birdwind[i, "windclass"]) %>% pull())
  
  birdsub <- birdsub  |> 
    select(c(id, windclass, ends_with(wc)))
  
  names(birdsub) <- c("id", "windclass", "uwnd", "vwnd", "ws", "geom")
  
  birdsub
  
} )  |> bind_rows()  
         
  
# clean up the old cols

birdwind <- birdwind |>
  select(-c(ends_with(c("1", "2", "3", "4")), "windclass", "ID", "id"))


# join the matching data back together

birdwind_all <- st_join(birdwind, winds)


# calculate the wind angle 
# note the results are between -180 and 180  is the  
#atan2 is the angle relative to the x-axis in a graph +/-180 is West, +90 is N, 0 is E, -90 is S

birdwind_all <- birdwind_all |>
  mutate(windangle = (180/3.14)*atan2(uwnd, vwnd))


birdswind_df <- birdwind_all |>
  #st_coordinates() |>
  st_drop_geometry() 


#save the data
st_write(birdwind_all, path("02_outputs", "birdwind_all.gpkg"), driver = "GPKG")
write.csv(birdswind_df, path("02_outputs", "birdwind_all.csv"))
