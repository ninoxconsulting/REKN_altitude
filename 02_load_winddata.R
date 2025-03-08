
library(RCurl) 
library(terra)
library(fs)
library(terra)

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

# note TIME has been dropped. but not a huge problem aas we have the date_time stamp. 

# format back date in format to filter the data.
# ie date will be converted to ymd and time will be grouped into one of 4 categories 
#  (based on the closest time to one of 6 hours blocks ; each day has a 0, 6, 12, and 18) 
bird$date_file <-  as.character(unique(ymd_hms(bird$datetime)))
bird$date_file <- gsub("-", "", bird$date_file)
bird$date_file <- sub(" .*", "",  bird$date_file)

# select which time set to pull from 

library(tidyverse)
library(lubridate)
library(purrr)

get_time <- function(time) {
  time %>%
    stringr::str_split(" ") %>%
    purrr::map_chr(2) %>%
    lubridate::hms()
}

bird$timeclass_file <- get_time(bird$datetime)

difftime(bird$timeclass_file[1] , "01H 00M 00S", units = "mins"))

difftime(hms("17H 29M 52S"), hms("01H 01M 01S"), units = "mins")

#i <- interval(ymd("2017-01-01"), d)               ## 2017-01-01 UTC--2017-11-28 UTC j <- d %--% ymd("2017-12-31")                      ## 2017-11-28 UTC--2017-12-31 UTC
#dt <- ymd_hms("2021-05-24 17:29:52")
#v <-c(dt, dt + 100, dt + 1000); int_diff(v) 




# need to pull the timestamp closest to the actual timestamp 





# get a list of the unique dates in the dataset. 
unique_dates <- unique(bird$date_file)

# convert the unique dates into filenames to download 
if (!dir.exists(path("00_downloads"))){
  dir.create(path("00_downloads"))
}


# this steps through the list of unique dates and downloads the corresponding files for each unique date.

purrr::map(unique_dates, function(i){
  
  i <- unique_dates[1]
  
  fyear <- substr(i, 1, 4)
  fmonth <- substr(i, 5, 6)
  
  fname <- paste0("CCMP_Wind_Analysis_", i, "_V03.1_L4.nc")
  
  # check if file already exists
  if(file.exists(fs::path("00_downloads", fname))){
    cli::cli_alert("File already exists")

      } else {
  
  url <- paste0("https://data.remss.com/ccmp/v03.1/Y", fyear, "/M", fmonth, "/", fname)
  download.file(url, destfile = fs::path("00_downloads", basename(url)),mode = "wb")
      }
  
})





# how to read in a file. 

nc_dir <- fs::path("00_downloads", "CCMP_Wind_Analysis_20210524_V03.1_L4.nc")
library(terra)

aa <- rast(nc_dir) 

# this reads in as a stacked rast with 16 layers. 
# u – Zonal Wind (west-east) (m/s)
# v – Meridional Wind (north-south) (m/s)
# Positive v wind is blowing from the south, Positive u wind is blowing from the west


# stack to covariates 

#[1] "uwnd_1" "uwnd_2" "uwnd_3" "uwnd_4" "vwnd_1" "vwnd_2" "vwnd_3" "vwnd_4" "ws_1"   "ws_2"   "ws_3"  
#[12] "ws_4"   "nobs_1" "nobs_2" "nobs_3" "nobs_4"

# at each time count, 0 , 6, 12, 18 hours.


# projec to WGS84? 


# figure out a way to extract the values for each point at the dates and times needed. 




# calculate w/e winds and n/s winds 





# caclculate wind angle using 

#(180/3.14)*atan2( "uwnd" , "vwnd" )



# final location = speed, altitude, direction of wind at time. 




# summarise for each bird species 





















### Rough working using the netcdf files. 
## this might be obsolete now
# 
# library(RNetCDF)
# 
# nc_dir <- path("01_inputs", "wind_raw", "CCMP_Wind_Analysis_20210525_V03.1_L4.nc")
# 
# url <- "https://data.remss.com/ccmp/v03.1/Y2021/M05/CCMP_Wind_Analysis_20210529_V03.1_L4.nc"
# data <- open.nc(nc_dir)
# 
# file.inq.nc(data)
# 
# file.inq.nc(data)["format"]
# 
# print.nc(data)
# 
# # look at the details 
# att.get.nc(data, "NC_GLOBAL", "title")
# att.get.nc(data, "NC_GLOBAL", "title")
# [1] "RSS CCMP V3.1 6-hourly surface winds (Level 4)"
# att.get.nc(data, "NC_GLOBAL", "Conventions")
# [1] "CF-1.7 ACDD-1.3"
# 
# 
# #get the variable
# latitude <- var.get.nc(data, "latitude")
# latitude_subset <- var.get.nc(data, "latitude", start=c(1), count=c(4))
# print(latitude_subset)
# 
# 
# uwnd <- var.get.nc(data, "uwnd")
# uwnd <- var.get.nc(data, "uwnd", start = c(NA, NA, 1), count = c(NA, NA,1)) #long, lat , time
# vwnd <- var.get.nc(data, "vwnd")#long, lat , time
# ws <- var.get.nc(data, "ws")#long, lat , time
# nobs <- var.get.nc(data, "nobs")#long, lat , time
# 
# 
# # get time variable 
# att.get.nc(data, "time", "units")
# #> att.get.nc(data, "time", "units")
# #[1] "hours since 1987-01-01 00:00:00"
# 
# 
# desire_date <- as.Date("2021-05-25")
# days_since_1987 <- as.numeric(difftime(desire_date, as.Date("1987-01-01"), units = "hours"))
# 
# time <- var.get.nc(data, "time")
# time_index <- which(time == days_since_1987)
# time_index
# 
# 
# # version x2 ; 
# 
# #https://pjbartlein.github.io/REarthSysSci/netCDF.html#reading-a-netcdf-data-set-using-the-ncdf4-package
# 
# nc_file <- path("01_inputs", "wind_raw", "CCMP_Wind_Analysis_20210525_V03.1_L4.nc")
# 
# 
# library(ncdf4)
# library(CFtime)
# library(lattice)
# library(RColorBrewer)
# 
# ncin <- nc_open(nc_file)
# print(ncin)
# 
# # get longitude and latitude
# lon <- ncvar_get(ncin,"longitude")
# nlon <- dim(lon)
# head(lon)
# 
# lat <- ncvar_get(ncin,"latitude")
# nlat <- dim(lat)
# head(lat)
# print(c(nlon,nlat))
# 
# 
# time <- ncvar_get(ncin,"time")
# time
# 
# tunits <- ncatt_get(ncin,"time","units")
# tunits
# 
# 
# 
# # get the wind data
# dname <- "uwnd"
# tmp_array <- ncvar_get(ncin,dname)
# dlname <- ncatt_get(ncin,dname,"long_name")
# dunits <- ncatt_get(ncin,dname,"units")
# fillvalue <- ncatt_get(ncin,dname,"_FillValue")
# dim(tmp_array)
# 
# 
# title <- ncatt_get(ncin,0,"title")
# institution <- ncatt_get(ncin,0,"institution")
# datasource <- ncatt_get(ncin,0,"source")
# references <- ncatt_get(ncin,0,"references")
# history <- ncatt_get(ncin,0,"history")
# Conventions <- ncatt_get(ncin,0,"Conventions")
# 
# 
# cf <- CFtime(tunits$value, calendar = "standard", time)
# 
# timestamps <- as_timestamp(cf) # get character-string times
# timestamps
# class(timestamps)
# time_cf <- parse_timestamps(cf, timestamps) # parse the string into date components
# time_cf
# class(time_cf)
# 
# 
# tmp_array[tmp_array==fillvalue$value] <- NA
# 
