
library(RCurl) 
library(terra)
library(fs)
library(terra)

# 5) Get wind data 

# https://www.remss.com/measurements/ccmp/

#data.remss.com/ccmp/v03.1/Y2021/M05/

# methods to download the wind files direct from web. 

url <- "https://data.remss.com/ccmp/v03.1/Y2021/M05/CCMP_Wind_Analysis_20210529_V03.1_L4.nc"

url <- "https://data.remss.com/ccmp/v03.1/Y2021/M05/CCMP_Wind_Analysis_20210501_V03.1_L4.nc"

download.file(url, basename(url))




#https://data.remss.com/ccmp/
#use version 3.1 = up to 2024 dataset 

#https://podaac.jpl.nasa.gov/MEaSUREs-CCMP
# https://www.remss.com/measurements/ccmp/

# dealing with netcdf files 
#https://www.youtube.com/watch?v=Xer1XBm3sns

#install.packages("RNetCDF")

# how to read in a file. 

nc_dir <- path("01_inputs", "wind_raw", "CCMP_Wind_Analysis_20210525_V03.1_L4.nc")

aa <- rast(nc_dir)



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
