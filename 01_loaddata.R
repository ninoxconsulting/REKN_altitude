
# methdology for Redknot conversion to R

library(fs)
library(dplyr)
library(readr)
library(sf)


# 1) read in csv from 01_inputs folder

bd <- read_csv(path("01_inputs", "DS 2 _P7165_Con_213835_250122134729.csv"),name_repair = "unique",locale=locale(encoding="latin1"))



# 2) convert to spatial points

bdsf <- st_as_sf(bd, coords = c("Longitude", "Latitude"), crs = 4326)


# 3) calculate the flight distance and speed. 


## see note see code in other repo for this usign GCD 




# 4) convert the ellipsoid height 

#“Geoid Height Calculator” from the NSF GAGE Facility. 
# https://observablehq.com/@earthscope/geoid-height-calculator/2




# 5) Get wind data 

#https://podaac.jpl.nasa.gov/MEaSUREs-CCMP
# https://www.remss.com/measurements/ccmp/


