
# methdology for Redknot conversion to R

library(fs)
library(dplyr)
library(readr)
library(sf)
library(terra) #testing line
library(lidR) # cam test

# 1) read in csv from 01_inputs folder

bd <- read_csv(path("01_inputs", "DS 2 _P7165_Con_213835_250122134729.csv"),name_repair = "unique",locale=locale(encoding="latin1"))



# 2) convert to spatial points

bdsf <- st_as_sf(bd, coords = c("Longitude", "Latitude"), crs = 4326)


# first date and last date?
  

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




