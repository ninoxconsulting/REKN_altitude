#save the data
library(sf)
#install.packages("climaemet")
library(climaemet)
library(ggplot2)
library(fs)
library(dplyr)

bb <- st_read(path("02_outputs", "birds_final_proc","birdwind_all_20250426.gpkg"), quiet = TRUE) 

# format the data 

bb <- bb |>
  select(
    id, filename, datetime, shore_status,Fix, Alt.m., bearing, speed_kmh, geoid_height, ortho_height,
    ws, windangle, windangle_azmith, geom
  ) |>
  dplyr::mutate(
    bid = stringr::str_split_i(filename, "_", 4),
    bproj = stringr::str_split_i(filename, "-", 2)
  ) |>
  select(-c(filename))


# format colours 
bb <- bb |> 
  mutate(ortho_height_cat = case_when(
    ortho_height < 50 ~ "0 - 50",
    ortho_height >= 50 & ortho_height < 100 ~ "50 - 100",
    ortho_height >= 100 & ortho_height < 250 ~ "100 - 250",
    ortho_height >= 250 & ortho_height < 500 ~ "250 - 500",
    ortho_height >= 500 & ortho_height < 1000 ~ "500 - 1000",
    ortho_height >= 1000 & ortho_height < 2000 ~ "1000 - 2000",
    ortho_height >= 2000 ~ ">2000"
  )) |> 
  mutate(ortho_height_cat = factor(ortho_height_cat, 
                                   levels = c("0 - 50", "50 - 100", "100 - 250", "250 - 500", "500 - 1000", "1000 - 2000", ">2000")))


# format the data 
bbid <- as.numeric(unique(bb$bid))
length(bbid) #178

# summary with the bird list 
blist_all <- read.csv(path("02_outputs","reference_data_edited.csv")) |> 
  select(tag.id,tag.model) |> 
  mutate(tag.id = as.character(tag.id)) 

#blist <- blist_all |> 
#  filter(tag.id %in% bbid)
#length(blist$X) #130


bb <- left_join(bb, blist_all, by = c("bid" = "tag.id")) # join the data with the bird list

#48 which I dont have reference data for? 

#setdiff(bbid, blist_all $tag.id) # check if all birds are in the list
#setdiff( blist_all $tag.id, bbid) # check if all birds are in the list

bb_type <- bb |> 
  group_by(bid) |>
  summarise(
    n = n(),
    start = min(datetime),
    end = max(datetime),
    duration = as.numeric(difftime(end, start, units = "days")),
    .groups = "drop" 
  )|> 
  st_drop_geometry()

bb_type <- bb_type |> 
  left_join(blist_all, by = c("bid" = "tag.id")) 


# check the following data 

bb1 <- bb %>% filter(bb$bid == 224094) 
bb1 <- bb %>% filter(bb$bid == 230314) 
bb1 <- bb %>% filter(bb$bid == 233928) 
bb1 <- bb %>% filter(bb$bid == 233922) 
bb1 <- bb %>% filter(bb$bid == 232982) 



badids <- c(8110, 8111, #224094
            88401,  #230314
            4224, #233928
            3656) #233922


bb <- bb %>% filter(!id %in% badids) 

bb_type <- bb |> 
  group_by(bid) |>
  summarise(
    n = n(),
    start = min(datetime),
    end = max(datetime),
    duration = as.numeric(difftime(end, start, units = "days")),
    .groups = "drop" 
  )|> 
  st_drop_geometry()

bb_type <- bb_type |> 
  left_join(blist_all, by = c("bid" = "tag.id")) 





write.csv(bb, path("02_outputs", "birds_final_proc","birdwind_final.csv"), row.names = FALSE)
st_write(bb, path("02_outputs", "birds_final_proc","birdwind_final.gpkg"), row.names = FALSE, append = FALSE)









####################################################################################
# Summary plots - general all birds

bb <- st_read( path("02_outputs", "birds_final_proc","birdwind_final.gpkg"))

bbs <- bb |> 
  group_by(bid) |>
  st_drop_geometry() |>
  summarise(
    n = n(),
    start = min(datetime),
    end = max(datetime),
    duration = as.numeric(difftime(end, start, units = "days")),
    .groups = "drop"
    
  )


# review the onshore/offshore data

# summary of individuals by type of location data 
bbt <- bb |> 
  group_by(bid, shore_status) |>
  st_drop_geometry() |>
  summarise(
    n = n()
  )


# subset a group of bird which have lots of offshore data (for testing)

offshore <- bbt |> 
  filter(shore_status == "Offshore") |>
  filter(n > 20) |>
  select(bid)

bb_od <- bb |> 
  filter(bid %in% offshore$bid) 



# fix = quality of data 
# Alt.m. = altitude in meters - based on GPS 
# bearing = direction of travel calculated between points using geosphere (-180 to 180)
# speed_kmh = speed in km/h calculated between points
# geoid_height = height above the geoid (sea level) in meters
# ortho_height = height above the ellipsoid in meters
# ws = wind speed in m/s using the modelled CCMP data
# windangle = wind direction in degrees using the modelled CCMP data (-180 to 180)

# sort(unique(bb$speed_kmh, na.rm = TRUE)) # outliers
# sort(unique(bb$bearing, na.rm = TRUE))
# sort(unique(bb$geoid_height, na.rm = TRUE))
# sort(unique(bb$ortho_height, na.rm = TRUE))
# sort(unique(bb$ws, na.rm = TRUE))


# plot the speed and geoid height in geom_point
ggplot(bb, aes(x = speed_kmh, y = ortho_height, colour = bid)) +
  geom_point() +
  labs(title = "Speed vs Altitude",
       x = "Speed (km/h)",
       y = "Altitude (m)") +
  theme_minimal() + xlim(c(0, 500))+ theme(legend.position="none")
  

# plot the speed and geoid height in geom_point by type 
ggplot(bb, aes(x = speed_kmh, y = ortho_height, colour = shore_status)) +
  geom_point() +
  labs(title = "Speed vs Altitude",
       x = "Speed (km/h)",
       y = "Altitude (m) ortho-height") +
  theme_minimal() + xlim(c(0, 50)) 


# # plot the speed and geoid height in geom_point by type 
# ggplot(bb, aes(x = speed_kmh, y = geoid_height, colour = shore_status)) +
#   geom_point() +
#   labs(title = "Speed vs Altitude",
#        x = "Speed (km/h)",
#        y = "Altitude (m) geoid_height") +
#   theme_minimal() + xlim(c(0, 50))



# # summary of all points speeds 
# 
# ggplot(bb, aes(x = speed_kmh)) +
#   geom_histogram(bins = 30) +
#   labs(title = "Histogram of Speeds",
#        x = "Speed (km/h)",
#        y = "Frequency") +
#   theme_minimal() + xlim(c(0, 100)) 




















################################################################################

# # Plot - windrose example plots 
# 
# # summary of all points
# # TODO: note this does not work currently as need to convert from bearing to azmith? Still to do. 
# # need to convert wind angle (0 - 360)
# 
# rose <- ggwindrose(
#   speed = bb_od$ws,
#   direction = bb_od$windangle_azmith,
#   n_directions = 8,
#   n_speeds = 4,
#   #speed_cuts = seq(0, 16, 4),
#   legend_title = "Wind speed (m/s)",
#   calm_wind = 0,
#   n_col = 1,
#   plot_title = "Test Red Knot"
# )
# rose + labs(
#   subtitle = "2000-2020",
#   caption = "Source: AEMET"
#   
# )
# 


####################################################################################

# leaflet plots 

library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
#library (gganimate)
library(ggspatial)
library(leaflet)


## read in compiled data with movements and limit to rufa 
xx <- unique(bb_od$bid)

bb_od <- cbind(bb_od , st_coordinates(bb_od ))

sort(unique(round(bb_od$ortho_height,0))) # outliers


# select an example bird 
bi <- bb_od |> filter(bid == xx[1])

# pal <- colorNumeric(
# #  #palette = "viridis",
#   palette = "magma",
#   domain = unique(bi$ortho_height))

#pal <- colorQuantile("Reds", bi$ortho_height, 4,) # pretty = FALSE)

pal <- colorFactor(
  #palette = "viridis",
  palette = "Reds",
  domain = unique(bi$ortho_height_cat))


# select an icon 

# option 1:
# shipIcon  <- makeIcon(iconUrl = fs::path("C:/r_repo/2025_REKN/REKN_altitude/Pngtreeuparrow4565998.png"),
#                       iconWidth = 20,
#                       iconHeight = 20,
#                       iconAnchorX = 20,
#                       iconAnchorY = 20
#                       )
# option 2:
# shipIcon2 <- makeAwesomeIcon(icon = shipIcon,
#                              #library = "ion",
#                              #iconColor = "black",
#                              iconRotate = bi$windangle,
#                              squareMarker = FALSE,
#                              markerColor = "lightgray")

# option 3
# used icon embedded on libraty
shipIcon3 <- awesomeIcons(
  icon = "arrow-up",
  #icon  = shipIcon,
  #library = "ion",
  markerColor = "lightgray",
  iconColor = "black",
  spin = FALSE,
  extraClasses = NULL,
  squareMarker = FALSE,
  iconRotate = bi$windangle,
  fontFamily = "monospace",
  text = NULL
)

# option 4
# shipIcon4 <- makeAwesomeIcon(icon = "arrow-up",
#                              #library = "ion",
#                              #iconColor = "black",
#                              iconRotate = bi$windangle,
#                              squareMarker = FALSE,
#                              markerColor = "lightgray")


# create map 


birdmapall <- leaflet(bi) |> 
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB", group = "Background") |> 
  addPolylines(data =bi, lng = bi$X, lat = bi$Y,
               color = "grey",   opacity = 0.1, stroke = TRUE, group = "Bird trajectory") |> 
  addCircleMarkers(lng = bi$X, lat = bi$Y,
                   weight = 4,
                   color = ~pal(bi$ortho_height_cat),
                   fill = TRUE,
                   fillOpacity = 1,
                   label = ~bi$datetime,
                   group = "Bird Altitude",
                   radius = ~10 ,
                   popup = paste("Bird id:", bi$bid, "<br>",
                                 "Flight altitude:", round(bi$ortho_height,0), "m", "<br>",
                                 "Date:", bi$datetime,"<br>",
                                 "Wind speed:", round(bi$ws,0), "m/s", "<br>",
                                 "Wind direction:", round(bi$windangle,0), "<br>")) |>
  #addMarkers(lng = bi$X, lat = bi$Y,icon = shipIcon ) |> 
  #addPolylines(data =bi, lng = bi$X, lat = bi$Y,
  #             color = "grey",   opacity = 0.1, stroke = TRUE, group = "Bird trajectory") |> 
  addLegend("bottomright", pal = pal, values = ~bi$ortho_height_cat,
          title = "Flight Altitude",
           opacity = 1) |>
   addAwesomeMarkers(
     lng = bi$X, 
     lat = bi$Y,
     icon = shipIcon3,
     popup = paste("Wind speed:", round(bi$ws,0), "m/s", "<br>"),
     group = "Wind speed and direction") |> 
  addLayersControl(
    baseGroups = 
      "Background"
     #"Positron (minimal)",
    #  "World Imagery (satellite)"
    #)
    ,
    overlayGroups = c("Bird Altitude", "Bird trajectory", "Wind speed and direction"),
    options = layersControlOptions(collapsed = FALSE)
  )#|> 
 # addControl(title, position = "topleft", className="map-title")


birdmapall

# layer control 
#https://rstudio.github.io/leaflet/articles/showhide.html


# add markers 
#https://rstudio.github.io/leaflet/articles/markers.html#icon-markers
#https://leafletjs.com/examples/custom-icons/
#https://r-graph-gallery.com/182-add-circles-rectangles-on-leaflet-map.html

#colours
#https://rstudio.github.io/leaflet/articles/colors.html


#icons
#https://ionic.io/ionicons
