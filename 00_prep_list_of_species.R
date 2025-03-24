
# Load R Packages
library(dplyr)
library(fs)
library(readr)

## DATA PRE-PROCESSING ##
# Read CSV Data
indata <- path("01_inputs")

list.files(indata)


# Read CSV


birdsmove<- read_csv(path(indata, "final_tags_list_edited.csv"))
birdall <- read_csv(path(indata, "reference_data_edited.csv"))



birdall <- birdall |>  
  select(c(animal.id, deploy.on.date,tag.manufacturer.name, tag.id, tag.model,
           tag.serial.no, study.site, proj)) |> 
  mutate(alt_include = case_when(
    proj %in% c("Johnson_GPS","Newstead", "Mingnan","Mispillion") ~ "no",
    proj == "sthcarolina_arctic" ~ "maybe",
    TRUE ~ "yes"
  )) 

birdall <- birdall  |>  
  mutate(alt_include = case_when(
    tag.model == "Sunbird Solar Argos" ~ "no",
    TRUE ~ alt_include
  ))
  


birdall <- left_join(birdall, birdsmove, by = c("tag.id" = "tag.id"))
     
      
write.csv(birdall, path(indata, "sp_altitude_list.csv"))
                    