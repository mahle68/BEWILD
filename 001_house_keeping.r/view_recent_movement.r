#script to monitor recent movement in golden eagles: juvies (eobs)
#7.7.2025 Elham Nourani, PhD.
#Konstanz, DE


library(tidyverse)
library(move2)
library(mapview)
library(sf)
library(lubridate)
library(units)


#read in the data

#creds <- movebank_store_credentials(username = "mahle68", rstudioapi::askForPassword())
GE_FN_id <- movebank_get_study_id("BEWILD_golden_eagle_global")

#timerange... past n weeks
n_days <- 14

end <- as.character(today() - 1) %>% 
  str_remove_all(pattern = "-") %>% 
  str_c("100000000")

start <- as.character(today() - (n_days)) %>% 
  str_remove_all(pattern = "-") %>% 
  str_c("100000000")


gps <- movebank_retrieve(study_id = GE_FN_id, sensor_type_id = "gps", #download data for wintering 
                         entity_type = "event",  attributes = "all",
                         timestamp_start = start,
                         timestamp_end = end) %>% 
  drop_na(individual_local_identifier)


#plot

gps_sf <- gps %>% 
  #remove the points at (0,0) ... 
  filter(!(location_lat == 0 & location_long == 0)) %>%
  drop_na("location_long") %>% 
  st_as_sf(coords = c("location_long", "location_lat"), crs = "EPSG:4326") %>% 
  mutate(hour = hour(timestamp),
         yday = yday(timestamp)) %>% 
  group_by(individual_local_identifier, yday(timestamp)) %>% 
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
  ) %>% 
  ungroup()


mapview(gps_sf, zcol = "individual_local_identifier")

mapview(gps_sf %>% filter(individual_local_identifier == "E28512_eobs12854"))
mapview(gps_sf %>% filter(individual_local_identifier == "E25585_eobs12857"))
mapview(gps_sf %>% filter(individual_local_identifier == "E29643_eobs12852"))
mapview(gps_sf %>% filter(individual_local_identifier == "E29715_eobs12850"))
mapview(gps_sf %>% filter(individual_local_identifier == "E28946_eobs12855"))


#look at hourly distance, colored by indiviudal ID
ggplot(gps_sf, aes(x = hour, y = dist, color = individual_local_identifier)) +
  geom_point() +
  labs(x = "Hour", y = "Distance", color = "Identifier") +
  theme_minimal()
