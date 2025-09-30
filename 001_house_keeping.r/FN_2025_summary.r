#script to sumamrize the data collected for golden eagles in Finland 2025: juvies (eobs)
#30.09.2025 Elham Nourani, PhD.
#Konstanz, DE


library(tidyverse)
library(move2)
library(mapview)
library(sf)
library(lubridate)
library(units)



#creds <- movebank_store_credentials(username = "mahle68", rstudioapi::askForPassword())
GE_FN_id <- movebank_get_study_id("BEWILD_golden_eagle_global")

################
## GPS #########
################

#download all GPS ---------------------------------------------------------------------------------
gps <- movebank_retrieve(study_id = GE_FN_id, sensor_type_id = "gps", attributes = "all", entity_type = "event", 
                         individual_local_identifier = c("E29715_eobs12850", "E29643_eobs12852", "E28512_eobs12854",
                                                         "E25585_eobs12857", "E28946_eobs12855"))


#calculate day since tagging
#open meta-data
meta_data <- read.csv("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Projects/BEWILD/meta_data/Golden_eagle_metadata(Sheet1).csv") %>% 
  mutate(deployment_dt_utc = as.POSIXct(strptime(deployment_dt_utc, format = "%d/%m/%Y %H:%M"), tz = "UTC"))

gps <- gps %>% 
  left_join(meta_data %>% select(individual_local_identifier, deployment_dt_utc)) %>%
  mutate(days_since_tagging = floor(difftime(timestamp, deployment_dt_utc, unit = "days")))

#Plot all data ------------------------------------------------------------------------------------

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


#Plot: time lag ~ date  ------------------------------------------------------------------------------------

gps <- gps %>% 
  group_by(individual_local_identifier, days_since_tagging) %>%
  arrange(timestamp, .by_group = T) %>% 
  mutate(time_lag_sec = if_else(row_number() == 1, 0, difftime(timestamp, lag(timestamp), units = "secs") %>% as.numeric())) %>% 
  ungroup()
  
ggplot(gps, aes(x = days_since_tagging, y = time_lag_sec, color = individual_local_identifier)) +
  geom_point() +
  labs(x = "day since tagging", y = "time lag", color = "Identifier") +
  ylim(0, 1800) +  #only plot time lags shorter than half an hour
  theme_minimal()


ggplot(gps, aes(x = as.character(as.Date(timestamp)), y = time_lag_sec, color = individual_local_identifier)) +
  geom_point() +
  labs(x = "date", y = "time lag ()", color = "Identifier") +
  #ylim(0, 1800) +  #only plot time lags shorter than half an hour
  theme_minimal()

#Plot: battery life ~ date  ------------------------------------------------------------------------------------


ggplot(gps, aes(x = days_since_tagging, y = eobs_battery_voltage, color = individual_local_identifier)) +
  geom_point() +
  labs(x = "day since tagging", y = "battery level", color = "Identifier") +
  theme_minimal()


ggplot(gps, aes(x = as.Date(timestamp), y = eobs_battery_voltage, color = individual_local_identifier)) +
  geom_point() +
  labs(x = "date", y = "battery level", color = "Identifier") +
  theme_minimal() +
  scale_x_date(date_breaks = "7 days", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#Plot: distance  ------------------------------------------------------------------------------------
#look at hourly distance, colored by indiviudal ID
ggplot(gps_sf, aes(x = hour, y = dist, color = individual_local_identifier)) +
  geom_point() +
  labs(x = "Hour", y = "Distance", color = "Identifier") +
  theme_minimal()


################
## IMU #########
################


