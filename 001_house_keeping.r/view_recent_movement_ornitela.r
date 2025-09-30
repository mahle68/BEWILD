#script to monitor recent movement in golden eagles: adults (ornitela)
#7.7.2025 Elham Nourani, PhD.
#Konstanz, DE


library(tidyverse)
library(move2)
library(mapview)
library(sf)
library(lubridate)
library(units)


#read in the data

GE_FN <- list.files("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Projects/BEWILD/FN_ornitela_data/Jul7_2025", full.names = T) %>% 
  map_dfr(read.csv) %>% #read all in and make one dataframe
  mutate(timestamp = as.POSIXct(UTC_datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))



gps_sf <- GE_FN %>% 
  drop_na("Latitude") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326") %>% 
  mutate(hour = hour(timestamp),
         yday = yday(timestamp)) %>% 
  group_by(device_id, yday(timestamp)) %>% 
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
  ) %>% 
  ungroup()


mapview(gps_sf, zcol = "device_id")

#look at hourly distance, colored by indiviudal ID
ggplot(gps_sf, aes(x = hour, y = dist, color = device_id)) +
  geom_point() +
  labs(x = "Hour", y = "Distance", color = "Identifier") +
  theme_minimal()

ggplot(gps_sf, aes(x = hour, y = dist, color = Altitude_m)) +
  geom_point() +
  labs(x = "Hour", y = "Distance", color = "Identifier") +
  theme_minimal()

ggplot(gps_sf, aes(x = hour, y = dist, color = Altitude_m)) +
  geom_point() +
  labs(x = "Hour", y = "Distance", color = "Identifier") +
  theme_minimal()



###odba
# Function to calculate ODBA for each row
calculate_odba <- function(x, y, z) {
  return(abs(x) + abs(y) + abs(z))
}


gps_sf <- gps_sf %>% 
  mutate(mutate(ODBA = abs(as.numeric(acc_x)) + abs(as.numeric(acc_y)) + abs(as.numeric(acc_z))))

# Apply the function to each row
gps_sf$ODBA <- apply(gps_sf, 1, function(row) calculate_odba("acc_x", "acc_y", "acc_z"))

# Print the updated data frame with ODBA
print(data)
