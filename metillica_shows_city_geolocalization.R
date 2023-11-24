#Add city geolocalization
library(dplyr)
library(tidygeocoder)

#Corrections
#Glenn Falls, NY, United States --> Glens Falls
#Victoriaville, ON, Canada --> QC
#South Shetland Islands, Antarctica --> do not use country to obtain geoloc
#Chek Lap Kok, Hong Kong --> do not use country to obtain geoloc
#Manila, Phillippines --> do not use country to obtain geoloc
#San Juan, Puerto Rico --> do not use country to obtain geoloc
#for cities in England, Northern Ireland, Scotland and Wales --> do not use country to obtain geoloc

#Part 1
#United Kingdom cities geolocalization info obtain without using country
met_city_lat_long_part1 <- met_shows %>%
  select(city, state, country) %>%
  filter(country %in% c('England','Northern Ireland','Scotland','Wales','Hong Kong','Phillippines','Puerto Rico')) %>%
  #mutate(country="") %>%
  distinct(city, state, country) %>%
  geocode(city = city, method = "osm")

#Part 2
#Using city/state/country information to get geolocalization
met_city_lat_long_part2 <- met_shows %>%
  select(city, state, country) %>%
  filter(!(country %in% c('England','Northern Ireland','Scotland','Wales','Hong Kong','Phillippines','Puerto Rico'))) %>%
  #mutate(country="") %>%
  distinct(city, state, country) %>%
  geocode(city = city, method = "osm")

met_city_lat_long <- rbind(met_city_lat_long_part1,met_city_lat_long_part2)
rm(met_city_lat_long_part1)
rm(met_city_lat_long_part2)

#save metallica city show geolocalization
saveRDS(met_city_lat_long, file="./data/met_city_lat_long_20231124.Rda")
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20231124.Rda")
