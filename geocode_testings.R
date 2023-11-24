#Add city geolocalization
library(dplyr)
library(tidygeocoder)

address_components <- data.frame(
  city=c('Glens Falls', 'San Juan', 'Manila', 'Chek Lap Kok', 'South Shetland Islands', 'New-York', 'Montreal', 'Paris', 'London', 'Glasgow', 'London', 'Glasgow', 'London', 'Glasgow'),
  state=c('NY', '', '', '', '', 'NY', 'QC', '', '', '', '', '', '', ''),
  country=c('United States', '', '', '', '', 'United States', 'Canada', 'France', 'England', 'Scotland', 'United Kingdom', 'United Kingdom', '', ''),
  stringsAsFactors=FALSE)

lat_longs <- address_components[1,] %>%
  geocode(city = city, state = state, country=country, method = "osm")

address_components[c(1,2),] %>%
  geocode(city = city, method = "osm")

#met_city_lat_long <- unique(met_shows[c("city","state","country")]) %>%
#  geocode(city = city, state = state, country=country, method = "osm")

check_na <- met_city_lat_long[is.na(met_city_lat_long$lat)==T | is.na(met_city_lat_long$long)==T,]

#Corrections
#Glenn Falls, NY, United States --> Glens Falls
#Victoriaville, ON, Canada --> QC
#South Shetland Islands, Antarctica --> do not use country to obtain geoloc
#Chek Lap Kok, Hong Kong --> do not use country to obtain geoloc
#Manila, Phillippines --> do not use country to obtain geoloc
#San Juan, Puerto Rico --> do not use country to obtain geoloc
#for cities in England, Northern Ireland, Scotland and Wales --> do not use country to obtain geoloc

#List of distinct city/state/country
city_state_country<- unique(met_shows[c("city","state","country")])

#Part 1
#
met_city_lat_long_part1 <- met_shows %>%
  select(city, state, country) %>%
  filter(country %in% c('England','Northern Ireland','Scotland','Wales','Hong Kong','Phillippines','Puerto Rico')) %>%
  #mutate(country="") %>%
  distinct(city, state, country) %>%
  geocode(city = city, method = "osm")

#Part 2
#
met_city_lat_long_part2 <- met_shows %>%
  select(city, state, country) %>%
  filter(!(country %in% c('England','Northern Ireland','Scotland','Wales','Hong Kong','Phillippines','Puerto Rico'))) %>%
  #mutate(country="") %>%
  distinct(city, state, country) %>%
  geocode(city = city, method = "osm")

met_city_lat_long <- rbind(met_city_lat_long_part1,met_city_lat_long_part2)

#save metallica city show geolocalization
saveRDS(met_city_lat_long, file="./data/met_city_lat_long_20231124.Rda")
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20231124.Rda")
