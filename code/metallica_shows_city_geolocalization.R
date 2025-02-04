#Create met_city_lat_long dataset
#Add city geolocalization
library(tidyverse)
library(tidygeocoder)

#load the list of show
#File created in metallica_past_tour_date.R script
met_shows <- readRDS(file="./data/met_shows_20231120.Rda")

#Corrections
#Glenn Falls, NY, United States --> Glens Falls
#Victoriaville, ON, Canada --> QC
#South Shetland Islands, Antarctica --> do not use country to obtain geoloc
#Chek Lap Kok, Hong Kong --> do not use country to obtain geoloc
#Manila, Phillippines --> Phillippines misspelled?, Philippines, Country in Asia
#Manila, Phillippines --> do not use country to obtain geoloc
#San Juan, Puerto Rico --> do not use country to obtain geoloc
#for cities in England, Northern Ireland, Scotland and Wales --> do not use country to obtain geoloc

#Part 1
#United Kingdom cities geolocalization info obtain without using country
met_city_lat_long_part1 <- metus_shows_new_cities %>%
  select(city, state, country) %>%
  filter(country %in% c('Antarctica','England','Northern Ireland','Scotland','Wales','Hong Kong','Phillippines','Puerto Rico')) %>%
  #mutate(country="") %>%
  distinct(city, state, country)

if (dim(met_city_lat_long_part1)[1] >0) {
  met_city_lat_long_part1 <- met_city_lat_long_part1 %>%
    geocode(city = city, method = "osm")
}

#Part 2
#Using city/state/country information to get geolocalization
met_city_lat_long_part2 <- metus_shows_new_cities %>%
  select(city, state, country) %>%
  filter(!(country %in% c('Antarctica','England','Northern Ireland','Scotland','Wales','Hong Kong','Phillippines','Puerto Rico'))) %>%
  #mutate(country="") %>%
  distinct(city, state, country) %>%
  geocode(city = city, state = state, country=country, method = "osm")

#use line below instead if Part 1 does not have cases
#met_city_lat_long_new <- met_city_lat_long_part2
if (dim(met_city_lat_long_part1)[1] >0) {
  met_city_lat_long_new <- rbind(met_city_lat_long_part1,met_city_lat_long_part2)
  rm(met_city_lat_long_part1)
} else {
  met_city_lat_long_new <- met_city_lat_long_part2
}
rm(met_city_lat_long_part2)

#Adding Continent
met_city_continent <-met_shows %>%
  distinct(city, state, country, continent)
met_city_lat_long <- left_join(met_city_continent, met_city_lat_long, by = c('city', 'state', 'country')) 
met_city_lat_long <- met_city_lat_long %>% arrange(city, state, country)

#save metallica city show geolocalization
saveRDS(met_city_lat_long, file="./data/met_city_lat_long_20241016.Rda")
#save the list of show in CSV
met_city_lat_long_csv <- "./data/metallica_city_lat_long.csv"
if (file.exists(met_city_lat_long_csv)) {file.remove(met_city_lat_long_csv)}
write.csv(met_city_lat_long,met_city_lat_long_csv, row.names = T)

#load metallica city show geolocalization
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241016.Rda")

################################################
#Mapping the shows
# some standard map packages.
install.packages(c("maps", "mapdata"))
install.packages(c("leaflet"))
library(leaflet)
leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = met_city_lat_long,
             lng = ~long, lat = ~lat,
             popup = paste(paste('<b>City:</b>',
                                 met_city_lat_long$city)))

#city stats
met_shows_city_stats <- met_shows %>%
  filter(show_cancelled == 0) %>%
  group_by(city, state, country) %>% 
  summarise(n_shows=n(), min_date=min(show_date), max_date=max(show_date))

#met_shows_city_stats <- left_join(met_shows_city_stats, met_city_lat_long, by = c('city', 'state', 'country'))
#inner join used to remove cancelled shows
met_shows_city_stats <- inner_join(met_shows_city_stats, met_city_lat_long, by = c('city', 'state', 'country'))

leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = met_shows_city_stats,
             lng = ~long, lat = ~lat,
             popup = paste(paste('<b>City:</b>',met_shows_city_stats$city),
                           paste('<b>Nb of Shows:</b>',met_shows_city_stats$n_shows),
                           paste('<b>First Show:</b>',met_shows_city_stats$min_date),
                           paste('<b>Last Show:</b>',met_shows_city_stats$max_date),
                           sep = '<br/>'))