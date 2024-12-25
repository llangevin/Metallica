library(shiny)
library(leaflet)
library(DT)

setwd("~/Projects/Metallica")

#city geolocalization
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241017.Rda")

#city stats
met_shows_city_stats <- readRDS(file="./data/met_shows_20241017.Rda") %>%
  group_by(city, state, country, continent) %>% 
  summarise(n_shows=n(), min_date=min(show_date), max_date=max(show_date)) %>%
  ungroup()  %>% mutate(city_num=row_number())

met_shows_city_stats <- left_join(met_shows_city_stats, met_city_lat_long, by = c('city', 'state', 'country', 'continent'))
#data <- met_shows_city_stats %>%
#  select(city_num, city, state, country, continent, n_shows) %>%
#  arrange(city_num, city, state, country, continent)

#data <- met_shows_city_stats
#met_shows <- readRDS(file="./data/met_shows_20241017.Rda")
#met_shows <- left_join(met_shows, met_shows_city_stats, by = c('city', 'state', 'country', 'continent'))
#data <- met_shows %>%
#  select(city_num, show_date, city, state, country, continent, venue, n_shows) %>%
  #arrange(show_date, city, state, country, continent)
#  arrange(city_num, show_date, city, state, country, continent)