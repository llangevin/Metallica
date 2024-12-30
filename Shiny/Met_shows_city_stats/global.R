library(tidyverse)
library(shiny)
library(leaflet)
library(DT)

setwd("~/Projects/Metallica")

#city geolocalization
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241215.Rda")

#city stats
met_shows_city_stats <- readRDS(file="./data/met_shows_20241215.Rda") %>%
  group_by(city, state, country, continent) %>% 
  summarise(n_shows=n(), min_date=min(show_date), max_date=max(show_date)) %>%
  ungroup()  %>% mutate(city_num=row_number())

met_shows_city_stats <- left_join(met_shows_city_stats, met_city_lat_long, by = c('city', 'state', 'country', 'continent'))
#saveRDS(met_shows_city_stats, file="~/Projects/Metallica/Shiny/Met_shows_city_stats/data/met_shows_city_stats_20241215.Rda")
#data <- met_shows_city_stats
data <- met_shows_city_stats %>%
  select(city_num, city, state, country, continent, n_shows) %>%
  arrange(city_num, city, state, country, continent)
