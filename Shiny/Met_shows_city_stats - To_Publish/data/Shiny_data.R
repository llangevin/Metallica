Sys.getlocale()
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.getenv()
Sys.setenv(LANG = "en")
sessionInfo()

# clear workspace
rm(list = ls())

library(tidyverse)

#set working directory
setwd("~/Projects/Metallica")

#load the list of show
met_shows <- readRDS(file="./data/met_shows_20241215.Rda")

#remove cancelled shows
met_shows <- met_shows %>% filter(show_cancelled == 0)
#remove National anthem performance
met_shows <- met_shows %>% filter(show_ID != "2017-08-07-san-francisco-california")

#load metallica city show geolocalization
#cancelled shows have to be removed
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241215.Rda")

#Load additional Shows information
#cancelled shows already removed
met_show_info <- readRDS(file="./data/met_show_info_20241215.Rda")

#city stats for leaflet
met_shows_city_stats <- met_shows %>%
  group_by(city, state, country, continent) %>% 
  summarise(n_shows=n(), min_date=min(show_date), max_date=max(show_date)) %>%
  ungroup() %>%
  mutate(city_num=row_number()) %>%
  mutate(city_id=as.character(city_num))

met_shows_city_stats <- inner_join(met_shows_city_stats, met_city_lat_long, by = c('city', 'state', 'country', 'continent'))
met_shows_city_stats <- met_shows_city_stats %>%
  #select(city_num, city, state, country, continent, n_shows) %>%
  arrange(city_num, city, state, country, continent)

#Add info to met_shows
met_show_info <- met_show_info %>% select(show_ID, tour_value, Other_acts_value)
met_shows <- left_join(met_shows, met_show_info, by = c('show_ID'))
met_shows_city_stats_cn <- met_shows_city_stats %>% select(city, state, country, continent, city_num, city_id)
met_shows <- left_join(met_shows, met_shows_city_stats_cn, by = c('city', 'state', 'country', 'continent')) %>%
  select(city_id, show_ID, show_weblink, show_date, city, state, country, continent, venue, tour_value, Other_acts_value, show_number) %>%
  arrange(city_id, city, state, country, continent)
rm(met_shows_city_stats_cn)

#popup_info
met_shows_city_stats <- met_shows_city_stats %>%
  mutate(popup_info = ifelse(n_shows == 1, 
                             paste(paste('<b>City:</b>',met_shows_city_stats$city),
                                   paste('<b>Nb of Shows:</b>',met_shows_city_stats$n_shows),
                                   paste('<b>Date of Show:</b>',met_shows_city_stats$min_date),
                                   sep = '<br/>'), 
                             paste(paste('<b>City:</b>',met_shows_city_stats$city),
                                   paste('<b>Nb of Shows:</b>',met_shows_city_stats$n_shows),
                                   paste('<b>First Show:</b>',met_shows_city_stats$min_date),
                                   paste('<b>Last Show:</b>',met_shows_city_stats$max_date),
                                   sep = '<br/>')))

#saveRDS(met_shows_city_stats, file="~/Projects/Metallica/Shiny/Met_shows_city_stats - To_Publish/data/met_shows_city_stats_20241215.Rda")
#saveRDS(met_shows, file="~/Projects/Metallica/Shiny/Met_shows_city_stats - To_Publish/data/met_shows_20241215.Rda")

#met_shows_city_stats <- readRDS(file="~/Projects/Metallica/Shiny/data/met_shows_city_stats_20241215.Rda")
#met_shows <- readRDS(file="~/Projects/Metallica/Shiny/data/met_shows_20241215.Rda")