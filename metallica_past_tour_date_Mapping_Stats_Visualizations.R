Sys.getlocale()
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.getenv()
Sys.setenv(LANG = "en")
sessionInfo()

# clear workspace
rm(list = ls())

library(tidyverse)
library(rvest)

setwd("C:/Users/Utilisateur/Documents/Projects/Metallica")
################################################
#Mapping the shows
# some standard map packages.
install.packages(c("maps", "mapdata"))
install.packages(c("leaflet"))
library(leaflet)
#met_city_lat_long from C:/Users/Utilisateur/Documents/Projects/Metallica/metillica_shows_city_geolocalization.R
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20231124.Rda")
leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = met_city_lat_long,
             lng = ~long, lat = ~lat,
             popup = paste(paste('<b>City:</b>',
                                 met_city_lat_long$city)))

#city stats
met_shows <- readRDS(file="./data/met_shows_20231120.Rda") #from metallica_past_tour_date script
met_shows_city_stats <- met_shows %>%
  group_by(city, state, country) %>% 
  summarise(n_shows=n(), min_date=min(show_date), max_date=max(show_date))

met_shows_city_stats <- left_join(met_shows_city_stats, met_city_lat_long, by = c('city', 'state', 'country'))

#corrections for Antartica
met_shows_city_stats[456,] <- left_join(met_shows_city_stats[456,-c(7,8)], met_city_lat_long_part1[12,], by = c('city', 'state', 'country'))
met_city_lat_long[184,] <- left_join(met_city_lat_long[184,-c(4,5)], met_city_lat_long_part1[12,], by = c('city', 'state', 'country'))
met_city_lat_long_part1[12,]
rm(met_shows_city_stats_test)
leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = met_shows_city_stats,
             lng = ~long, lat = ~lat,
             popup = paste(paste('<b>City:</b>',met_shows_city_stats$city),
                           paste('<b>Nb of Shows:</b>',met_shows_city_stats$n_shows),
                           paste('<b>First Show:</b>',met_shows_city_stats$min_date),
                           paste('<b>Last Show:</b>',met_shows_city_stats$max_date),
                           sep = '<br/>'))

#########################################
#Stats & Visualization
#Distribution of Shows per year
library(ggplot2)
#Using geom_bar
ggplot(met_shows, aes(show_year)) +
  geom_bar(fill = "grey", color = "black") +
  labs(y = "Number of Shows", 
       x = "Year",
       title ="Metallica",
       subtitle = "Number of Shows per Year") +
  theme_dark()

#Using geom_col and geom_text
library(dplyr)
met_shows_Year <- met_shows %>% group_by(show_year) %>% summarize(Count_Year = n()) %>% arrange(desc(Count_Year))
ggplot(met_shows_Year, aes(show_year, Count_Year)) +
  geom_col(fill = "grey", color = "black") +
  labs(y = "Number of Shows", 
       x = "Year",
       title ="Metallica",
       subtitle = "Number of Shows per Year") +
  theme_dark() +   
  geom_text(aes(label = Count_Year), nudge_x = 0, nudge_y = 2.5)

#Cities with most shows
met_shows %>% group_by(city) %>% summarize(Count_City = n()) %>%
  filter(Count_City>=15) %>% arrange(Count_City) %>% mutate(city = factor(city, levels = city)) %>%
  ggplot(aes(city, Count_City)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "City",
       title ="Metallica",
       subtitle = "Number of Shows per City") +
  theme_dark()

#States with most shows
met_shows %>% filter(country=='United States') %>% group_by(state) %>% summarize(Count_state = n()) %>%
  filter(Count_state>=1) %>% arrange(Count_state) %>% mutate(state = factor(state, levels = state)) %>%
  ggplot(aes(state, Count_state)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "State",
       title ="Metallica",
       subtitle = "Number of Shows per state") +
  theme_dark() +   
  geom_text(aes(label = Count_state), nudge_x = 0, nudge_y = 2)
#geom_text(aes(label = Count_state), nudge_x = 0, nudge_y = nchar(as.character(Count_state))) 

#Countries with most shows
met_shows %>% group_by(country) %>% summarize(Count_City = n()) %>%
  filter(Count_City>=15) %>% arrange(Count_City) %>% mutate(country = factor(country, levels = country)) %>%
  ggplot(aes(country, Count_City)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "Country",
       title ="Metallica",
       subtitle = "Number of Shows per Country") +
  theme_dark() +   
  geom_text(aes(label = Count_City), hjust = 0, nudge_x = 0.05, nudge_y = 0) 

met_shows_Country <- met_shows %>% group_by(country) %>% summarize(Count_City = n()) %>%
  filter(Count_City>=15) %>% arrange(Count_City) %>% mutate(country = factor(country, levels = country))
ggplot(met_shows_Country,aes(country, Count_City)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "Country",
       title ="Metallica",
       subtitle = "Number of Shows per Country") +
  theme_dark() +   
  geom_text( 
    data=met_shows_Country %>% filter(country=='United States'), # Filter data first
    aes(label=Count_City), hjust = 1.05, nudge_x = 0, nudge_y = 0
  ) +
  geom_text( 
    data=met_shows_Country %>% filter(country!='United States'), # Filter data first
    aes(label=Count_City), hjust = 0, nudge_x = 0.05, nudge_y = 0
  )
rm(met_shows_Country)

#Venues with most shows
met_shows %>% group_by(venue) %>% summarize(Count_Venue = n()) %>%
  filter(Count_Venue>=6) %>% arrange(Count_Venue) %>% mutate(venue = factor(venue, levels = venue)) %>%
  ggplot(aes(venue, Count_Venue)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "Venue",
       title ="Metallica",
       subtitle = "Number of Shows per Venue") +
  theme_dark() +   
  geom_text(aes(label = Count_Venue), hjust = 0, nudge_x = 0.05, nudge_y = 0)

#Adding city, state and country to label
met_shows %>% mutate(venue = paste0(venue,', ',show_venue_city)) %>% group_by(venue) %>% summarize(Count_Venue = n()) %>%
  filter(Count_Venue>=6) %>% arrange(Count_Venue) %>% mutate(venue = factor(venue, levels = venue)) %>%
  ggplot(aes(venue, Count_Venue)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "Venue",
       title ="Metallica",
       subtitle = "Number of Shows per Venue") +
  theme_dark() +   
  geom_text(aes(label = Count_Venue), hjust = 0, nudge_x = 0.05, nudge_y = 0)
