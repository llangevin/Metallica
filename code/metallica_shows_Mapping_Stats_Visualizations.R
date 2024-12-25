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
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241215.Rda")
leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = met_city_lat_long,
             lng = ~long, lat = ~lat,
             popup = paste(paste('<b>City:</b>',
                                 met_city_lat_long$city)))

#city stats
met_shows <- readRDS(file="./data/met_shows_20241215.Rda") #from metallica_past_tour_date script
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
  theme_dark()# +   
#  geom_text(aes(label = Count_City), nudge_x = 0, nudge_y = 2)

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

#################################################################################
#Shows Tour stats
met_show_info <- readRDS(file="./data/met_show_info_20241215.Rda")

#Tours with most shows
met_show_info %>% group_by(tour_value) %>% summarize(Count_Tour_Shows = n()) %>%
  filter(Count_Tour_Shows>=1 & is.na(tour_value) == F) %>% arrange(Count_Tour_Shows) %>%
  mutate(tour_value = factor(tour_value, levels = tour_value)) %>%
  ggplot(aes(tour_value, Count_Tour_Shows)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "Tour",
       title ="Metallica",
       subtitle = "Number of Shows per Tour") +
  theme_dark() +   
  geom_text(aes(label = Count_Tour_Shows), hjust = 0, nudge_x = 0.05, nudge_y = 0)

#Add begin and end Year in Tour label using show_date
tour_years <- met_show_info %>% group_by(tour_value) %>%
  summarize(Count_Tour_Shows = n(), tour_min = min(year(show_date)), tour_max = max(year(show_date))) %>%
  ungroup() %>% mutate(tour_value_year = paste(paste(tour_value, substr(tour_min,3,4), sep=' '), substr(tour_max,3,4), sep='-')) %>%
  arrange(Count_Tour_Shows) %>% filter(Count_Tour_Shows>=1 & is.na(tour_value) == F) %>%
  mutate(tour_value_year = factor(tour_value_year, levels = tour_value_year))

ggplot(data=tour_years, aes(tour_value_year, Count_Tour_Shows)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "Tour",
       title ="Metallica",
       subtitle = "Number of Shows per Tour") +
  theme_dark() +   
  geom_text(aes(label = Count_Tour_Shows), hjust = 0, nudge_x = 0.05, nudge_y = 0)

#tour_years %>% mutate(tour_value_year = paste(paste(tour_value, tour_min, sep=' '), tour_max, sep='-'))

cat(tour_years[1,]$tour_value, tour_years[1,]$tour_min, sep='-')
paste(tour_years[1,]$tour_value, tour_years[1,]$tour_min, sep='-')

#Other Act
#Number of shows for Other Act
max_oa <- max(1+ nchar(as.character(met_show_info$Other_acts_value)) -nchar( gsub(",", "", met_show_info$Other_acts_value)), na.rm=T)
oa_col <- paste0(rep("oa_", max_oa), sprintf("%02d", seq(1:max_oa)))

suppressWarnings({ 
  # Code that generates warning messages 
  met_show_info_oa_col <- met_show_info %>% dplyr::select(Other_acts_value) %>%
    tidyr::separate(col=Other_acts_value, sep=',' ,oa_col, extra = "drop") %>% tidyr::gather(oa_col) %>%
    filter(is.na(value) == F) %>% dplyr::select(value) %>% rename(Other_acts = value) %>%
    group_by(Other_acts) %>% summarize(Count_Other_acts = n())
})

met_show_info_oa_col %>%
  filter(Count_Other_acts>=25) %>% arrange(Count_Other_acts) %>% mutate(Other_acts = factor(Other_acts, levels = Other_acts)) %>%
  ggplot(aes(Other_acts, Count_Other_acts)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of Shows",
       x = "Other Acts",
       title ="Metallica",
       subtitle = "Number of Shows per Other Acts") +
  theme_dark() +   
  geom_text(aes(label = Count_Other_acts), hjust = 0, nudge_x = 0.05, nudge_y = 0)

#Song
#Most played songs
met_show_songs <- readRDS(file="./data/met_show_songs_20241215.Rda")
met_show_songs %>% group_by(song) %>% summarize(Count_Song_Shows = n()) %>%
  filter(Count_Song_Shows>=200 & is.na(song) == F) %>% arrange(Count_Song_Shows) %>% mutate(song = factor(song, levels = song)) %>%
  ggplot(aes(song, Count_Song_Shows)) +
  geom_col(fill = "grey", color = "black") +
  coord_flip() +
  labs(y = "Number of time played",
       x = "Song Title",
       title ="Metallica",
       subtitle = "Number of times the Song has been played in Show") +
  theme_dark() +   
  geom_text(aes(label = Count_Song_Shows), hjust = 0, nudge_x = 0.05, nudge_y = 0)

#Most played album/song

########################
#Using Metallica album themes from https://github.com/johnmackintosh/metallicaRt
#install.packages("remotes")
#library(remotes)
#remotes::install_github("johnmackintosh/metallicaRt")
library(metallicaRt)
#Using geom_col and geom_text
library(scales)
show_col(lightning_pal()(10),labels = FALSE)
library(dplyr)
met_shows_Year <- met_shows %>% group_by(show_year) %>% summarize(Count_Year = n()) %>% arrange(desc(Count_Year))
ggplot(met_shows_Year, aes(show_year, Count_Year)) +
  geom_col() +
  #scale_color_killem() +
  #scale_fill_killem() +
  scale_color_lightning() +
  scale_fill_lightning() +
  labs(y = "Number of Shows", 
       x = "Year",
       title ="Metallica",
       subtitle = "Number of Shows per Year") +
  theme_dark() +   
  geom_text(aes(label = Count_Year), nudge_x = 0, nudge_y = 2.5)

#metalli_palette("kill")
# basic treemap
library(treemap)
p <- treemap(met_shows_Year,
             index=c("show_year"),
             vSize="Count_Year",
             type="index",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
)    