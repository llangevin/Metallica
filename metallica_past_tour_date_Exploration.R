Sys.getlocale()
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.getenv()
Sys.setenv(LANG = "en")
sessionInfo()

# clear workspace
rm(list = ls())

library(tidyverse)
library(rvest)

#read https://www.metallica.com/tour/past/?pg=1
m_page <- read_html('https://www.metallica.com/tour/past/?pg=1')

#ID of the show
#.show
m_page %>% html_nodes(".show") %>% html_text()
m_page %>% html_nodes(".show") %>% html_attr("data-show-id")
show_ID <- m_page %>% html_nodes(".show") %>% html_attr("data-show-id")

#Place of the show
#.venue-name
m_page %>% html_nodes(".venue-name") %>% html_text()
gsub("\n@\n"," @ ", m_page %>% html_nodes(".venue-name") %>% html_text() %>% .[5])
gsub("\n","", m_page %>% html_nodes(".venue-name") %>% html_text() %>% .[5])
venue <- gsub("\n@\n"," @ ", m_page %>% html_nodes(".venue-name") %>% html_text())
venue <- gsub("\n","", venue)

#Town/State/Country of the show
#.venue-city p
m_page %>% html_nodes(".venue-city") %>% html_text()
show_venue_city <- gsub("\n@\n"," @ ", m_page %>% html_nodes(".venue-city") %>% html_text())
show_venue_city <- gsub("\n@\n"," @ ", m_page %>% html_nodes(".venue-city") %>% html_text() %>% .[5])
show_venue_city <- gsub("\n","", show_venue_city)
length(show_venue_city)
gregexpr(",",show_venue_city, fixed = TRUE)
show_venue_city_attr <- gregexpr(",",show_venue_city, fixed = TRUE)
sum(attr(gregexpr(",",show_venue_city, fixed = TRUE)[[1]],'match.length'))
sum(attr(gregexpr(",",show_venue_city, fixed = TRUE)[[20]],'match.length'))
#Numbers of "," in show_venue_city, 2:city/country, 3:city/state/country
sapply(gregexpr(",",show_venue_city, fixed = TRUE), function(y) sum(attr(y, "match.length"))) #int [1:20]
show_venue_city_dim <- sapply(gregexpr(",",show_venue_city, fixed = TRUE), function(y) sum(attr(y, "match.length"))) #int [1:20]
#Positions of "," in show_venue_city
gregexpr(",",show_venue_city, fixed = TRUE)[[1]][1]
gregexpr(",",show_venue_city, fixed = TRUE)[[1]][2]
gregexpr(",",show_venue_city, fixed = TRUE)[[20]][1]

#show_venue_city with one ","
show_venue_city[20]
show_venue_city_dim[20]
show_venue_city_attr[[20]][1]
substr(show_venue_city[20],1,-1 + show_venue_city_attr[[20]][1])
substr(show_venue_city[20],1 + show_venue_city_attr[[20]][1], nchar(show_venue_city[[20]]))

#show_venue_city with 2 ","
show_venue_city[1]
show_venue_city_dim[1]
show_venue_city_attr[[1]][1]
show_venue_city_attr[[1]][2]
substr(show_venue_city[1],1,-1 + show_venue_city_attr[[1]][1])
substr(show_venue_city[1],1 + show_venue_city_attr[[1]][1],-1 + show_venue_city_attr[[1]][2])
substr(show_venue_city[1],1 + show_venue_city_attr[[1]][2], nchar(show_venue_city[[1]]))

city <- character(length(show_venue_city))
state <- character(length(show_venue_city))
country <- character(length(show_venue_city))
for (i in (1:length(show_venue_city))) {
  if(show_venue_city_dim[i] == 2) {
    ## 3 components: city/state/country
    city[i] <- substr(show_venue_city[i],1,-1 + show_venue_city_attr[[i]][1])
    state[i] <- substr(show_venue_city[i],1 + show_venue_city_attr[[i]][1],-1 + show_venue_city_attr[[i]][2])
    country[i] <- substr(show_venue_city[i],1 + show_venue_city_attr[[i]][2], nchar(show_venue_city[[i]]))
  }
  else {
    ## 2 components
    city[i] <- substr(show_venue_city[i],1,-1 + show_venue_city_attr[[i]][1])
    country[i] <- substr(show_venue_city[i],1 + show_venue_city_attr[[i]][1], nchar(show_venue_city[[i]]))
  }
}

#Date of the Show
#.date-numbers p
m_page %>% html_nodes(".date-numbers") %>% html_text()
m_page %>% html_nodes(".date-numbers") %>% html_text() %>% .[2]
m_page %>% html_nodes(".date-numbers") %>% html_text() %>% .[2] %>% gsub("\n","")
gsub("\n","", m_page %>% html_nodes(".date-numbers") %>% html_text() %>% .[2])
as.Date(gsub("\n","", m_page %>% html_nodes(".date-numbers") %>% html_text() %>% .[2]), "%b %d, %Y")
as.Date("Nov 10, 2023", "%b %d, %Y")
format(Sys.Date(), format = "%b %d, %Y")
show_date <- as.Date(gsub("\n","", m_page %>% html_nodes(".date-numbers") %>% html_text()), "%b %d, %Y")

#web page of the show
#.past-show-item
m_page %>% html_nodes(".past-show-item") %>% html_attr("href")
show_weblink <- m_page %>% html_nodes(".past-show-item") %>% html_attr("href")
m_page %>% html_nodes(".past-show-item") %>% html_attr("title")
show_title <- m_page %>% html_nodes(".past-show-item") %>% html_attr("title")

#current page
m_page %>% html_nodes(".current-page") %>% html_text()
m_page %>% html_nodes(".current-page") %>% html_attr("title")
as.numeric(gsub("\n","", m_page %>% html_nodes(".current-page") %>% html_text() %>% .[1]))

#Next page
m_page %>% html_nodes(".page-next") %>% html_text()
m_page %>% html_nodes(".page-next") %>% html_attr("href")
m_page %>% html_nodes(".page-next") %>% html_attr("href") %>% paste("https://www.metallica.com", ., sep="") %>% .[1]

#Hidden counter
m_page %>% html_nodes(".js-counter") %>% html_text()
gsub("\n","", m_page %>% html_nodes(".js-counter") %>% html_text()) #"2,219 tours for this year or years"

#Build dataset
m_show_df<-data.frame()
m_show_df <- data.frame(show_ID, show_title, show_date, show_venue_city, city, state, country, venue, show_weblink, stringsAsFactors = FALSE)

###################################
#Geocode testing
#Add city geolocalization
library(dplyr)
library(tidygeocoder)

address_components <- data.frame(
  city=c('Glens Falls', 'San Juan', 'Manila', 'Chek Lap Kok', 'South Shetland Islands', 'New-York', 'Montreal', 'Paris', 'London', 'Glasgow', 'London', 'Glasgow', 'London', 'Glasgow'),
  state=c('NY', '', '', '', '', 'NY', 'QC', '', '', '', '', '', '', ''),
  country=c('United States', '', '', '', '', 'United States', 'Canada', 'France', 'England', 'Scotland', 'United Kingdom', 'United Kingdom', '', ''),
  stringsAsFactors=FALSE)

#Moscow,Russia
#Saint Petersburg,Russia
#St. Petersburg,Russia


address_components <- data.frame(
  city=c('Moscow', 'Moscow', 'Saint Petersburg', 'St. Petersburg', 'South Shetland Islands'),
  state=c('ID', '', '', '', ''),
  country=c('United States', 'Russia', 'Russia', 'Russia', 'Antarctica'),
  stringsAsFactors=FALSE)

lat_longs <- address_components[5,] %>%
  geocode(city = city, state = state, country=country, method = "osm")
lat_longs <- address_components[5,] %>%
  geocode(city = city, method = "osm")

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
  geocode(city = city, state = state, country=country, method = "osm")

met_city_lat_long <- rbind(met_city_lat_long_part1,met_city_lat_long_part2)

#save metallica city show geolocalization
saveRDS(met_city_lat_long, file="./data/met_city_lat_long_20231124.Rda")
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20231124.Rda")

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
  group_by(city, state, country) %>% 
  summarise(n_shows=n(), min_date=min(show_date), max_date=max(show_date))

met_shows_city_stats <- left_join(met_shows_city_stats, met_city_lat_long, by = c('city', 'state', 'country'))

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
