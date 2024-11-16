Sys.getlocale()
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.getenv()
Sys.setenv(LANG = "en")
sessionInfo()

# clear workspace
rm(list = ls())

#Set Working Directory to Project Directory
setwd("~/Projects/Metallica")
#getwd()

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
  city=c('Warsaw','Glens Falls', 'San Juan', 'Manila', 'Chek Lap Kok', 'South Shetland Islands', 'New-York', 'Montreal', 'Paris', 'London', 'Glasgow', 'London', 'Glasgow', 'London', 'Glasgow'),
  state=c('','NY', '', '', '', '', 'NY', 'QC', '', '', '', '', '', '', ''),
  country=c('Poland','United States', '', '', '', '', 'United States', 'Canada', 'France', 'England', 'Scotland', 'United Kingdom', 'United Kingdom', '', ''),
  stringsAsFactors=FALSE)

#Moscow,Russia
#Saint Petersburg,Russia
#St. Petersburg,Russia


address_components <- data.frame(
  city=c('Moscow', 'Moscow', 'Saint Petersburg', 'St. Petersburg', 'South Shetland Islands'),
  state=c('ID', '', '', '', ''),
  country=c('United States', 'Russia', 'Russia', 'Russia', 'Antarctica'),
  stringsAsFactors=FALSE)

lat_longs <- address_components[1,] %>%
  geocode(city = city, state = state, country=country, method = "osm")
lat_longs <- address_components[1,] %>%
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

#Other information and data available in :https://www.metallica.com/tour/past/
#read https://www.metallica.com/tour/past/?pg=1
m_page <- read_html('https://www.metallica.com/tour/past/?pg=1')

#all continents
#.js-option-continent
m_page %>% html_nodes(".js-option-continent") %>% html_text()
m_page %>% html_nodes(".js-option-continent") %>% html_attr("data-continent")
m_page %>% html_nodes(".js-option-continent") %>% html_attr("value")

#all states
#.js-option-states
m_page %>% html_nodes(".js-option-states") %>% html_text()
m_page %>% html_nodes(".js-option-states") %>% html_attr("value")
m_page %>% html_nodes(".js-option-states") %>% html_attr("data-state")
m_page %>% html_nodes(".js-option-states") %>% html_attr("data-continent")
m_page %>% html_nodes(".js-option-states") %>% html_attr("data-country")

#all countries
#.js-option-country
m_page %>% html_nodes(".js-option-country") %>% html_text()
m_page %>% html_nodes(".js-option-country") %>% html_attr("value")
m_page %>% html_nodes(".js-option-country") %>% html_attr("data-country")
m_page %>% html_nodes(".js-option-country") %>% html_attr("data-continent")

m_page <- read_html('https://www.metallica.com/tour/past/?pg=1')
country_continent <- data.frame(m_page %>% html_nodes(".js-option-country") %>% html_attr("data-country"), m_page %>% html_nodes(".js-option-country") %>% html_attr("data-continent"))
names(country_continent)[1] <- "country"
names(country_continent)[2] <- "continent"
met_shows <- left_join(met_shows, country_continent, by = c('country'))

#all cities
#.js-option-city

#all tours
#.js-option-tour
m_page %>% html_nodes(".js-option-tour") %>% html_text()

#all venues
#.js-option-venue

#all albums
#.js-album-option
m_page %>% html_nodes(".js-album-option") %>% html_text()
m_page %>% html_nodes(".js-album-option") %>% html_attr("value")
m_page %>% html_nodes(".js-album-option") %>% html_attr("data-background")
m_page %>% html_nodes(".js-album-option") %>% html_attr("data-albumdeliverykey")

#all songs
#.js-song-option
m_page %>% html_nodes(".js-song-option") %>% html_text()
m_page %>% html_nodes(".js-song-option") %>% html_attr("data-albumdeliverykey")

############################################################
#cancelled shows
m_show_page <- read_html('https://www.metallica.com/tour/2022-06-29-frauenfeld-switzerland.html')
m_show_page %>% html_nodes(".c-banner-buttons-wrap") %>% html_text()
gsub("\n","", m_show_page %>% html_nodes(".c-banner-buttons-wrap") %>% html_text())
#not cancelled show
m_show_page <- read_html('https://www.metallica.com/tour/2024-09-29-mexico-city-mexico.html')
m_show_page %>% html_nodes(".c-banner-buttons-wrap") %>% html_text()
gsub("\n","", m_show_page %>% html_nodes(".c-banner-buttons-wrap") %>% html_text())

check_show_cancelled <- gsub("\n","", m_show_page %>% html_nodes(".c-banner-buttons-wrap") %>% html_text())
if(length(check_show_cancelled) >0 ) {
  if(check_show_cancelled == "Cancelled") {
  Cancelled <- 1
  }
} else {
  Cancelled <- 0
}

#load the list of show
met_shows <- readRDS(file="./data/met_shows_20241017.Rda")

#Build dataset
met_cancelled_shows<-data.frame()

for (i in (1:dim(met_shows)[1])) {
  #for (i in (1:2)) {
  show_ID <- met_shows$show_ID[i]
  show_date <- met_shows$show_date[i]
  show_weblink <- met_shows$show_weblink[i]
  print(paste("show_weblink: ", show_weblink))
  m_show_page <- read_html(show_weblink)
  check_show_cancelled <- gsub("\n","", m_show_page %>% html_nodes(".c-banner-buttons-wrap") %>% html_text())
  if(length(check_show_cancelled) >0 ) {
    if(check_show_cancelled == "Cancelled") {
      show_cancelled <- 1
    }
  } else {
    show_cancelled <- 0
  }

  #Append page shows to main list of shows
  if(i == 1) {
    met_cancelled_shows <- data.frame(show_weblink, show_cancelled, stringsAsFactors = FALSE)
  } else {
    met_cancelled_shows <- rbind(met_cancelled_shows, data.frame(show_weblink, show_cancelled, stringsAsFactors = FALSE))
  }
}

#save the list of cancelled show
saveRDS(met_cancelled_shows, file="./data/met_cancelled_shows_20241115.Rda")
table(met_cancelled_shows$show_cancelled)

met_shows <- met_shows %>% select(show_ID:show_year)
met_shows <- dplyr::inner_join(met_shows, met_cancelled_shows, by = "show_weblink")

############################################################
#read https://www.metallica.com/tour/2024-09-29-mexico-city-mexico.html
m_show_page <- read_html('https://www.metallica.com/tour/2024-09-29-mexico-city-mexico.html')

#read https://www.metallica.com/tour/2022-06-29-frauenfeld-switzerland.html
m_show_page <- read_html('https://www.metallica.com/tour/2022-06-29-frauenfeld-switzerland.html')

#Info
m_show_page %>% html_nodes(".event-header__date") %>% html_text()
m_show_page %>% html_nodes(".c-banner-heading") %>% html_text()
m_show_page %>% html_nodes(".mobile") %>% html_text()
m_show_page %>% html_nodes(".desktop") %>% html_text()
m_show_page %>% html_nodes(".event-header-eventName") %>% html_text()

#Tour
#Other Acts
m_show_page %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text()
m_show_page %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text() %>% .[1]
m_show_page %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text() %>% .[2]
Other_acts_value <- m_show_page %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text() %>% .[2]
Other_acts_value <- gsub("\\s+", " ", trimws(gsub("\n","", Other_acts_value)))

m_show_page %>% html_nodes(".c-amp-details__info-dl__title") %>% html_text()
m_show_page %>% html_nodes(".c-amp-details__info-dl__title") %>% html_text() %>% .[1]
trimws("                                M72 World Tour                            ")

#Songs
m_show_page %>% html_nodes(".c-setlist__song__inner") %>% html_text()
m_show_page %>% html_nodes(".c-setlist__song__name") %>% html_text()
m_show_page %>% html_nodes(".c-setlist__song__name") %>% html_attr("href")
m_show_page %>% html_nodes(".c-setlist__song__name") %>% html_attr("a")
m_show_page %>% html_nodes(".c-setlist") %>% html_text()
m_show_page %>% html_nodes(".c-setlist__song") %>% html_text()

#load the list of show
met_shows <- readRDS(file="./data/met_shows_20241017.Rda")
dim(met_shows)
met_shows$show_weblink[1]

#Build dataset
met_show_info<-data.frame()
met_show_songs<-data.frame()

#header_date <- character(dim(met_shows)[1])
header_date <- character(2)
banner_heading <- character(2)
banner_heading_desktop <- character(2)
banner_heading_mobile <- character(2)
tour_title <- character(2)
tour_value <- character(2)
Other_acts_title <- character(2)
Other_acts_value <- character(2)
#for (i in (1:dim(met_shows)[1])) {
for (i in (1:2)) {
  show_ID <- met_shows$show_ID[i]
  show_date <- met_shows$show_date[i]
  show_weblink <- met_shows$show_weblink[i]
  m_show_page <- read_html(show_weblink)
  #gsub("\n","", m_page %>% html_nodes(".js-counter") %>% html_text())
  #header_date <- gsub("\n","", m_show_page %>% html_nodes(".event-header__date") %>% html_text())
  #show_date <- as.Date(gsub("\n","", m_page %>% html_nodes(".event-header__date") %>% html_text()), "%b %d, %Y")
  #banner_heading <- gsub("\n","", m_show_page %>% html_nodes(".c-banner-heading") %>% html_text())
  #banner_heading_desktop <- gsub(",","", m_show_page %>% html_nodes(".desktop") %>% html_text())
  #banner_heading_mobile <- gsub(",","", m_show_page %>% html_nodes(".mobile") %>% html_text())
  #tour_title <- gsub("\n","", m_show_page %>% html_nodes(".c-amp-details__info-dl__title") %>% html_text() %>% .[1])
  tour_value <- trimws(gsub("\n","", m_show_page %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text() %>% .[1]) )
  #Other_acts_title <- gsub("\n","", m_show_page %>% html_nodes(".c-amp-details__info-dl__title") %>% html_text() %>% .[2])
  Other_acts_value <- m_show_page %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text() %>% .[2]
  Other_acts_value <- gsub("\\s+", " ", trimws(gsub("\n","", Other_acts_value)))
  
  song <- trimws(gsub("\n","", m_show_page %>% html_nodes(".c-setlist__song__name") %>% html_text()))
  song_link <- m_show_page %>% html_nodes(".c-setlist__song__name") %>% html_attr("href")
  song_link <- paste("https://www.metallica.com", song_link, sep = "")
  #Append page shows to main list of shows
  if(i == 1) {
    met_show_info <- data.frame(show_ID, show_date, show_weblink, tour_value, Other_acts_value, stringsAsFactors = FALSE)
    met_show_songs <- data.frame(show_weblink, song, song_link, stringsAsFactors = FALSE)
  } else {
    met_show_info <- rbind(met_show_info, data.frame(show_ID, show_date, show_weblink, tour_value, Other_acts_value, stringsAsFactors = FALSE))
    met_show_songs <- rbind(met_show_songs, data.frame(show_weblink, song, song_link, stringsAsFactors = FALSE))
  }
}

met_show_songs %>% group_by(show_weblink) %>% mutate(Song_Number=row_number())
paste("https://www.metallica.com/", "/songs/creeping-death.html", sep = "")
#https://www.metallica.com/songs/whiplash.html
#https://www.metallica.com//songs/creeping-death.html

#Build dataset
met_show_info<-data.frame()
met_show_info <- data.frame(header_date, banner_heading, banner_heading_desktop, banner_heading_mobile, tour_title, tour_value, Other_acts_title, Other_acts_value, stringsAsFactors = FALSE)
