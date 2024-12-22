#R script to update the Metallica Shows information from https://www.metallica.com/tour/past/
#Generate new_met_shows dataset which contains 1 row per Metallica show with some attributes (date, location)
#shows that are not in met_shows generated from metallica_past_tour_date.R

#metallica_shows_update.R
#R script to update the Metallica Shows information from https://www.metallica.com/tour/past/
#Generate new_met_shows (Metallica Update Shows) dataset which contains 1 row per Metallica show with some attributes (date, location)
#new_met_shows is the list of new shows not in met_shows generated from metallica_shows.R
#Also update and add geolocalization information of new city shows to met_city_lat_long

Sys.getlocale()
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.getenv()
Sys.setenv(LANG = "en")
sessionInfo()

# clear workspace
rm(list = ls())

library(tidyverse)
library(rvest)

#set working directory
setwd("~/Projects/Metallica")

#dataset frame
#new_met_shows: Metallica update shows
new_met_shows<-data.frame()

#load Metallica list of shows to update
met_shows <- readRDS(file="./data/met_shows_20241016.Rda")
print(max(met_shows$show_date))

#parameters
read_next <- TRUE

#1st page to read
new_met_shows_page <- read_html('https://www.metallica.com/tour/past/?pg=1')

#function to read list of metallica shows
read_new_met_shows <- function() {
  while(read_next == TRUE) {
    
    #current page
    pg <- as.numeric(gsub("\n","", new_met_shows_page %>% html_nodes(".current-page") %>% html_text() %>% .[1]))
    print(paste("Reading Page: ", pg))
    
    #ID of the show
    show_ID <- new_met_shows_page %>% html_nodes(".show") %>% html_attr("data-show-id")
    
    #web page of the show
    show_weblink <- new_met_shows_page %>% html_nodes(".past-show-item") %>% html_attr("href")
    show_title <- new_met_shows_page %>% html_nodes(".past-show-item") %>% html_attr("title")
    
    #Date of the Show
    show_date <- as.Date(substr(show_title,1,10), "%Y-%m-%d")
    
    #Add Year of the show
    show_year<- as.numeric(format(show_date,"%Y"))
    
    #.venue-city p
    show_venue_city <- gsub("\n|  ","", trimws(new_met_shows_page %>% html_nodes(".venue-city") %>% html_text()))
    
    #Town/State/Country of the show
    city <- character(length(show_venue_city))
    state <- character(length(show_venue_city))
    country <- character(length(show_venue_city))
    show_venue_city_attr <- gregexpr(",",show_venue_city, fixed = TRUE)
    show_venue_city_dim <- sapply(gregexpr(",",show_venue_city, fixed = TRUE), function(y) sum(attr(y, "match.length"))) #int [1:20]
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
    
    #Place of the show
    venue <- trimws(gsub("\n|  ","", new_met_shows_page %>% html_nodes(".venue-name") %>% html_text()))
    venue <- gsub("@"," @ ", venue)
    
    #cancelled shows
    ctas <- trimws(gsub("\n","", new_met_shows_page %>% html_nodes(".ctas") %>% html_text()))
    show_cancelled <- numeric(length(ctas))
    for (i in (1:length(ctas))) {
      if (ctas[i] == "Cancelled") {
        show_cancelled[i] <- 1
      } else {
        show_cancelled[i] <- 0
      }
    }

    #Append page shows to main list of shows
    if(pg == 1) {
      new_met_shows <- data.frame(show_ID, show_title, show_date, show_venue_city, city, state, country, venue, show_weblink, show_year, show_cancelled, stringsAsFactors = FALSE)
    } else {
      new_met_shows <- rbind(new_met_shows, data.frame(show_ID, show_title, show_date, show_venue_city, city, state, country, venue, show_weblink, show_year, show_cancelled, stringsAsFactors = FALSE))
    }
    
    #Next page to read if any
    if(min(show_date) > max(met_shows$show_date)) {
      new_met_shows_page <- read_html(new_met_shows_page %>% html_nodes(".page-next") %>% html_attr("href") %>% paste("https://www.metallica.com", ., sep="") %>% .[1])
    } else {
      #Stop reading
      read_next <- FALSE
      Hidden_counter <- gsub("\n","", new_met_shows_page %>% html_nodes(".js-counter") %>% html_text()) #"2,219 tours for this year or years"
      Hidden_counter <- gsub(",","", Hidden_counter) #"2219 tours for this year or years"
      Hidden_counter <- as.numeric(substr(Hidden_counter,1,-1 + gregexpr(" ",Hidden_counter, fixed = TRUE)[[1]][1]))
    }
  }
  assign("new_met_shows", new_met_shows, envir=.GlobalEnv)
  assign("Hidden_counter", Hidden_counter, envir=.GlobalEnv)
  }
read_new_met_shows()

#Keep new shows that are not in main met_shows
new_met_shows <- new_met_shows[new_met_shows$show_date > max(met_shows$show_date),]

#Corrections
#None

#Add the show number
new_met_shows$show_number <-  nrow(met_shows) + nrow(new_met_shows) - as.numeric(rownames(new_met_shows)) +1

#Add the country/continent
new_met_shows_page <- read_html('https://www.metallica.com/tour/past/?pg=1')
country_continent <- data.frame(new_met_shows_page %>% html_nodes(".js-option-country") %>% html_attr("data-country"), new_met_shows_page %>% html_nodes(".js-option-country") %>% html_attr("data-continent"))
names(country_continent)[1] <- "country"
names(country_continent)[2] <- "continent"
country_continent[country_continent$continent == "null",]
#Corrections
country_continent$country[country_continent$country == "Phillippines"] <- "Philippines"
country_continent$continent[country_continent$country == "Philippines"] <- "Asia"
country_continent$continent[country_continent$country == "Russia"] <- "Europe"
new_met_shows <- left_join(new_met_shows, country_continent, by = c('country'))

#Append list of new shows
met_shows <- rbind(new_met_shows, met_shows, stringsAsFactors = FALSE)

#save the list of show
saveRDS(met_shows, file="./data/met_shows_20241215.Rda")
#save the list of show in CSV
met_shows_csv <- "./data/metallica_shows.csv"
if (file.exists(met_shows_csv)) {file.remove(met_shows_csv)}
write.csv(met_shows,met_shows_csv, row.names = T)

#validations and data checks
print(Hidden_counter) #Number of shows from Metallica web site
dim(met_shows)
summary(met_shows)
min(met_shows$show_date)
max(met_shows$show_date)
min(met_shows$show_year)
max(met_shows$show_year)
min(met_shows$show_number)
max(met_shows$show_number)
met_shows[is.na(met_shows$show_year),]
met_shows[is.na(met_shows$show_number),]

#checks countries and continents
table(met_shows$continent)
table(met_shows$country)
met_shows[met_shows$continent == "null",]
met_shows[met_shows$country == "null",]
met_shows[met_shows$show_ID == "null",]
met_shows[met_shows$show_title == "null",]
met_shows[met_shows$show_venue_city == "null",]
met_shows[met_shows$city == "null",]
met_shows[met_shows$show_weblink== "null",]

#load the list of show
met_shows <- readRDS(file="./data/met_shows_20241215.Rda")

#Update met_city_lat_long
#created in metallica_shows_city_geolocalization.R
#load metallica city show geolocalization
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241016.Rda")

#Update Geolocalization
#Add city geolocalization
library(tidygeocoder)

#Identify new city from the update list of shows
new_met_shows_new_cities <- anti_join(new_met_shows, met_city_lat_long, by = c("city", "state", "country"))

#Part 1
#United Kingdom cities geolocalization info obtain without using country
met_city_lat_long_part1 <- new_met_shows_new_cities %>%
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
met_city_lat_long_part2 <- new_met_shows_new_cities %>%
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
met_city_continent <-new_met_shows_new_cities %>%
  distinct(city, state, country, continent)
met_city_lat_long_new <- left_join(met_city_continent, met_city_lat_long_new, by = c('city', 'state', 'country'))
rm(met_city_continent)

#Append new shows geolocalization
met_city_lat_long <- rbind(met_city_lat_long_new, met_city_lat_long, stringsAsFactors = FALSE)
met_city_lat_long <- met_city_lat_long %>% arrange(city, state, country)

#save metallica city show geolocalization
saveRDS(met_city_lat_long, file="./data/met_city_lat_long_20241215.Rda")
#save the list of show in CSV
met_city_lat_long_csv <- "./data/metallica_city_lat_long.csv"
if (file.exists(met_city_lat_long_csv)) {file.remove(met_city_lat_long_csv)}
write.csv(met_city_lat_long,met_city_lat_long_csv, row.names = T)

#validations and data checks
dim(met_city_lat_long)
summary(met_city_lat_long)
met_city_lat_long[is.na(met_city_lat_long$lat),]
met_city_lat_long[is.na(met_city_lat_long$long),]
table(met_city_lat_long$continent)
table(met_city_lat_long$country)
met_city_lat_long[met_city_lat_long$continent == "null",]
met_city_lat_long[met_city_lat_long$country == "null",]
met_city_lat_long[met_city_lat_long$state == "null",]
met_city_lat_long[met_city_lat_long$city == "null",]

#Duplicates in city
met_city_lat_long %>% group_by(city, state, country) %>% summarise(Count=n()) %>% filter(Count > 1)

#Corrections, removing duplicates
#met_city_lat_long %>% distinct() %>% group_by(city, state, country) %>% summarise(Count=n()) %>% filter(Count > 1)
#met_city_lat_long <- met_city_lat_long %>% distinct()
#met_city_lat_long <- met_city_lat_long %>% filter(city != 'Warsaw' | long != 21.0067249)

#All cities in met_shows in met_city_lat_long
anti_join(met_shows, met_city_lat_long, by = c("city", "state", "country"))

#load metallica city show geolocalization
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241215.Rda")

#Update met_show_info and met_show_songs using new_met_shows
#created in metallica_shows_info_songs.R

#remove cancelled shows
new_met_shows <- new_met_shows %>% filter(show_cancelled == 0)

#remove National anthem performance
new_met_shows <- new_met_shows %>% filter(show_ID != "2017-08-07-san-francisco-california")

#Build dataset
met_show_info<-data.frame()
met_show_songs<-data.frame()

tour_value <- character(2)
Other_acts_value <- character(2)
song_setlist <- numeric(1)
for (i in (1:dim(new_met_shows)[1])) {
  #for (i in (1:2)) {
  show_ID <- new_met_shows$show_ID[i]
  show_date <- new_met_shows$show_date[i]
  show_weblink <- new_met_shows$show_weblink[i]
  print(paste("show_weblink: ", show_weblink))
  m_show_page <- read_html(show_weblink)
  tour_other_act_value <- m_show_page %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text()
  tour_other_act_title <- m_show_page %>% html_nodes(".c-amp-details__info-dl__title") %>% html_text()
  length(tour_other_act_value)
  if (length(tour_other_act_value) == 2) {
    tour_value <- trimws(gsub("\n","", tour_other_act_value %>% .[1]) )
    Other_acts_value <- gsub("\\s+", " ", trimws(gsub("\n","", tour_other_act_value[2])))
  } else if (length(tour_other_act_value) == 1) {
    if (trimws(gsub("\n","", tour_other_act_title)) == "Tour:") {
      tour_value <- trimws(gsub("\n","", tour_other_act_value))
      Other_acts_value <- NA
    } else {
      tour_value <- NA
      Other_acts_value <- gsub("\\s+", " ", trimws(gsub("\n","", tour_other_act_value)))
    }
  } else {
    tour_value <- NA
    Other_acts_value <- NA
  }  
  song <- trimws(gsub("\n","", m_show_page %>% html_nodes(".c-setlist__song__name") %>% html_text()))
  song_link <- m_show_page %>% html_nodes(".c-setlist__song__name") %>% html_attr("href")
  song_link <- paste("https://www.metallica.com", song_link, sep = "")
  if (length(song) > 0) {
    song_setlist <- 1
  } else {
    song_setlist <- 0
  }
  #Append page shows to main list of shows
  if(i == 1) {
    new_met_shows_info <- data.frame(show_ID, show_date, show_weblink, tour_value, Other_acts_value, song_setlist, stringsAsFactors = FALSE)
    if (length(song) > 0) {
      new_met_shows_songs <- data.frame(show_weblink, song, song_link, stringsAsFactors = FALSE)
    }
  } else {
    new_met_shows_info <- rbind(new_met_shows_info, data.frame(show_ID, show_date, show_weblink, tour_value, Other_acts_value, song_setlist, stringsAsFactors = FALSE))
    if (length(song) > 0) {
      new_met_shows_songs <- rbind(new_met_shows_songs, data.frame(show_weblink, song, song_link, stringsAsFactors = FALSE))
    }
  }
}

#Checks
table(new_met_shows_info$tour_value)
table(new_met_shows_info$Other_acts_value)
table(new_met_shows_info$song_setlist)
table(new_met_shows_songs$song)

#Append new shows info and songs
met_show_info <- readRDS(file="./data/met_show_info_20241116.Rda")
met_show_songs <- readRDS(file="./data/met_show_songs_20241116.Rda")

met_show_info <- rbind(new_met_shows_info, met_show_info, stringsAsFactors = FALSE)
new_met_shows_songs <- new_met_shows_songs %>% group_by(show_weblink) %>% mutate(song_number=row_number())
met_show_songs <- rbind(new_met_shows_songs, met_show_songs)

#Checks
dim(met_show_songs)
dim(new_met_shows_songs)
dim(met_shows)
length(unique(met_shows$show_ID))
dim(met_shows %>% filter(show_cancelled == 0) %>% filter(show_ID != "2017-08-07-san-francisco-california"))
dim(new_met_shows_info)
dim(met_show_info)

saveRDS(met_show_info, file="./data/met_show_info_20241116.Rda")
saveRDS(met_show_songs, file="./data/met_show_songs_20241116.Rda")