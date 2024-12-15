#R script to update the Metallica Shows information from https://www.metallica.com/tour/past/
#Generate metus_shows dataset which contains 1 row per Metallica show with some attributes (date, location)
#shows that are not in met_shows generated from metallica_past_tour_date.R

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
#metus_shows: Metallica update shows
metus_shows<-data.frame()

#load Metallica list of shows to update
met_shows <- readRDS(file="./data/met_shows_20241115.Rda")
print(max(met_shows$show_date))

#parameters
read_next <- TRUE

#1st page to read
mus_page <- read_html('https://www.metallica.com/tour/past/?pg=1')

#function to read list of metallica shows
read_metus_shows <- function() {
  while(read_next == TRUE) {
    
    #current page
    pg <- as.numeric(gsub("\n","", mus_page %>% html_nodes(".current-page") %>% html_text() %>% .[1]))
    print(paste("Reading Page: ", pg))
    
    #ID of the show
    show_ID <- mus_page %>% html_nodes(".show") %>% html_attr("data-show-id")
    
    #web page of the show
    show_weblink <- mus_page %>% html_nodes(".past-show-item") %>% html_attr("href")
    show_title <- mus_page %>% html_nodes(".past-show-item") %>% html_attr("title")
    
    #Date of the Show
    show_date <- as.Date(gsub("\n","", mus_page %>% html_nodes(".date-numbers") %>% html_text()), "%b %d, %Y")

    #Add Year of the show
    show_year<- as.numeric(format(show_date,"%Y"))
    
    #.venue-city p
    show_venue_city <- gsub("\n@\n"," @ ", mus_page %>% html_nodes(".venue-city") %>% html_text())
    show_venue_city <- gsub("\n","", show_venue_city)
    
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
    venue <- gsub("\n@\n"," @ ", mus_page %>% html_nodes(".venue-name") %>% html_text())
    venue <- gsub("\n","", venue)
    
    #cancelled shows
    ctas <- trimws(gsub("\n","", mus_page %>% html_nodes(".ctas") %>% html_text()))
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
      metus_shows <- data.frame(show_ID, show_title, show_date, show_venue_city, city, state, country, venue, show_weblink, show_year, show_cancelled, stringsAsFactors = FALSE)
    } else {
      metus_shows <- rbind(metus_shows, data.frame(show_ID, show_title, show_date, show_venue_city, city, state, country, venue, show_weblink, show_year, show_cancelled, stringsAsFactors = FALSE))
    }
    
    #Next page to read if any
    if(min(show_date) > max(met_shows$show_date)) {
      mus_page <- read_html(mus_page %>% html_nodes(".page-next") %>% html_attr("href") %>% paste("https://www.metallica.com", ., sep="") %>% .[1])
    } else {
      #Stop reading
      read_next <- FALSE
      Hidden_counter <- gsub("\n","", mus_page %>% html_nodes(".js-counter") %>% html_text()) #"2,219 tours for this year or years"
      Hidden_counter <- gsub(",","", Hidden_counter) #"2219 tours for this year or years"
      Hidden_counter <- as.numeric(substr(Hidden_counter,1,-1 + gregexpr(" ",Hidden_counter, fixed = TRUE)[[1]][1]))
    }
  }
  assign("metus_shows", metus_shows, envir=.GlobalEnv)
  assign("Hidden_counter", Hidden_counter, envir=.GlobalEnv)
  }
read_metus_shows()

#Keep new shows that are not in met_shows
metus_shows <- metus_shows[metus_shows$show_date > max(met_shows$show_date),]

#Corrections
#None

#Add the show number
metus_shows$show_number <-  nrow(met_shows) + nrow(metus_shows) - as.numeric(rownames(metus_shows)) +1

#Add the country/continent
mus_page <- read_html('https://www.metallica.com/tour/past/?pg=1')
country_continent <- data.frame(mus_page %>% html_nodes(".js-option-country") %>% html_attr("data-country"), mus_page %>% html_nodes(".js-option-country") %>% html_attr("data-continent"))
names(country_continent)[1] <- "country"
names(country_continent)[2] <- "continent"
country_continent[country_continent$continent == "null",]
#Corrections
country_continent$country[country_continent$country == "Phillippines"] <- "Philippines"
country_continent$continent[country_continent$country == "Philippines"] <- "Asia"
country_continent$continent[country_continent$country == "Russia"] <- "Europe"
metus_shows <- left_join(metus_shows, country_continent, by = c('country'))

#Append list of new shows
met_shows <- rbind(metus_shows, met_shows, stringsAsFactors = FALSE)

#save the list of show
saveRDS(met_shows, file="./data/met_shows_20241017.Rda")
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
met_shows <- readRDS(file="./data/met_shows_20241017.Rda")

#load metallica city show geolocalization
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241016.Rda")

#Update Geolocalization
#Add city geolocalization
library(tidygeocoder)

#Identify new city from the update list of shows
metus_shows_new_cities <- anti_join(met_shows, met_city_lat_long, by = c("city", "state", "country"))

#Part 1
#United Kingdom cities geolocalization info obtain without using country
met_city_lat_long_part1 <- metus_shows_new_cities %>%
  select(city, state, country) %>%
  filter(country %in% c('Antarctica','England','Northern Ireland','Scotland','Wales','Hong Kong','Phillippines','Puerto Rico')) %>%
  #mutate(country="") %>%
  distinct(city, state, country) %>%
  geocode(city = city, method = "osm")

#Part 2
#Using city/state/country information to get geolocalization
met_city_lat_long_part2 <- metus_shows_new_cities %>%
  select(city, state, country) %>%
  filter(!(country %in% c('Antarctica','England','Northern Ireland','Scotland','Wales','Hong Kong','Phillippines','Puerto Rico'))) %>%
  #mutate(country="") %>%
  distinct(city, state, country) %>%
  geocode(city = city, state = state, country=country, method = "osm")

met_city_lat_long_new <- rbind(met_city_lat_long_part1,met_city_lat_long_part2)
#use line below instead if Part 1 does not have cases
#met_city_lat_long_new <- met_city_lat_long_part2
rm(met_city_lat_long_part1)
rm(met_city_lat_long_part2)

#Adding Continent
met_city_continent <-metus_shows_new_cities %>%
  distinct(city, state, country, continent)
met_city_lat_long_new <- left_join(met_city_continent, met_city_lat_long_new, by = c('city', 'state', 'country'))
rm(met_city_continent)

#Append new shows geolocalization
met_city_lat_long <- rbind(met_city_lat_long_new, met_city_lat_long, stringsAsFactors = FALSE)
met_city_lat_long <- met_city_lat_long %>% arrange(city, state, country)

#save metallica city show geolocalization
saveRDS(met_city_lat_long, file="./data/met_city_lat_long_20241017.Rda")
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
met_city_lat_long <- readRDS(file="./data/met_city_lat_long_20241017.Rda")