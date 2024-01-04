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

#dataset fram
met_shows<-data.frame()

#parameters
read_next <- TRUE

#1st page to read
m_page <- read_html('https://www.metallica.com/tour/past/?pg=1')

#function to read list of metallica shows
read_met_shows <- function() {
  while(read_next == TRUE) {
    
    #current page
    pg <- as.numeric(gsub("\n","", m_page %>% html_nodes(".current-page") %>% html_text() %>% .[1]))
    print(paste("Reading Page: ", pg))
    
    #ID of the show
    show_ID <- m_page %>% html_nodes(".show") %>% html_attr("data-show-id")
    
    #web page of the show
    show_weblink <- m_page %>% html_nodes(".past-show-item") %>% html_attr("href")
    show_title <- m_page %>% html_nodes(".past-show-item") %>% html_attr("title")
    
    #Date of the Show
    show_date <- as.Date(gsub("\n","", m_page %>% html_nodes(".date-numbers") %>% html_text()), "%b %d, %Y")

    #Add Year of the show
    show_year<- as.numeric(format(show_date,"%Y"))
    
    #.venue-city p
    show_venue_city <- gsub("\n@\n"," @ ", m_page %>% html_nodes(".venue-city") %>% html_text())
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
    venue <- gsub("\n@\n"," @ ", m_page %>% html_nodes(".venue-name") %>% html_text())
    venue <- gsub("\n","", venue)

    #Append page shows to main list of shows
    if(pg == 1) {
      met_shows <- data.frame(show_ID, show_title, show_date, show_venue_city, city, state, country, venue, show_weblink, show_year, stringsAsFactors = FALSE)
    } else {
      met_shows <- rbind(met_shows, data.frame(show_ID, show_title, show_date, show_venue_city, city, state, country, venue, show_weblink, show_year, stringsAsFactors = FALSE))
    }
    
    #Next page to read if any
    if(min(show_date) > "1982-03-14") {
      m_page <- read_html(m_page %>% html_nodes(".page-next") %>% html_attr("href") %>% paste("https://www.metallica.com", ., sep="") %>% .[1])
    } else {
      #Stop reading
      read_next <- FALSE
      Hidden_counter <- gsub("\n","", m_page %>% html_nodes(".js-counter") %>% html_text()) #"2,219 tours for this year or years"
      Hidden_counter <- gsub(",","", Hidden_counter) #"2219 tours for this year or years"
      Hidden_counter <- as.numeric(substr(Hidden_counter,1,-1 + gregexpr(" ",Hidden_counter, fixed = TRUE)[[1]][1]))
    }
  }
  assign("met_shows", met_shows, envir=.GlobalEnv)
  assign("Hidden_counter", Hidden_counter, envir=.GlobalEnv)
  }
read_met_shows()

#Corrections
#met_shows$state[met_shows$city == "Victoriaville"] <- "QC"
met_shows$state[met_shows$show_ID == "1986-12-07-victoriaville-canada"] <- "QC"
#met_shows$city[met_shows$city == "Glenn Falls"] <- "Glens Falls"
met_shows$city[met_shows$show_ID == "1986-04-27-glenn-falls-new-york"] <- "Glens Falls"

#Add the show number
met_shows$show_number <- nrow(met_shows) - as.numeric(rownames(met_shows)) +1

#Add the country/continent
m_page <- read_html('https://www.metallica.com/tour/past/?pg=1')
country_continent <- data.frame(m_page %>% html_nodes(".js-option-country") %>% html_attr("data-country"), m_page %>% html_nodes(".js-option-country") %>% html_attr("data-continent"))
names(country_continent)[1] <- "country"
names(country_continent)[2] <- "continent"
met_shows <- left_join(met_shows, country_continent, by = c('country'))

#save the list of show
saveRDS(met_shows, file="./data/met_shows_20231120.Rda")
met_shows <- readRDS(file="./data/met_shows_20231120.Rda")

#1 Add Show_Year
#2 Add Show_Number
#3 Add city geolocalization
#4 Some stats
#5 Build update data show
#6 Think about Show_Info and shiny