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

#load the list of show
met_shows <- readRDS(file="./data/met_shows_20241115.Rda")
#met_shows is generated in metallica_shows.R script R script
#that read the Metallica Shows information from https://www.metallica.com/tour/past/
#met_shows$show_weblink is used to read each show and its songs list.

#remove cancelled shows
met_shows <- met_shows %>% filter(show_cancelled == 0)

#remove National anthem performance
met_shows <- met_shows %>% filter(show_ID != "2017-08-07-san-francisco-california")

#Build dataset
met_show_info<-data.frame()
met_show_songs<-data.frame()

tour_value <- character(2)
Other_acts_value <- character(2)
song_setlist <- numeric(1)
for (i in (1:dim(met_shows)[1])) {
#for (i in (1:2)) {
  show_ID <- met_shows$show_ID[i]
  show_date <- met_shows$show_date[i]
  show_weblink <- met_shows$show_weblink[i]
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
  if (length(song) > 0) {song_setlist <- 1}
  else {song_setlist <- 0}
  #Append page shows to main list of shows
  if(i == 1) {
    met_show_info <- data.frame(show_ID, show_date, show_weblink, tour_value, Other_acts_value, song_setlist, stringsAsFactors = FALSE)
    if (length(song) > 0) {
      met_show_songs <- data.frame(show_weblink, song, song_link, stringsAsFactors = FALSE)
    }
  } else {
    met_show_info <- rbind(met_show_info, data.frame(show_ID, show_date, show_weblink, tour_value, Other_acts_value, song_setlist, stringsAsFactors = FALSE))
    if (length(song) > 0) {
      met_show_songs <- rbind(met_show_songs, data.frame(show_weblink, song, song_link, stringsAsFactors = FALSE))
    }
  }
}

#Checks
table(met_show_info$tour_value)
table(met_show_info$Other_acts_value)
table(met_show_info$song_setlist)
table(met_show_songs$song)

#save the list of show info Tour and Other Act
saveRDS(met_show_info, file="./data/met_show_info_20241116.Rda")
met_show_info <- readRDS(file="./data/met_show_info_20241116.Rda")
#met_show_info could have be added to met_shows

met_show_songs <- met_show_songs %>% group_by(show_weblink) %>% mutate(song_number=row_number())
#save the list of show setlits
saveRDS(met_show_songs, file="./data/met_show_songs_20241116.Rda")
met_show_songs <- readRDS(file="./data/met_show_songs_20241116.Rda")
