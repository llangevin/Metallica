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

#remove cancelled shows
met_shows <- met_shows %>% filter(show_cancelled == 0)

#Build dataset
met_show_info<-data.frame()
met_show_songs<-data.frame()

tour_value <- character(2)
Other_acts_value <- character(2)
for (i in (1:dim(met_shows)[1])) {
#for (i in (1:2)) {
  show_ID <- met_shows$show_ID[i]
  show_date <- met_shows$show_date[i]
  show_weblink <- met_shows$show_weblink[i]
  print(paste("show_weblink: ", show_weblink))
  m_show_page <- read_html(show_weblink)
  tour_value <- trimws(gsub("\n","", m_show_page %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text() %>% .[1]) )
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
