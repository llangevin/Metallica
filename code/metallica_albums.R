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

#Music
#Albums
#read https://www.metallica.com/releases/albums/#primary
met_albums_page <- read_html('https://www.metallica.com/releases/albums/#primary')

#Album web link
album_weblink <- paste("https://www.metallica.com", trimws(gsub("\n","", met_albums_page %>% html_nodes(".release-tile") %>% html_attr("href")) ), sep = "")

#Album release date
album_release_date <- met_albums_page %>% 
  html_nodes(".tile-release-date") %>% 
  html_text() %>% 
  str_squish() %>%                # Removes \n, tabs, and extra internal/outer spaces
  str_remove("^Released ") %>%    # Specific removal of the prefix
  mdy()                           # Automatically parses "Month Day, Year"

#album_tile_name
album_tile_name <- met_albums_page %>% 
  html_nodes(".tile-name") %>% 
  html_text() %>% 
  str_squish()

#create Albums data.frame
for (i in (1:length(album_tile_name))) {
  if(i == 1) {
    met_albums <- data.frame(album_tile_name[i], album_release_date[i], album_weblink[2*i], stringsAsFactors = FALSE)
  } else {
    met_albums <- rbind(met_albums, data.frame(album_tile_name[i], album_release_date[i], album_weblink[2*i], stringsAsFactors = FALSE))
  }
}

#Rename and reorder
met_albums <- met_albums %>%
  rename(album_tile_name = album_tile_name.i., album_release_date = album_release_date.i., album_weblink = album_weblink.2...i.) %>%  # Rename
  arrange(album_release_date)

#save the list of albums
saveRDS(met_albums, file="./data/met_albums.Rda")

#load the list of albums
met_albums <- readRDS(file="./data/met_albums.Rda")

#Tracklist songs per album
met_album_songs <- data.frame()
for (a in (1:dim(met_albums)[1])) {
  album_weblink <- met_albums[a,3]
  album_tile_name <- met_albums[a,1]
  met_album <- read_html(album_weblink)
  tracklist <- met_album %>% html_nodes(".c-setlist__song__name") %>% html_text()
  songs_weblink <- paste("https://www.metallica.com", met_album %>% html_nodes(".c-setlist__song__name") %>% html_attr("href"), sep = "")
  for (i in (1:length(tracklist))) {
    song_number <- i
    song_title <- str_squish(tracklist[i])
    song_weblink<- songs_weblink[i]
    #Build list of songs per album
    if(i == 1) {
      songs_df <- data.frame(song_title, song_number, album_tile_name, song_weblink, album_weblink, stringsAsFactors = FALSE)
    } else {
      songs_df <- rbind(songs_df, data.frame(song_title, song_number, album_tile_name, song_weblink, album_weblink, stringsAsFactors = FALSE))
    }
  }
  met_album_songs <- rbind(met_album_songs,songs_df)
}

#save the list of albums/songs
saveRDS(met_album_songs, file="./data/met_album_songs.Rda")

#load the list of albums/songs
met_album_songs <- readRDS(file="./data/met_album_songs.Rda")

#Musicians per album
met_album_musicians <- data.frame()
for (a in (1:dim(met_albums)[1])) {
  album_weblink <- met_albums[a,3]
  album_tile_name <- met_albums[a,1]
  met_album <- read_html(album_weblink)
  musician_title <- met_album %>% html_nodes(".c-amp-details__info-dl__title") %>% html_text() %>% str_squish() %>% str_remove(":")
  musician_value <- met_album %>% html_nodes(".c-amp-details__info-dl__value") %>% html_text() %>% str_squish()
  musician_title2 <- c()
  musician_value2 <- c()
  #loop each element and separate the ones with more than one musician title into single
  for (i in (1:length(musician_title))) {
    if(gregexpr(",", musician_title)[[i]][1] == -1) {
      musician_title2 <- c(musician_title2,musician_title[i])
      musician_value2 <- c(musician_value2,musician_value[i])
    } else {
      musician_title2 <- c(musician_title2,str_to_title(str_trim(unlist(strsplit(musician_title[i], ",")))))
      for (j in (1:(1+length(gregexpr(",", musician_title[i])[[1]])))) {
        #print(j)
        musician_value2 <- c(musician_value2,musician_value[i])
      }
    }
  }  
  for (m in (1:length(musician_title2))) {
    musician_role <- musician_title2[m]
    musician_name <- musician_value2[m]
    #Build list of musicians per album
    if(m == 1) {
      musicians_df <- data.frame(musician_role, musician_name, album_tile_name, album_weblink, stringsAsFactors = FALSE)
    } else {
      musicians_df <- rbind(musicians_df, data.frame(musician_role, musician_name, album_tile_name, album_weblink, stringsAsFactors = FALSE))
    }
  }
  met_album_musicians <- rbind(met_album_musicians,musicians_df)
}

#save the list of albums/songs
saveRDS(met_album_musicians, file="./data/met_album_musicians.Rda")

#load the list of albums/songs
met_album_musicians <- readRDS(file="./data/met_album_musicians.Rda")

#Song writers
#remove wrong link
met_album_songs_w <- met_album_songs %>%
  select(song_title, song_weblink, album_weblink) %>%
  filter(!(song_weblink %in% c('https://www.metallica.comnull')))

met_song_writers <- data.frame()
for (s in (1:dim(met_album_songs_w[1])[1])) {
  song_title <- met_album_songs_w[s,1]
  song_weblink <- met_album_songs_w[s,2]
  album_weblink <- met_album_songs_w[s,3]
  met_album_song <- read_html(song_weblink)
  song_banner <- met_album_song %>% html_nodes(".c-banner-heading") %>% html_text()
  writers <- met_album_song %>% html_nodes(".c-song-detail__writtenby__list-item") %>% html_text()
  #Build list of writers per song
  for (w in (1:length(writers))) {
    writer <- writers[w]
    if(w == 1) {
      writers_df <- data.frame(song_banner, song_weblink, album_weblink, writer, stringsAsFactors = FALSE)
    } else {
      writers_df <- rbind(writers_df, data.frame(song_banner, song_weblink, album_weblink, writer, stringsAsFactors = FALSE))
    }    
  }
  met_song_writers <- rbind(met_song_writers,writers_df)
}

#save the list of song/writers
saveRDS(met_song_writers, file="./data/met_song_writers.Rda")

#load the list of song/writers
met_song_writers <- readRDS(file="./data/met_song_writers.Rda")

#Song lyrics
#remove wrong link
met_album_songs_w <- met_album_songs %>%
  select(song_title, song_weblink, album_weblink) %>%
  filter(!(song_weblink %in% c('https://www.metallica.comnull')))

met_song_lyrics <- data.frame()
for (s in (1:dim(met_album_songs_w[1])[1])) {
  song_weblink <- met_album_songs_w[s,2]
  album_weblink <- met_album_songs_w[s,3]
  met_album_song <- read_html(song_weblink)
  song_banner <- met_album_song %>% html_nodes(".c-banner-heading") %>% html_text()
  #manage copyrights
  lyrics_copyright <- met_album_song %>% html_nodes(".c-song-detail__lyrics__content") %>% html_text()
  parts <- str_split_fixed(lyrics_copyright, "© ", n = 2)
  song_lyrics <- parts[, 1]
  song_copyright <- ""
  if (str_split_fixed(parts[, 2], "\n", n = 2)[, 1] != "") {
    song_copyright <- paste("© ", str_split_fixed(parts[, 2], "\n", n = 2)[, 1], sep = "")
  }
  #manage special notes
  patterns <- "Originally released by|[“\"]Last Caress[”\"]|Released by Thin|Includes excerpts"
  parts2 <- str_split_fixed(song_lyrics, paste0("(?=", patterns, ")"), n = 2)
  song_lyrics <- str_trim(parts2[, 1]) # Part before the pattern
  song_notes <- str_trim(parts2[, 2]) # Part starting with the pattern
  #Build list of lyrics, copyright per song
  met_song_lyrics <- rbind(met_song_lyrics, data.frame(song_banner, song_weblink, album_weblink, song_lyrics, song_copyright, song_notes, stringsAsFactors = FALSE))
}

#save the list of song/writers
saveRDS(met_song_lyrics, file="./data/met_song_lyrics.Rda")

#load the list of song/writers
met_song_lyrics <- readRDS(file="./data/met_song_lyrics.Rda")
