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
