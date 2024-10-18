library(dplyr)

#city geolocalization
met_city_lat_long <- readRDS(file="data/met_city_lat_long_20231124.Rda")

#city stats
met_shows_city_stats <- readRDS(file="data/met_shows_20231120.Rda") %>%
  group_by(city, state, country, continent) %>% 
  summarise(n_shows=n(), min_date=min(show_date), max_date=max(show_date))

met_shows_city_stats <- left_join(met_shows_city_stats, met_city_lat_long, by = c('city', 'state', 'country', 'continent'))
