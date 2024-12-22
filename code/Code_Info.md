## Metallica Shit Project Scripts Info

The Code_Info.md file contains the list of scripts utilized in the project and what they are used for, what they read from <https://www.metallica.com> and outputs that they produce.

-   metallica_shows.R
    -   R script to read the Metallica Shows information from <https://www.metallica.com/tour/past/>
    -   Generate met_shows dataset which contains 1 row per Metallica show with some attributes (date, location)
    -   To update met_shows if it exists, use metallica_past_tour_date_update_shows.R
-   metallica_shows_city_geolocalization.R
    -   R script to obtain geolocalization for shows city
    -   Generate met_city_lat_long dataset which contains 1 row per city with geographic and geolocalization attributes
    -   To update met_city_lat_long if it exists, use metallica_past_tour_date_update_shows.R
    -   The script contains code to map the shows with leaflet with some stats per city
-   metallica_shows_info_songs.R
    -   Read additional info from each show web page like Other Acts and if there is a songs list
    -   Information is stored in met_show_info dataset
    -   If there is a songs list it is read and each played song is stored in met_show_songs dataset with some attributes
-   metallica_shows_update.R
    -   R script to update the Metallica Shows information from <https://www.metallica.com/tour/past/>
    -   Generate metus_shows (Metallica Update Shows) dataset which contains 1 row per Metallica show with some attributes (date, location)
    -   metus_shows is the list of new shows not in met_shows generated from metallica_shows.R
    -   Also update and add geolocalization information of new city shows to met_city_lat_long
-   metallica_shows_Mapping_Stats_Visualizations.R
    -   Test the leaflet package to visualize Metallica shows around the world
    -   Contains Stats & Visualization Plots, Distribution of Shows per year
-   metallica_past_tour_date_Exploration.R
    -   Test different packages and functions
    -   Test rvest to read the metallica.com site
    -   Test tidygeocoder to geolocalization to cities for mapping
    -   Dev and tests of code to build the list of shows dataset
