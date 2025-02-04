library(tidyverse)
library(leaflet)

botsad.final <- read_csv("https://raw.githubusercontent.com/Janzeero-PhD/Botanical-Garden-of-NULES/master/botsad_final.csv")

server <- function(input, output) {
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data = botsad.final,
                       lng = ~ X, lat = ~ Y, radius = 3,
                       popup = paste0("<img src = ", botsad.final$link_4, " />"),
                       color="black",  fillColor="red", stroke = F, 
                       fillOpacity = 0.5,
                       label = botsad.final$species_5,
                       layerId = botsad.final$id
      )
  })
  
  # store the click
  observeEvent(input$map_marker_click, {
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Make a barplot or scatterplot depending of the selected point
  output$table <- renderTable({
    if (is.null(data_of_click$clickedMarker)) {
      return(NULL)
    }
    return(
      subset(botsad.final %>%
               dplyr::select(3, 7:12, 14), 
             id == data_of_click$clickedMarker$id
      )
    )
  })
}

ui <- fluidPage(
  br(),
  column(8, leafletOutput("map", height = "600px")),
  column(4, br(), br(), br(), br(), tableOutput("table")),
  br()
)

shinyApp(ui = ui, server = server)