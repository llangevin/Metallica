

function(input, output) {
  
  # Filter data based on selections
  output$Table <- DT::renderDataTable({
    #data <- met_shows_city_stats %>%
      #select(city_num, city, state, country, continent, n_shows) %>%
      #arrange(city_num, city, state, country, continent)
    if (input$continent != "All") {
      data <- data[data$continent == input$continent,]
    }
    if (input$country != "All") {
      data <- data[data$country == input$country,]
    }
    if (input$state != "All") {
      data <- data[data$state == input$state,]
    }
    if (input$city != "All") {
      data <- data[data$city == input$city,]
    }
    DT::datatable(data, selection = "single",options=list(stateSave = TRUE))})
  
  #  output$Table <- renderDataTable({
  #    DT::datatable(data, selection = "single",options=list(stateSave = TRUE))
  #  })
  
  output$Map <- leaflet::renderLeaflet({
    data_map <- leaflet::leaflet(met_shows_city_stats) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lng=~long,
        lat=~lat,
        layerId = ~city_num,
        radius = ~sqrt(n_shows),
        color = "purple",
        stroke = FALSE,
        fillOpacity = 0.5,
        popup = paste(paste('<b>City:</b>',met_shows_city_stats$city),
                      paste('<b>Nb of Shows:</b>',met_shows_city_stats$n_shows),
                      paste('<b>First Show:</b>',met_shows_city_stats$min_date),
                      paste('<b>Last Show:</b>',met_shows_city_stats$max_date),
                      sep = '<br/>')) %>%
      addMiniMap(width = 150, height = 150)
    data_map
  })
  
  shiny::observeEvent(input$Map_marker_click, {
    clickId <- input$Map_marker_click$id
    DT::dataTableProxy("Table") %>%
      DT::selectRows(which(data$city_num == clickId)) %>%
      DT::selectPage(which(input$Table_rows_all == clickId) %/% input$Table_state$length + 1)
  })
  
}