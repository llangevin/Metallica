

function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- met_shows_city_stats
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
    data
  }))
  
}