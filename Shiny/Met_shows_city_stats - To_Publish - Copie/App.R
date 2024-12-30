library(tidyverse)
library(shiny)
library(leaflet)
library(DT)

met_shows_city_stats <- readRDS(file="data/met_shows_city_stats_20241215.Rda")
met_shows <- readRDS(file="data/met_shows_20241215.Rda")

server <- function(input, output) {
  output$Table <- DT::renderDataTable({
    data <- met_shows
    data$show_weblink <- paste0("<a href='",data$show_weblink,"' target='_blank'>",data$show_ID,"</a>")
    data <- data %>%
      select(-show_ID)
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
    DT::datatable(data, escape = FALSE)
    })
  output$City_Summary <- DT::renderDataTable({
    data <- met_shows_city_stats %>%
      select(city, state, country, continent, lat, long, n_shows, min_date, max_date, city_id)
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
    DT::datatable(data)
  })
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  output$map <- leaflet::renderLeaflet({
    #data_map <- leaflet::leaflet(met_shows_city_stats) %>%
    leaflet() %>% 
      leaflet::addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      leaflet::addCircleMarkers(
        data = met_shows_city_stats,
        lng=~long,
        lat=~lat,
        layerId = met_shows_city_stats$city_id,
        radius = ~sqrt(n_shows)*2,
        color = "purple",
        stroke = FALSE,
        fillOpacity = 0.5,
        popup = ~ popup_info) %>%
      addMiniMap(width = 150, height = 150)
    #data_map
  })
  
  observeEvent(input$map_marker_click, {
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  output$table_map <- renderTable({
    if (is.null(data_of_click$clickedMarker)) {
      return(NULL)
    }
    else {
      met_shows_data <- subset(met_shows %>%
               dplyr::select(city_id, show_ID, show_date, city, state, country, continent, venue, tour_value, Other_acts_value, show_number), 
             city_id == data_of_click$clickedMarker$id)
      met_shows_data$show_date <- format(met_shows_data$show_date,'%Y-%m-%d')
      met_shows_data$show_number <- format(met_shows_data$show_number,digits =NULL)
    return(
      met_shows_data
      )
    }
  })
  
}

ui <- fluidPage( titlePanel(h3("Metallica Shows History")),
                 #titlePanel(h6("Using info from https://www.metallica.com/tour/past/ page")),
  navbarPage(a(href="https://www.metallica.com/", target='_blank', "Metallica.com"),
             tabPanel("City/Shows Map",
                      returnValue("Click on Map City/dot to see City/Shows Summary in Popup and the List of all Metallica Shows in the City (below Map)"),
                      leafletOutput("map"),
                      tableOutput("table_map"),
                      hr()
             ),
             tabPanel("City/Shows Summary",
                      fluidRow(
                        column(3,
                               selectInput("continent",
                                           "Continent:",
                                           c("All",
                                             unique(as.character(met_shows$continent))))
                        ),
                        column(3,
                               conditionalPanel("input.continent",
                                                selectInput("country",
                                                            "Country:",
                                                            c("All",
                                                              unique(as.character(met_shows$country)))))
                        ),
                        column(3,
                               selectInput("state",
                                           "State:",
                                           c("All",
                                             unique(as.character(met_shows$state))))
                        ),
                        column(3,
                               selectInput("city",
                                           "City:",
                                           c("All",
                                             unique(as.character(met_shows$city))))
                        )
                      ),
                      # Create a new row for the table.
                      DT::dataTableOutput("City_Summary"),
                      hr()
             ),
             tabPanel("List/Shows Explorer",
                      fluidRow(
                        column(3,
                               selectInput("continent",
                                           "Continent:",
                                           c("All",
                                             unique(as.character(met_shows$continent))))
                        ),
                        column(3,
                               conditionalPanel("input.continent",
                                                selectInput("country",
                                                            "Country:",
                                                            c("All",
                                                              unique(as.character(met_shows$country)))))
                        ),
                        column(3,
                               selectInput("state",
                                           "State:",
                                           c("All",
                                             unique(as.character(met_shows$state))))
                        ),
                        column(3,
                               selectInput("city",
                                           "City:",
                                           c("All",
                                             unique(as.character(met_shows$city))))
                        )
                      ),
                      # Create a new row for the table.
                      DT::dataTableOutput("Table"),
                      hr()
             )
  ),
  tags$footer(
    "As of 20241215, Using info from ",
    tags$a(
      "metallica.com/tour/past/",
      target = "_blank",
      href = "https://www.metallica.com/tour/past/"
    ),
    style = "position: absolute; width: 100%; color: black; text-align: left;"
  )
)

shinyApp(ui = ui, server = server)