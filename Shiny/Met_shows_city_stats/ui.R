fluidPage(
  titlePanel("Metallica Shows per City"),
  leafletOutput("Map"),
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(3,
           selectInput("continent",
                       "Continent:",
                       c("All",
                         unique(as.character(met_shows_city_stats$continent))))
    ),
    column(3,
           conditionalPanel("input.continent",
                            selectInput("country",
                                        "Country:",
                                        c("All",
                                          unique(as.character(met_shows_city_stats$country)))))
    ),
    column(3,
           selectInput("state",
                       "State:",
                       c("All",
                         unique(as.character(met_shows_city_stats$state))))
    ),
    column(3,
           selectInput("city",
                       "City:",
                       c("All",
                         unique(as.character(met_shows_city_stats$city))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("Table")
)