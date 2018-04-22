## make a Shiny app using American Community Survey data.
library(shiny)
library(tidyverse)
library(tidycensus)
library(choroplethr)
library(leaflet)
library(choroplethrMaps)
library(sf)
devtools::install_github("tidyverse/ggplot2")

data(state)
census_api_key("baa39e44423b9990c45a6ccdc610481eddf2cc8a",install = TRUE,overwrite = TRUE)

readRenviron("~/.Renviron")
ui <- fluidPage(
  leafletOutput("map", height="600px"),
  absolutePanel(fixed = TRUE,
                draggable = TRUE, top = 80, left = "auto", right = 25, bottom = "auto",
                width = 350, height = "auto",
                radioButtons("featureID", "Display Feature", choices = c("Median Household Income", "Median Gross Rent", "Ratio of Both"), selected = "Median Household Income"),
                selectInput("stateID", "State:",state.abb, selected = "NY", multiple = FALSE), 
                style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;")
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    features <- switch(input$featureID,
                       `Median Gross Rent` = "B25064_001",
                       `Median Household Income` = "B19013_001",
                       `Ratio of Both` = "B25074_001")
    state <- get_acs(geography = "tract", 
                     variables = features, 
                     state = input$stateID , 
                     geometry = TRUE)
    
    pal <- colorQuantile("viridis", domain = state$estimate, n = 8)
    state %>% st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.80,
                  color = ~ pal(estimate)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~ estimate,
                title = "Range of Estimates",
                opacity = 1)
  })
}

shinyApp(ui = ui, server = server)
