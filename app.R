library(maps)
library(leaflet)
library(dplyr)
library(shiny)
library(rgeoboundaries)
library(rnaturalearth)
library(sf)

######### Get the map of Saudi Arabia ##############
map <- ne_states(country = "Saudi Arabia", returnclass = "sf")
map$name[5] <- "Asir"
map <- st_set_crs(map, 4326)
map <- map[order(map$name),]
plot(map)

######### Get the data of the diseases in Saudi Arabia ##############
data <- read.csv("data/Diseases.csv", header = TRUE)
data <- data[order(data$name),]
head(data)

######### Add the data to the map ##############
map <- map %>%
  left_join(data, by = "name")

######### UI ##############
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-panel-custom {
        color: gray;
        font-size: 24px;
        font-weight: bold;
      }
    "))
  ),
  titlePanel(
    title = div("Visualization of Diseases in Saudi Arabia", class = "title-panel-custom")
  ),
  sidebarLayout(
    sidebarPanel(
      tags$div(class = "sidebar-label-custom",
               selectInput("disease", "Select Disease:", 
                           choices = c("Cardiovascular", "Cancer", "Diabetes", "Hypertension", "Population"),
                           selected = "Cancer")
      )
    ),
    mainPanel(
      leafletOutput("mymap")
    )
  )
)

######### SERVER ##############
server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    disease_col <- input$disease
    pal <- colorNumeric(palette = "YlOrRd", domain = map[[disease_col]])
    
    leaflet(map) %>% 
      addTiles() %>% 
      addPolygons(
        color = "grey", 
        weight = 1, 
        fillColor = ~pal(map[[disease_col]]), 
        fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste(name, disease_col, ":", map[[disease_col]])
      ) %>%
      addLegend(
        pal = pal, 
        values = map[[disease_col]], 
        opacity = 0.5, 
        title = disease_col, 
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)
