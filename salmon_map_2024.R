#BC Salmon RAPSTA Recommendations Map 2024 

# Install required packages if you haven't already
install.packages("leaflet")
install.packages("dplyr")
install.packages("sf")
install.packages("httr")
install.packages("geojsonio")
install.packages("jsonlite")
install.packages("shiny")
install.packages("leaflet.extras")
install.packages("htmltools")
install.packages("htmlwidgets")
install.packages("readr")


# Load the packages
library(leaflet)
library(dplyr)
library(sf)
library(httr)
library(geojsonio)
library(jsonlite)
library(shiny)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(readr)


getwd()
setwd("/Users/samrenshaw/Desktop")

# Load fisheries management areas GeoJSON

fisheries_areas <- geojson_read("DFO_PFMA_SUBAREAS_SP.geojson", what = "sp")
fish_areas_sf <- st_as_sf(fisheries_areas)

area23 <- c(23)
area3 <- c(3)
area22<- c(22)

range1 <- setdiff(1:10,3)
areaF <- c(range1, 101:110, 130:142)

chinook_colour <- "turquoise"
chum_colour <- "yellow"
pink_colour <- "hotpink"
MultipleSpecies <- "brown"


# Filter out polygons where MANAGEMENT_AREA is desired number
filtered_data <- fish_areas_sf %>%
  mutate(color = case_when(
    MANAGEMENT_AREA %in% area23 ~ chinook_colour,
    MANAGEMENT_AREA %in% area3 ~ pink_colour,
    MANAGEMENT_AREA %in% area22 ~ chum_colour,
    TRUE ~ "grey"  # Default color for other areas
  ))

# Filter the data for the specified ranges (optional)
filtered_data_w_mapcol <- filtered_data %>% filter(MANAGEMENT_AREA %in% c(areaF, area23, area3, area22))

# Read data from CSV file
salmon_data <- read.csv("salmon_map_RAPSTA2024.csv")

species_colors <- c(
  Chinook = "turquoise",
  Coho = "lightgreen",
  Sockeye = "orange",
  Pink = "hotpink",
  Chum = "yellow"
)

# Add a colour column to the data frame based on the species_colors
salmon_data$colour <- species_colors[salmon_data$salmon_name]

# Create the interactive map with colored default markers
map <- leaflet(data = salmon_data) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(
    data = filtered_data_w_mapcol,
    color = ~color,
    weight = 1,
    opacity = 0.5,
    fillOpacity = 0.2,
    popup = ~paste("Management Area:", MANAGEMENT_AREA_NAME)  # Add pop-up labels
  )%>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(lng = -126.5, lat = 54, zoom = 5) %>%
  addLayersControl(
    overlayGroups = c("Fishery Management Area"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(
    data = filtered_data_w_mapcol, 
    color = ~color, 
    weight = 1, 
    opacity = 0.06, 
    fillOpacity = 0.3, 
    label = ~paste("Recommended Fisheries in", MANAGEMENT_AREA_NAME),
    labelOptions = labelOptions(
    style = list("color" = "black"),
    textsize = "15px",
    direction = "auto"
  ))%>% 
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    ~longitude, ~latitude, 
    color = ~colour, 
    fillColor = ~colour,
    radius = 8,
    fillOpacity = 100,
    stroke = FALSE,  # No border
    popup = ~paste("<b>", fishery_location, "</b><br>", info)
  ) %>%
  addLegend(
    position = "bottomright",  # Position of the legend on the map
    colors = c((species_colors),"grey"),  # Colors to display in legend
    labels = c(names(species_colors),"Multiple Species"),  # Labels for the colors
    title = "Salmon Species",  # Title of the legend
    opacity = 1  # Opacity of the legend
)



#Display the map 
map
saveWidget(map, file = "salmonRAPSTA_map_2024.html", selfcontained = TRUE)
