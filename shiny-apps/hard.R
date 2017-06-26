library(tidyverse)
library(shiny)
library(xts)
library(sf)
library(dygraphs)
library(leaflet)

# Define UI for application
ui <- fluidPage(
  
   # Application title
   titlePanel("Soil moisture sensor data on Cookfarm"),
   
   # Using a layout with a sidebar and a main panel
   sidebarLayout(
     
      # Defining the sidebar
      sidebarPanel(
        
        # Interface to select the sensor
        selectInput(
          "sensor_id",
          "Sensor",
          choices = c("CAF397", "CAF314", "CAF231", "CAF275", "CAF316"),
          selected = 'UHDICM'
        ),
        
        # Quick look at the map
        leafletOutput('map')
      ),
      
      # Defining the main panel
      mainPanel(
        # Graph of the time serie
        dygraphOutput('dygraph', width = '100%', height = '500px')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Load data
  data('cookfarm', package = "GSIF")
  
  # Extract sensor data
  sensors <- cookfarm$readings
  
  # Extract profile data
  profiles <- cookfarm$profiles
  
  # Routine that updates the data
  get_data <- reactive({
    sensors %>% 
      filter(SOURCEID == input$sensor_id) %>% # Filter the records that match the current sensor ID 
      dplyr::select(SOURCEID, Date, ends_with('VW')) # Select the ID, the date, and the volumetric moisture content
  })
  
  # Routine that creates the time serie form the updated data
  get_ts <- reactive({
    get_data() %>%  # Update the data
      dplyr::select(-SOURCEID) %>% # Remove the sensor 
      xts(.$Date) # Convert to time serie object
  })
  
  # Plot graph from the updated time serie
  output$dygraph <- renderDygraph({
    
    get_ts() %>% # Update the time serie object 
      dygraph(ylab = 'Volumetric Water Content (%)', main = paste0('Volumetric Water Content for Sensor ', input$sensor_id) )# Creates interactive plot
    
   })
  
  # Get Simple Feature object from sensor ID
  get_point <- reactive({
    profiles %>% 
      filter(SOURCEID == input$sensor_id) %>% 
      distinct(Easting, Northing) %>% 
      st_as_sf(coords = c('Easting', 'Northing'), crs = cookfarm$proj4string) %>% 
      st_transform(crs = 4326)
  })
  
  # Plot location of sensor
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(provider = "Esri.WorldImagery") %>% 
      addMarkers(data = get_point() )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
