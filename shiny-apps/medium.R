library(tidyverse)
library(shiny)
library(xts)
library(dygraphs)

# Define UI for application that draws a histogram
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
        
        # Quick look at the histogram
        plotOutput('hist_ts')
      ),
      
      # Defining the main panel
      mainPanel(
         dygraphOutput('dygraph', width = '100%', height = '500px')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Data
  data('cookfarm', package = "GSIF")
  
  # Extract data
  sensors <- cookfarm$readings
  
  # 
  # The important stuff: reactive contexts
  # 

  # Routine that updates the data
  get_data <- reactive({
    sensors %>% 
      filter(SOURCEID == input$sensor_id) %>% # Select the records for the selected sensor
      dplyr::select(SOURCEID, Date, ends_with('VW')) # Select the date and the volumetric water records
  })
  
  # Routine that creates the time serie form the updated data
  get_ts <- reactive({
    get_data() %>%  # Get the latest data
      dplyr::select(-SOURCEID) %>% # Remove the ID column
      xts(.$Date) # Convert to XTS (time series)
  })
  
  # Plot graph from the updated time serie
  output$dygraph <- renderDygraph({
    
    dygraph( get_ts() )
    
   })
  
  # Plot statistical distribution on the side bar
  output$hist_ts <- renderPlot({
    get_data() %>% 
      gather(key, value, -SOURCEID, -Date) %>% # Convert to long format 
      .$value %>% # Extract value
      hist(main = paste0("Distribution for sensor ", input$sensor_id)) # Create histogram
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

