library(dplyr)
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
      
      # Interface to select the name of the attribute
      selectInput(
        "sensor_id",
        "Sensor",
        choices = c("CAF397", "CAF314", "CAF231", "CAF275", "CAF316"),
        selected = 'UHDICM'
      )
      
    ),
      
    # Defining the main panel
    mainPanel(
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
    
  # Create the time series plot
  output$dygraph <- renderDygraph({
    
    sensors %>% 
      dplyr::filter(SOURCEID == input$sensor_id) %>% # Select the records for the selected sensor
      dplyr::select(-SOURCEID) %>% # Remove the ID column
      dplyr::select(Date, ends_with('VW')) %>% # Select the date and the volumetric water records
      xts(.$Date) %>% # Convert to XTS (time serie)
      dygraph # Create plot
    
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

