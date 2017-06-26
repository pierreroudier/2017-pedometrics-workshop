library(shiny)

# Define UI for application
ui <- fluidPage(
  
   # Application title
   titlePanel("Soil profile data on Cookfarm"),
   
   # Using a layout with a sidebar and a main panel
   sidebarLayout(
     
      # Defining the sidebar
      sidebarPanel(
        
        # Interface to select the name of the attribute
        selectInput(
          "attribute",
          "Attribute",
          choices = c('UHDICM', 'LHDICM', 'BLD', 'PHIHOX'),
          selected = 'UHDICM'
        )
        
      ),
      
      # Defining the main panel
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Load data
  data('cookfarm', package = "GSIF")
  
  # Extract the soil profiles
  profiles <- cookfarm$profiles
  
  # Create the plot to be rendered  
  output$distPlot <- renderPlot({
    
    # Create histogram
    hist(
      profiles[[input$attribute]], 
      col = 'darkgray', 
      border = 'white',
      main = paste0("Distribution for sensor ", input$attribute)
    )
   
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

