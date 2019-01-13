# load required libraries
# install custom github libraries if not already installed
if(!require(pacman)){
  install.packages('pacman')
  library(pacman)
}

pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard)

# generate a list of countries for which MAP data exists
africa <- suppressWarnings(shapefile('data/countries/Africa.shp'))

# load unique admin one and admin two locations
admin_1 <- shapefile('data/districts/admin_1.shp')

# define a UI use a fluid bootstrap layout
ui <- fluidPage(    
  
  # page title
  titlePanel("Malaria Atlas Project - District comparison"),
  
  # create a sidebar where the user can select a country, and districts (etc.)
  # we may change this to a header once basic functionality is resolved
  sidebarLayout(
    
    # sidebar panel for the inputs
    sidebarPanel(
      
      # country of interest selection (only one country allowed at a time)
      selectInput("country", "Select country of interest:",
                  choices = unique(africa$COUNTRY),
                  selected = "Benin")),
    
      uiOutput("select_dist")
    
        )
  )

# define the server logic
server <- function(input, output) {
  
  countries <- unique(africa$NAME)
  
  # update available district choices
  district_select <- function(input) {
    
    # return a reactive list of input values 
    reactive({
      select_id <- as.character(input)
      country_id <- as.character(africa$CODE[africa$COUNTRY == select_id])
      admin_1$NAME[admin_1$COUNTRY_ID == country_id]
    })
    
  }
  
  render_select <- function(input, label = "Select districts") {
    
    renderUI({
    reactive_input <- isolate(input$country)
    selected_dist <- unlist(district_select(input = reactive_input)())
    selectInput("selected_dist", "Select districts", 
                choices = selected_dist, label = label, 
                multiple = TRUE)
    })
  }
  
  output$select_dist <- render_select(input$country)

}

# create Shiny app
shinyApp(ui, server)
