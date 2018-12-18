# load required libraries
# install custom github libraries if not already installed
if(!require(pacman)){
  install.packages('pacman')
  library(pacman)
}

pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard)

# generate a list of countries for which MAP data exists
africa <- shapefile('data/countries/Africa.shp')

# load unique admin one and admin two locations
admin_1 <- shapefile('data/districts/admin_1.shp')
# admin 2 to do; will get working with admin 1 first

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
                 selected = "Benin"),

    # add a line break
    br(),

    checkboxGroupInput("dist_select", label = "Choose districts to compare:", 
                       choices = NULL)),
  
  # # add a panel to display the output
  mainPanel(
    
    div(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Selected districts", plotOutput("dist_plot"))
    )
  
    )
  )
  ))

# define the server logic
server <- function(input, output) {
  
  observeEvent(input$country,{
    updateSelectInput("dist_select",
                      choices = unique(admin_1$NAME[admin_1$COUNTRY_ID == africa$CODE[africa$COUNTRY == input$country]]))
  })
  
  output$dist_plot <- renderPlot({
    plot(admin_1[input$dist_select, ],
         axes = FALSE,
         main = "Tsetse suitability")
    
  })
  
}

# create Shiny app
shinyApp(ui, server)
