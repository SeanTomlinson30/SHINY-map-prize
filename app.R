# load required libraries
source("packages.R")

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
                  selected = "Benin"),

      selectInput("selected_dist", "Select districts to compare:",
                  choices = c("Test", "To populate"),
                  selected = "Test"), 
      
      selectInput("years_to_compare", "Select year of interest:",
                  choices = c(2015),
                  selected = 2015),
      
      checkboxGroupInput("var_selection", "Select variables to compare:",
                  choices = c("Plasmodium falciparum Incidence",
                              "Plasmodium knowlesi Risk",
                              "Indoor residual spraying (IRS) coverage",
                              "Insecticide-treated bednet (ITN) coverage",
                              "Artemisinin-based combination therapy (ACT) coverage",
                              "Dominant Vectors",
                              "Malaria-attributable fever as a proportion of all-cause fever",
                              "Non-malarial fever",
                              "All-cause fever", 
                              "Accessibility: travel time to cities"),
                  selected = "Plasmodium falciparum Incidence"),
      
      uiOutput("select_dist")),
  
    # main panel (tabs) for the outputs
    mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel(title = "Selected country and districts", plotOutput("select_country")),
                tabPanel(title = "Raw variables of interest", plotOutput("")),
                tabPanel(title = "Selected district statistics - map", plotOutput("")),
                tabPanel(title = "Selected district statistics - ranking", plotOutput("")))
    
  )
  
  ), 
  
  actionButton(inputId = "processStats", label = "Generate statistics"),
  actionButton(inputId = "genReport", label = "Generate a summary report")
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
      admin_1[[1]][admin_1[[2]] == country_id]
    })

  }

  render_select <- function(input, label = "Select districts") {

    renderUI({
    reactive_input <- isolate(input)
    selected_dist <- unlist(district_select(input = reactive_input)())
    selectInput("selected_dist", "Select districts",
                choices = selected_dist, label = label,
                multiple = TRUE)
    })
  }
  
  output$select_country <- renderPlot({
    plot(africa[africa$COUNTRY == input$country, ],
         axes = FALSE,
         box = FALSE,
         main = "Selected country")
    
  })
  
  output$select_dist <- render_select(input$country)

  # using the input country, grab the rasters produced by MAP
  input_rasters <- malariaAtlas::getRaster()
  
  
}

# create Shiny app
shinyApp(ui, server)
