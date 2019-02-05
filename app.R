# load required libraries
pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard)

# generate a list of countries for which MAP data exists
countries <- shapefile('data/countries/admin2013_0.shp')
africa <- shapefile('data/countries/Africa.shp')
admin_1 <- shapefile('data/districts/admin_1.shp')

africa$COUNTRY[africa$COUNTRY == "Congo-Brazzaville"] <- "Congo"
africa$COUNTRY[africa$COUNTRY == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
africa$COUNTRY[africa$COUNTRY == "Tanzania"] <- "United Republic of Tanzania"

# define a UI use a fluid bootstrap layout
ui <- fluidPage(    
  
  # set a margin for the checkbox
  tags$head(
    tags$style(
      HTML(".checkbox-inline { 
        margin-left: 0px;
        margin-right: 10px;
        }
        .checkbox-inline+.checkbox-inline {
        margin-left: 0px;
        margin-right: 10px;
        }"))),
  
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

      uiOutput("select_dist"), 
      
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
                  selected = "Plasmodium falciparum Incidence")),
  
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

  output$select_dist <- renderUI({
    
    select_id <- as.character(input$country)
    
    if(select_id == "Cote d`Ivoire"){
      
      country_id <- "CIV"
      
    }else{
      
      country_id <- countries$COUNTRY_ID[countries$name == select_id][1]
    
    }
    
    selected_dist <- admin_1$NAME[admin_1$COUNTRY_ID == country_id]
      
    checkboxGroupInput("selected_dist", "Select first-level administrative division:",
                       choices = selected_dist,
                       inline = TRUE)
    })

  
  output$select_country <- renderPlot({
    plot(countries[countries$name == input$country, ],
         axes = FALSE,
         main = "Selected country")
    
  })
  

  # using the input country, grab the rasters produced by MAP
  # input_rasters <- malariaAtlas::getRaster()
  
  
}

# create Shiny app
shinyApp(ui, server)
