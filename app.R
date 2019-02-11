# load required libraries
pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard, shinyBS)

# generate a list of countries for which MAP data exists
countries <- shapefile('data/countries/admin2013_0.shp')
africa <- shapefile('data/countries/Africa.shp')
admin_1 <- shapefile('data/districts/admin_1.shp')

# Read in lookup ##Sean
lookup <- read.csv('data/combined_lookup.csv', sep=',', check.names = F)

###
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
      
      # hover-over tooltip
      bsTooltip(id = "country", 
                title = "Please select the country of interest, available districts will update based on this selection.", 
                placement = "right", trigger = "hover", options = list(container = "body")),
      
      # dynamic district selection
      uiOutput("select_dist"),
      
      # hover-over tooltip
      bsTooltip(id = "select_dist", 
                title = "Please select the districts to feature within the comparison/ranking.", 
                placement = "right", trigger = "hover", options = list(container = "body")),
      
      # dynamic district selection
      uiOutput("select_raster"),
      
      # hover-over tooltip
      bsTooltip(id = "select_raster", 
                title = "Please select the rasters to compare.", 
                placement = "right", trigger = "hover", options = list(container = "body"))),
    
    
    # variable of interest selection
    #checkboxGroupInput("var_selection", "Select variables to compare:",
    #            choices = c_rasters,
    #            selected = "Plasmodium falciparum Incidence")),
    
    # main panel (tabs) for the outputs
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Selected country and districts", plotOutput("select_country")),
                  tabPanel(title = "Raw variables of interest", plotOutput("")),
                  tabPanel(title = "Selected district statistics - map", plotOutput("")),
                  tabPanel(title = "Selected district statistics - ranking", plotOutput("")))
      
    )
    
  ), 
  
  # event to observe re statistics/ranking generation
  actionButton(inputId = "processStats", label = "Generate statistics"),
  # tooltip for 'Generate statistics'
  bsTooltip(id = "processStats", 
            title = "Run generation of statistics and ranking system. This will produce results which feature in the tabs to the right.", 
            placement = "right", trigger = "hover", options = list(container = "body")),
  
  # event to observe the generation of a summary report featuring stats outputs
  actionButton(inputId = "genReport", label = "Generate a summary report")
      )

# define the server logic
server <- function(input, output) {
  
  # create dynamic reactive list of districts, per input country
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
  
  output$select_raster <- renderUI({
    
    select_id <- as.character(input$country)
    
    if(select_id == "Cote d`Ivoire"){
      
      country_id <- "CIV"
      
    }else{
      
      country_id <- countries$COUNTRY_ID[countries$name == select_id][1]
      
    }
    
    c_lookup = lookup[lookup$name == input$country,]
    c_rasters <- colnames(c_lookup)[which(c_lookup==1)]
    c_rasters <- gsub('\\.', ' ', c_rasters) # Replace periods with spaces
    
    checkboxGroupInput("c_rasters", "Select rasters:",
                       choices = c_rasters,
                       inline = TRUE)
  })
  
  # plot selected country, with selected districts overlayed
  output$select_country <- renderPlot({
    
    country_select <- countries[countries$name == input$country, ]
    country_id <- country_select$COUNTRY_ID
    dist_select <- admin_1[admin_1$COUNTRY_ID == country_id, ]
    dist_select <- dist_select[dist_select$NAME %in% input$selected_dist, ]
    
    plot(countries[countries$name == input$country, ],
         axes = FALSE,
         col = "#d9d9d9",
         main = "Selected country")
    
    plot(dist_select,
         add = TRUE,
         col = "#41b6c4",
         lty = 3)
    
  })
  
  # observeEvent for "processStats"
  observeEvent(input$processStats, {
    
    # 1. using the input country, grab the rasters produced by MAP
    # create a covariate dataframe
    withProgress(message = "Downloading requested covariates from MAP API", value = 0, {
      
      for(i in 1:length(input$var_selection)){
        
        # grab raster from MAP API
        raster_i <- malariaAtlas::getRaster(surface = input$var_selection[i],
                                            year = NA)
        
        # update progress bar
        incProgress(1/length(input$var_selection)) 
        
        # stack the surfaces, if there's more than one selected
        if(length(input$var_selection > 1)){
          
          if(i == 1){
            
            stack <- raster_i
            
          } else {
            
            stack <- stack(stack, raster_i)  
            
          }
          
        } else {
          
          stack <- raster_i
          
        }
        
      }})})
  
}

# create Shiny app
shinyApp(ui, server)