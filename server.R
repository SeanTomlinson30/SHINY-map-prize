# load required libraries
#pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard, shinyBS, stringr)

library(raster)
library(shiny)
library(RColorBrewer)
library(malariaAtlas)
library(shinydashboard)
library(stringr)
library(shinyalert)
library(shinyBS)

# generate a list of countries for which MAP data exists
countries <- shapefile('data/countries/admin2013_0.shp')
admin_1 <- shapefile('data/districts/admin_1.shp')

# read in MAP availability lookup table
lookup <- read.csv('data/combined_lookup.csv', sep = ',', check.names = FALSE)

# read in the processed data lookup table
lookup_processed <- read.csv('data/raster_stats_paths.csv', stringsAsFactors = FALSE)

# read in Africa shapefile and correct a few naming issues
africa <- shapefile('data/countries/Africa.shp')
africa$COUNTRY[africa$COUNTRY == "Congo-Brazzaville"] <- "Congo"
africa$COUNTRY[africa$COUNTRY == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
africa$COUNTRY[africa$COUNTRY == "Tanzania"] <- "United Republic of Tanzania"

# define the server logic
function(input, output, session) {
  
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
    
    c_lookup <- lookup[lookup$COUNTRY_ID == country_id,]
    c_rasters <- colnames(c_lookup)[which(c_lookup==1)]
    c_rasters = str_replace_all(c_rasters, '\\.', ' ') # Replace periods with spaces
    
    selectizeInput("select_raster", "Select rasters:", c_rasters, multiple = TRUE, options = list(maxItems = 4))
    # checkboxGroupInput("select_raster", "Select rasters:",
    #                    choices = c_rasters,
    #                    inline = TRUE)
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

  my_max=4
  my_min=1
  observe({
    if(length(input$select_raster) > my_max)
    {
      shinyalert("Oops!", "We are currently limited to a max of 4 rasters selections \n Please adjust raster selections", type = "warning")
      #showNotification('Please only select a maximum of 4 surfaces', type = 'warning')
    }})
    # if(length(input$select_raster) < )
    # {
    #   print('Error')
    # }
    # })
  
  # observeEvent for "processStats"
  observeEvent(input$processStats, {
    
    updateTabsetPanel(session=session, inputId = 'main0', selected = 'tab3')
  
    # check for max four inputs   
    observe({
      if(length(input$select_raster) > my_max)
      {
        shinyalert("Oops!", "We are currently limited to a max of 4 rasters selections \n Please adjust raster selections", type = "warning")
        #showNotification('Please only select a maximum of 4 surfaces', type = 'warning')
      }})
    
    # generate tables as a markdown
    output$report <- downloadHandler(
      
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "render_stats.Rmd")
        file.copy("render_stats.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(select_raster = input$select_raster,
                       country = input$country,
                       selected_dist = input$selected_dist)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        markdown::markdownToHTML(tempReport, output_file = file,
                                 params = params,
                                 envir = new.env(parent = globalenv())
        )
      }
    )
  }
  )

}

