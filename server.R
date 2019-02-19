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
library(shinythemes)

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
    
    selectizeInput("select_raster", "Select rasters:", c_rasters, options = list(maxItems = 4))
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
    
    # 1. using the input 'selected_dist' and 'select_raster', grab the raster stats
    # include a progress bar to inform users 
    withProgress(message = "Generating statistics for selected covariates", value = 0, {
      
      lookup_processed$surface_name <- str_replace_all(lookup_processed$surface_name, '\\.', ' ')

      for(i in 1:length(input$select_raster)){
        
        # grab csv with the stats
        # 1. get a path to the stats csv
        stats_i_idx <- which(lookup_processed$surface_name == input$select_raster[[i]])
        stats_i_path <- lookup_processed$stats_path[stats_i_idx]
        
        # 2. read in the csv 
        stats_i <- read.csv(stats_i_path, stringsAsFactors = FALSE)
        
        # 3. subset csv to only retain selected_dist
        # create a selection based off of input
        country_select <- countries[countries$name == input$country, ]
        country_id <- country_select$COUNTRY_ID
        dist_select <- admin_1[admin_1$COUNTRY_ID == country_id, ]
        dist_select <- dist_select[dist_select$NAME %in% input$selected_dist, ]
        
        # subset
        stats_i_sub <- stats_i[stats_i$zone %in% dist_select$GAUL_CODE, ]
        
        # add a variable which is the district name
        index <- which(dist_select$GAUL_CODE == stats_i_sub$zone)
        stats_i_sub$District <- dist_select$NAME[index]
        dist_select$mean <- stats_i$mean[index]
        
        # reorder stats_i_sub
        stats_i_sub <- stats_i_sub[c(6, 2:5)]
        names(stats_i_sub) <- c('District',
                                paste0(input$select_raster[[i]], " (mean)"),
                                paste0(input$select_raster[[i]], " (max)"),
                                paste0(input$select_raster[[i]], " (min)"),
                                paste0(input$select_raster[[i]], " (sd)"))
        
        # update progress bar
        incProgress(1/length(input$select_raster)) 
        
        # render table
        output$stats_table <- renderTable(stats_i_sub,
                                          hover = TRUE,
                                          na = "NA", 
                                          main = input$select_raster[[i]])
        # render plot
        output$stats_plot <- renderPlot({
          
          # define colour ramp for plot
          n <- length(levels(dist_select$mean))
          colours <- colorRampPalette(brewer.pal(brewer.pal.info["YlGnBu",1], "YlGnBu"))(n)
          mean_colours <- colours[dist_select$mean]
            
          plot(countries[countries$name == input$country, ],
               axes = FALSE,
               col = "#d9d9d9",
               main = input$select_raster[[i]])
          
          plot(dist_select,
               add = TRUE,
               col = mean_colours,
               lty = 3)})
        
        }
      })})
  
}

