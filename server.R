# load required libraries
pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard, shinyBS, stringr)

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
function(input, output) {
  
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
    
    # 1. using the input 'selected_dist' and 'select_raster', grab the raster stats
    # include a progress bar to inform users 
    withProgress(message = "Generating statistics for selected covariates", value = 0, {
      
      lookup_processed$surface_name <- str_replace_all(lookup_processed$surface_name, '\\.', ' ')
      
      for(i in 1:length(input$var_selection)){
        
        # grab csv with the stats
        # 1. get a path to the stats csv
        
        stats_i_idx <- which(lookup_processed$surface_name == input$var_selection[[i]])
        stats_i_path <- lookup_processed$stats_path[stats_i_idx]
        
        ## DEBUGGING 
        write(x = input$varselection, file='test_input_var_select.txt')
        write(x = lookup_processed$surface_name, file='test_surface_name.txt')
        write(x = stats_i_path, file='test_stats_i_path.txt')
        write(x = stats_i_idx, file='test_stats_idx.txt')
        write(x = lookup_processed$stats_path, file='test_stats_path')
        
        # 2. read in the csv 
        stats_i <- read.csv(stats_i_path, stringsAsFactors = FALSE, sep=",")
        
        
        
        # update progress bar
        # incProgress(1/length(input$var_selection)) 
        
        # render table
        
        output$stats_table <- renderTable({head(stats_i, n = -1)},
                                           hover = TRUE,
                                           na = "NA")
        }
      })})
  
}

