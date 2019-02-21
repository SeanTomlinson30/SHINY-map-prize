# load required libraries
#pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard, shinyBS, stringr)

#### load required libraries ####
if(!require(raster)){
  install.packages("raster")
  library(raster)
}

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(malariaAtlas)){
  install.packages("malariaAtlas")
  library(malariaAtlas)
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard)
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

if(!require(shinyalert)){
  install.packages("shinyalert")
  library(shinyalert)
}

if(!require(shinyBS)){
  install.packages("shinyBS")
  library(shinyBS)
}

if(!require(shinythemes)){
  install.packages("shinythemes")
  library(shinythemes)
}

if(!require(knitr)){
  install.packages("knitr")
  library(knitr)
}

if(!require(kableExtra)){
  install.packages("kableExtra")
  library(kableExtra)
}

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

  })
  
  # plot selected country, with selected districts overlayed
  output$select_country <- renderPlot({
    
    select_id <- as.character(input$country)
    
    # fix for CIV plot
    if(select_id == "Cote d`Ivoire"){
      
      country_id <- "CIV"
      
    } else {
    
      country_id <- countries$COUNTRY_ID[countries$name == select_id][1]
    
    }
    
    dist_select <- admin_1[admin_1$COUNTRY_ID == country_id, ]
    dist_select <- dist_select[dist_select$NAME %in% input$selected_dist, ]
    
    plot(countries[countries$COUNTRY_ID == country_id, ],
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
    
    # check for district selection inputs   
    if(length(input$selected_dist) == 0){
      
      shinyalert("Oops!", "Please select a district", type = "warning")

    }    
    
    # check for max four variable inputs   
    else if (length(input$select_raster) == 0){
      
      shinyalert("Oops!", "Please select a raster", type = "warning")

    } else {
      
      updateTabsetPanel(session=session, inputId = 'main0', selected = 'tab3')
      
      # sub to get names aligned
      lookup_processed$surface_name <- str_replace_all(lookup_processed$surface_name, '\\.', ' ')
      
      # define empty vector to populate with stats tables
      stats_list <- NULL
      
      # create a selection based off of input
      select_id <- as.character(input$country)
      
      # fix for CIV plot
      if(select_id == "Cote d`Ivoire"){
        
        country_id <- "CIV"
        
      } else {
        
        country_id <- countries$COUNTRY_ID[countries$name == select_id][1]
        
      }
      
      country_select <- countries[countries$COUNTRY_ID == country_id, ]
      
      dist_select <- admin_1[admin_1$COUNTRY_ID == country_id, ]
      dist_select <- dist_select[dist_select$NAME %in% input$selected_dist, ]
      
      for(i in 1:length(input$select_raster)){
        
        # grab csv with the stats
        # 1. get a path to the stats csv
        stats_i_idx <- which(lookup_processed$surface_name == input$select_raster[[i]])
        stats_i_path <- lookup_processed$stats_path[stats_i_idx]
        
        # 2. read in the csv 
        stats_i <- read.csv(stats_i_path, stringsAsFactors = FALSE)
        
        # 3. subset csv to only retain selected_dist
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
        
        stats_list[[i]] <- stats_i_sub
        
      }
      
      # generate tables as a markdown
      # copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "render_stats.rmd")
      file.copy("render_stats.rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(stats_list = stats_list,
                     dist_select = dist_select,
                     country_select = country_select)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = paste0(tempdir(), "/pop_stats.rmd"),
                        output_format = "md_document",
                        params = params,
                        envir = globalenv())
      
      getPage <- function() {
        
        return(includeHTML(paste0(tempdir(), "/pop_stats.rmd")))
        
      }
      
      output$report <- renderUI({getPage()})
      
    }
  }
  )
}

