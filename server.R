
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

if(!require(shinyjs)){
  install.packages("shinyjs")
  library(shinyjs)
}

if(!require(plotfunctions)){
  install.packages("plotfunctions")
  library(plotfunctions)
}

if(!require(sf)){
  install.packages("sf")
  library(sf)
}

# if(!require(devtools)){
#   install.packages("devtools")
#   library(devtools)
# }

if(!require(mapview)){
  #devtools::install_github("r-spatial/mapview@develop")
  install.packages("mapview")
  library(mapview)
}

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}

# read in MAP availability lookup table
lookup <- read.csv('data/combined_lookup.csv', sep = ',', check.names = FALSE)

# read in the processed data lookup table
lookup_processed <- read.csv('data/raster_stats_paths.csv', stringsAsFactors = FALSE)

#load simplified admin polygons
load('data/sf_afr_simp_fao.rda')

# raster layers for Africa downloaded, simplified and saved in download-rasters.r
load('data/rasters/pfpr2_10_2015.rda')
load('data/rasters/time_to_city_2015.rda')
load('data/rasters/itn_2015.rda')

# to allow checking for map changes
country_id_last <- FALSE
raster_id_last <- FALSE
lastmap <- FALSE

# get the country_id (e.g. CIV) for selected country name
get_country_id <- function(country_name) {
  
  country_name <- as.character(country_name)
  country_id <- sf_afr_simp$COUNTRY_ID[sf_afr_simp$name==country_name]
  country_id <- as.character(country_id)
  
}

# get district names for selected country id
get_dist_names <- function(country_id) {
  
  country_id <- as.character(country_id)
  dist_names <- sf_afr_simp$name[sf_afr_simp$COUNTRY_ID==country_id & sf_afr_simp$ADMN_LEVEL==1] 
  
}

# define the server logic
function(input, output, session) {
  # Simulate work being done for 1 second
  Sys.sleep(1)
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")

  url <- a("MAP Homepage", href="https://map.ox.ac.uk/")
  
  output$tab <- renderUI({
    
    tagList("MAP-district-comparison is a shiny app that allows easy interaction with summary statistics and plots for data provided by the Malaria Atlas Project:", url)
  
    })
  
  # create dynamic reactive list of districts, per input country
  output$select_dist <- renderUI({
    
    # get the country_id (e.g. CIV) for selected country name
    country_id <- get_country_id(input$country)
    
    # get names of districts in this country
    selected_dist <- get_dist_names(country_id)    
    
    checkboxGroupInput("selected_dist", "Select first-level administrative division (min 2):",
                       choices = selected_dist,
                       selected = selected_dist,
                       inline = TRUE)
  })
  
  output$select_raster <- renderUI({
    
    # get the country_id (e.g. CIV) for selected country name
    country_id <- get_country_id(input$country)
    
    c_lookup <- lookup[lookup$COUNTRY_ID == country_id,]
    c_rasters <- colnames(c_lookup)[which(c_lookup==1)]
    c_rasters = str_replace_all(c_rasters, '\\.', ' ') # Replace periods with spaces
    
    selectizeInput("select_raster", "Select rasters (max 4):", c_rasters, multiple = TRUE, options = list(maxItems = 4, placeholder='Select desired rasters by clicking or typing in this search box'))
    
  })

  # mapview interactive leaflet map plot
  output$mapview_country_raster <- renderLeaflet({
    
    # get the country_id (e.g. CIV) for selected country name
    country_id <- get_country_id(input$country)
    raster_id <- input$selected_raster[1]
    
    # exit function if country and layer haven't changed
    # to reduce waiting time for plot changes
    if(!is.null(raster_id) & !is.null(raster_id_last)){
      if(country_id==country_id_last & raster_id==raster_id_last)
        return(lastmap)      
    }

    # subset the country (includes districts)
    sf_cntry <- sf_afr_simp[sf_afr_simp$COUNTRY_ID==country_id & sf_afr_simp$ADMN_LEVEL==1,]
    
    # add country boundaries to the plot first
    m <- mapview(sf_cntry,
                 color = 'darkgrey',
                 lwd = 2,
                 legend = FALSE,
                 alpha.regions = 0, 
                 zcol = 'name')    
    
    # add the first selected raster to the plot
    if(!is.null(input$selected_raster)){

      switch(input$selected_raster[1],
            "Plasmodium falciparum Incidence" = m <- m + mapView(pfpr2_10_2015),
            "Insecticide treated bednet  ITN  coverage" = m <- m + mapView(itn_2015),
            # changed breaks to show more detail at the values in malaria countries
            "A global map of travel time to cities to assess inequalities in accessibility in 2015" = m <- m + mapview(time_to_city_2015, at=rev(c(0,200,400,800,1600,3200,6400,10000)), 
                                                 col.regions = rev(viridisLite::inferno(n=7))))
    }
    
    # record current ids so can check if they change above
    # set global vars, possibly bad practice
    country_id_last <<- country_id
    raster_id_last <<- raster_id    
  
    # set extent of map to the selected country 
    bbox <- as.vector(sf::st_bbox(sf_cntry))      
    m <- leaflet::fitBounds(m@map, bbox[1], bbox[2], bbox[3], bbox[4])
    
    # save last map to a global var, possibly dodgy
    # to allow avoiding changing map if it hasn't changed
    lastmap <<- m
  })  

  # DEPRECATED now mapview_country_raster() used instead   
  # plot selected country, with selected districts overlayed
  output$select_country <- renderPlot({
    
    # get the country_id (e.g. CIV) for selected country name
    country_id <- get_country_id(input$country)
    
    # subset the country (includes districts)
    sf_cntry <- sf_afr_simp[sf_afr_simp$COUNTRY_ID==country_id,]
    
    # andy testing plotting a raster layer
    # DEPRECATED
    # NOW I think this is better done with mapview_country_raster
    # TODO determine which layer by the first selected one from the list
    # show pfpr2-10 (or whichever other deemed most interesting) as default
    raster::plot(pfpr2_10_2015,ext=extent(sf_cntry))
    
    plot(sf::st_geometry(sf_cntry),
         #col = "#d9d9d9",
         #main = input$country,
         lty = 3, #dotted here so we can see which selected below, could be done by colour
         add = TRUE)
    
    # subset selected districts
    sf_dist_select <- sf_cntry[sf_cntry$name %in% input$selected_dist,] 
    
    plot(sf::st_geometry(sf_dist_select),
         #col = "#0dc5c1",
         #lty = 3,
         add = TRUE)
    
  })
  
  # observeEvent for "processStats"
  observeEvent(input$processStats, {
    
    output$downloadbutton <- renderUI({
      downloadButton('download', 'Download File ...')
    })

    # check for district selection inputs   

    show("download")
    # check for max four inputs   
    if(length(input$selected_dist) < 2){
      
      shinyalert("Oops!", "Please select at least 2 districts to compare", type = "warning")

    }    
    
    # check for max four variable inputs   
    else if (length(input$selected_raster) == 0){
      
      shinyalert("Oops!", "Please select a raster", type = "warning")

    } else {
      
      updateTabsetPanel(session=session, inputId = 'main0', selected = 'tab2')
      
      # sub to get names aligned
      lookup_processed$surface_name <- str_replace_all(lookup_processed$surface_name, '\\.', ' ')
      
      # define empty vector to populate with stats tables
      stats_list <- NULL
      
      # get the country_id (e.g. CIV) for selected country name
      country_id <- get_country_id(input$country)
      
      sf_cntry <- sf_afr_simp[sf_afr_simp$COUNTRY_ID==country_id,]
      
      # subset selected districts
      sf_dist_select <- sf_cntry[sf_cntry$name %in% input$selected_dist,] 
      
      for(i in 1:length(input$selected_raster)){
        
        # grab csv with the stats
        # 1. get a path to the stats csv
        stats_i_idx <- which(lookup_processed$surface_name == input$selected_raster[[i]])
        stats_i_path <- lookup_processed$stats_path[stats_i_idx]
        
        # 2. read in the csv 
        stats_i <- read.csv(stats_i_path, stringsAsFactors = FALSE)
        
        # 3. subset csv to only retain selected_dist
        stats_i_sub <- stats_i[stats_i$zone %in% sf_dist_select$GAUL_CODE, ]
        
        # add a variable which is the district name
        index <- which(sf_dist_select$GAUL_CODE == stats_i_sub$zone)
        stats_i_sub$District <- sf_dist_select$name[index]
        sf_dist_select$mean <- stats_i$mean[index]
        
        # reorder stats_i_sub
        stats_i_sub <- stats_i_sub[c(6, 2:5)]
        names(stats_i_sub) <- c('District',
                                paste0(input$selected_raster[[i]], " (mean)"),
                                paste0(input$selected_raster[[i]], " (max)"),
                                paste0(input$selected_raster[[i]], " (min)"),
                                paste0(input$selected_raster[[i]], " (sd)"))
        
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
                     dist_select = sf_dist_select,
                     country_select = sf_cntry)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file.path(tempdir(), "pop_stats.rmd"),
                        output_format = "md_document",
                        params = params,
                        envir = globalenv())
      
      getPage <- function() {
        
        return(includeMarkdown(file.path(tempdir(), "pop_stats.rmd")))
        
      }
      
      output$report <- renderUI({getPage()})
      
    }
  }
  )
    # download generated statistics as a html document
    output$download <- downloadHandler(
        
      filename = paste0("MAP_output_", Sys.Date(), ".html"),
      
      content = content <- function(file) {
          
        rmarkdown::render(input = file.path(tempdir(), "pop_stats.rmd"),
                            output_file = paste0(tempdir(), "/MAP_output_", Sys.Date(), ".html"),
                            output_format = "html_document")
          
        file.copy(paste0(tempdir(), "/MAP_output_", Sys.Date(), ".html"), file)
        
        },
        
      contentType = "text/html")
}
