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

if(!require(mapview)){
  #devtools::install_github("r-spatial/mapview@develop")
  install.packages("mapview")
  library(mapview)
}

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}
#for interactive table
if(!require(DT)){
  install.packages("DT")
  library(DT)
}


# read in MAP availability lookup table
lookup <- read.csv('data/combined_lookup.csv', sep = ',', check.names = FALSE)

# read in the processed data lookup table
lookup_processed <- read.csv('data/raster_stats_paths.csv', stringsAsFactors = FALSE)

# load simplified admin polygons
load('data/sf_afr_simp_fao.rda')
# correct some encoding issues
sf_afr_simp$name[sf_afr_simp$GAUL_CODE == "16840"] <- "Goh-Djiboua"
sf_afr_simp$name[sf_afr_simp$GAUL_CODE == "818"] <- "Extreme-Nord"
sf_afr_simp$name[sf_afr_simp$GAUL_CODE == "66"] <- "Cote d'Ivoire"

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
  
  # fix for encoding issue
  if(country_name == "Cote d'Ivoire"){
    
    country_id <- "CIV"
    
  } else {
    
  country_id <- sf_afr_simp$COUNTRY_ID[sf_afr_simp$name==country_name]
  
  }
  
  country_id <- as.character(country_id)
  
}

# get district names for selected country id
get_dist_names <- function(country_id) {
  
  country_id <- as.character(country_id)
  dist_names <- sf_afr_simp$name[sf_afr_simp$COUNTRY_ID==country_id & sf_afr_simp$ADMN_LEVEL==1] 
  
}

# define the server logic
function(input, output, session) {
  
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
            #"Plasmodium falciparum Incidence"
            "Plasmodium falciparum PR2 10" = m <- m + mapView(pfpr2_10_2015,
                                                              col.regions = colorRampPalette(brewer.pal(brewer.pal.info["YlGnBu",1], "YlGnBu"))),
            "Insecticide treated bednet  ITN  coverage" = m <- m + mapView(itn_2015,
                                                                           col.regions = colorRampPalette(brewer.pal(brewer.pal.info["YlGnBu",1], "YlGnBu"))),
            # changed breaks to show more detail at the values in malaria countries
            "A global map of travel time to cities to assess inequalities in accessibility in 2015" = m <- m + mapview(time_to_city_2015, 
                                                                                                                       col.regions = colorRampPalette(brewer.pal(brewer.pal.info["YlGnBu",1], "YlGnBu")),
                                                                                                                       at = rev(c(0,60,120,180,240,400,600,800,1200,1600,2400,3200,6400,10000))))
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

  ##########################################
  # interactive table to show district stats
  output$activetable = DT::renderDataTable({
    
    # get country and raster_ids
    # sets reactive dependence on these
    country_id <- get_country_id(input$country)
    raster_ids <- input$selected_raster
    
    #message("in activetable raster_ids :",raster_ids)
    
    sf_cntry <- sf_afr_simp[sf_afr_simp$COUNTRY_ID==country_id,]
    
    # subset selected districts
    sf_dist_select <- sf_cntry[sf_cntry$name %in% input$selected_dist,] 
    
    if (length(input$selected_raster) == 0){
      
      return(NULL)
      
    } else { 
      
      # code to get data copied from processStats
      
      # sub to get names aligned
      lookup_processed$surface_name <- str_replace_all(lookup_processed$surface_name, '\\.', ' ')
      
      # define empty vector to populate with stats tables
      stats_list <- NULL
      
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
        
        #stats_list[[i]] <- stats_i_sub
        #andy new code to put just means from diff layers into one table
        #get district name from first layer
        if (i==1) stats_layer_means <- stats_i_sub[c(1)]
        #add means and ranks from other layers
        
        #BEWARE this relies on district names being the same and in same order
        # means
        stats_layer_means <- cbind(stats_layer_means, stats_i_sub[2])   
        # ranks
        # reverse priority for malaria layers (high malaria=high priority=low rank)
        #BEWARE we should create column in lookup_processed specifying whether a high value = high priority
        if(length(grep("Plasmodium",input$selected_raster[[i]]))>0)
          priority <- rank(-stats_i_sub[2])
        else
          priority <- rank(stats_i_sub[2])

          
        
        #names(ranks) <- "(rank)" #this failed to get into table but ranks is cool
        stats_layer_means <- cbind(stats_layer_means, priority)  
        
        
      }      
    }
    
    #to go in the table  
    #stats_i_sub     
    #stats_layer_means
    
    #round data same as in reports, not first column
    stats_layer_means[-1] <- round(stats_layer_means[-1],2)
    # replace negative values as NA
    stats_layer_means[stats_layer_means[] < 0] <- NA
    
    DT::datatable(stats_layer_means, 
                  rownames=FALSE, 
                  #fillContainer = FALSE,
                  options = list(paging=FALSE))
    
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
