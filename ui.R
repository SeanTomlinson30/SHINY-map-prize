# load required libraries
#pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard, shinyBS, stringr)

library(raster)
library(shiny)
library(RColorBrewer)
library(malariaAtlas)
library(shinydashboard)
library(shinyBS)
library(stringr)

# generate a list of countries for which MAP data exists
africa <- shapefile('data/countries/Africa.shp')
africa$COUNTRY[africa$COUNTRY == "Congo-Brazzaville"] <- "Congo"
africa$COUNTRY[africa$COUNTRY == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
africa$COUNTRY[africa$COUNTRY == "Tanzania"] <- "United Republic of Tanzania"


# define a UI use a fluid bootstrap layout

navbarPage("Malaria Atlas Project - District comparison",
           tabPanel("Application",
                    fluidPage(    
                      
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
                      #titlePanel(HTML(paste("Malaria Atlas Project - District comparison", " ", " ", sep = "<br/>"))),
                      
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
                        
                        mainPanel(
                          
                          tabsetPanel(type = "tabs",

                                      tabPanel(title = "Selected country and districts", plotOutput("select_country", height = '800px', width = '800px')),
                                      tabPanel(title = "Selected district statistics - map", plotOutput("stats_plot")),
                                      tabPanel(title = "Selected district statistics - ranking", tableOutput("stats_table")))
                          

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
                          )),
           tabPanel("Help",
                    column(12, includeMarkdown("help.md"))),
           tabPanel("About",
                    column(12, includeMarkdown("about.md"))),
           tabPanel("Methodology",
                    column(12, includeMarkdown("methodology.md")))
)