
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

if(!require(shinycssloaders)){
  install.packages("shinycssloaders")
  library(shinycssloaders)
}

if(!require(shinyjs)){
  install.packages("shinyjs")
  library(shinyjs)
}

if(!require(mapview)){
  install.packages("mapview")
  library(mapview)
}

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}

# generate a list of countries for which MAP data exists
load('data/sf_afr_simp_fao.rda')
sf_afr_simp <- sf_afr_simp[sf_afr_simp$COUNTRY_ID != "XXX",]
sf_afr_simp <- sf_afr_simp[sf_afr_simp$COUNTRY_ID != "MYT",]	
sf_afr_simp$name[sf_afr_simp$GAUL_CODE == "16840"] <- "Goh-Djiboua"
sf_afr_simp$name[sf_afr_simp$GAUL_CODE == "818"] <- "Extreme-Nord"
country_names <- sf_afr_simp$name[sf_afr_simp$ADMN_LEVEL==0]	
country_names <- country_names[country_names != "Hala'ib triangle"]	
country_names <- country_names[country_names != "Ma'tan al-Sarra"]

# define a UI use a fluid bootstrap layout
appCSS <- "
#loading-content {
  position: absolute;
  background: #344151;
  opacity: 1;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

navbarPage(
  "MAP-district-comparison",
  tabPanel("Application",
           fluidPage(theme = shinytheme("flatly"),
                     useShinyalert(),
                     useShinyalert(),
                     useShinyjs(),
                     inlineCSS(appCSS),
                     
                     # Loading message
                     div(
                       id = "loading-content",
                       h2("Loading Application...")
                     ),
                     
                     hidden(
                       div(
                         id = "app-content",
                         p(" ")
                       )
                     ),
                     # set a margin for the checkbox
                     tags$head(
                       tags$style(
                         HTML(".checkbox-inline {
                              margin-left: 0px;
                              margin-right: 10px;}
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
                         uiOutput("tab"),
                         br(),
                         "More information can be found in the help tab.",
                         br(),
                         br(),

                         # country selection (only one country allowed at a time)
                         selectInput("country", "Country :",
                                     choices = country_names,
                                     selected = "Benin"),
                         
                         # hover-over tooltip
                         bsTooltip(id = "country",
                                   title = "Please select the country of interest, available districts will update based on this selection.",
                                   placement = "right", trigger = "hover", options = list(container = "body")),
                         
                         # andy choosing limited number raster layers
                         # maybe should be radio buttons to encourage just one layer
                         # OR could allow multiple layers and use syncview
                         checkboxGroupInput("selected_raster", "Data to show and compare :",
                                     choices = list("Malaria in children (Falciparum)" = "Plasmodium falciparum Incidence",
                                                    "Insecticide Treated Net distribution" = "Insecticide treated bednet  ITN  coverage",
                                                    "Travel time to nearest city" = "A global map of travel time to cities to assess inequalities in accessibility in 2015"), 
                                     selected = "Plasmodium falciparum Incidence"),
                         
                         helpText("First layer is shown in map, other layers included in 'Output'"),
                         
                         # radioButtons("selected_raster", "Data to show and compare :",
                         #              choices = list('Malaria in children (Falciparum)' = 1,
                         #                             'Insecticide Treated Net distribution' = 2,
                         #                             'Travel time to nearest city' = 3), 
                         #              selected = 3),
                         
                         # dynamic district selection
                         uiOutput("select_dist"),
                         
                         # hover-over tooltip
                         bsTooltip(id = "select_dist",
                                   title = "Please select the districts to feature within the comparison/ranking.",
                                   placement = "right", trigger = "hover", options = list(container = "body")),
                         
                         # andy commented out replaced by selection of fewer layers above
                         # dynamic raster selection
                         # uiOutput("select_raster") %>% withSpinner(type = '7', color="#0dc5c1"),
                         # 
                         # # hover-over tooltip
                         # bsTooltip(id = "select_raster",
                         #           title = "Please select the variables to compare.",
                         #           placement = "right", trigger = "hover", options = list(container = "body")),
                         # 
                         # helpText("Information on variable descriptions can be found within the 'help' tab of this app."),
                         
                         actionButton(inputId = "processStats", label = "Generate statistics", class='butt'),
                         tags$head(tags$style(".butt{margin-bottom:5px;}")),
                         
                         bsTooltip(id = "processStats",
                                   title = "Run generation of statistics and ranking system. This will produce results which feature in the tabs to the right.",
                                   placement = "right", trigger = "hover", options = list(container = "body")),
                         
                         uiOutput("downloadbutton")
                       ),
                       
                       mainPanel(
                         
                         tabsetPanel(id='main0', type = "tabs",
                                     #tabPanel(value ='tab1', title = "Selected country and districts", div(style = 'overflow-y:scroll;height:750px;',plotOutput("select_country", height = '750px', width = '750px'))),
                                     tabPanel(value ='tab1', title = "Map", div(style = 'overflow-y:scroll;height:750px;',leafletOutput("mapview_country_raster", height = '750px', width = '750px'))),
                                     #tabPanel(value ='tab1', title = "Map", leafletOutput("mapview_country_raster")),
                                     tabPanel(value ='tab2', title = "Output", div(style = 'overflow-y:scroll;height:750px;',htmlOutput("report"))))
                       ) # enf of main panel
                     ) # end of fluid page # end of sidebar layout
                         ) 
                       ), # end of tab panel
  tabPanel("Help",
           tabsetPanel(type = 'tabs',
                       tabPanel(title='Help', includeMarkdown('help.md')),
                       tabPanel(title='About', includeMarkdown('about.md'))))
)