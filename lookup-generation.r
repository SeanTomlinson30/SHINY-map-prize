# load required libraries
# script to source packages from the git 'packages' directory
library(pacman)
library(raster)
library(shiny)
library(RColorBrewer)
library(malariaAtlas)
library(shinydashboard)
require(rgdal)

raster_meta <- as.data.frame(available_rasters <- listRaster())

raster_name_list  = as.list(raster_meta$title)

raster_list <- lapply(raster_name_list, getRaster)

raster_stack <- stack(raster_list)

admin0shapefile <- shapefile('data/countries/admin2013_0.shp')

poly_mean <- over(admin0shapefile, raster_stack, fun=mean)


getRaster(available_rasters)
