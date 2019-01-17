# load required libraries
pacman::p_load(raster, shiny, RColorBrewer, malariaAtlas, shinydashboard, rgdal, googledrive)
##
# load in shapefile
admin0shapefile <- shapefile('data/countries/admin2013_0.shp')

# download admin 0 raster
# https://drive.google.com/open?id=1xY9meFKuR4UqJvc_LwayI11zupvbj5ZQ
temp <- tempfile()
dl <- drive_download(as_id("1xY9meFKuR4UqJvc_LwayI11zupvbj5ZQ"), path = temp, overwrite = TRUE)
admin0raster <- raster(dl$local_path)

# grab a list of available rasters from the MAP API
raster_meta <- as.data.frame(listRaster())

# subset to remove rows where title = "Relative Abundance Africa"
raster_meta <- raster_meta[!raster_meta$title == "Relative Abundance Africa", ]

# turn returned variable as a list to feed into sourcing function
raster_name_list <- as.list(raster_meta$title)

# feed list into getRaster function
raster_list <- lapply(raster_name_list, getRaster)

# stack the surfaces
# error with surfaces due to differing extents
# loop through and get a vector of raster extents
# extents <- t(sapply(raster_list, function (x) as.vector(extent(x))))
# 
# get unique extents across all rasters
# u_extents <- unique(extents)

# loop through surfaces and generate info for each country; can't stack as there's 43 unique extents
for(i in 1:length(raster_list)){
  
  # grab the raster for that iteration
  r_i <- raster_list[[i]]
  
  # mask it by the extent of the admin 0 raster
  r_i <- crop(r_i, admin0raster)
  
  # check number of columns match (cell size is equal)
  logic_cell <- r_i@ncols == admin0raster@ncols
  
  # if the number of cells isn't equal, resample 
  if(logic_cell == FALSE){
    
    message(paste0("Resampling raster ", i, " to align with master grid"))
    
    r_i <- resample(r_i, admin0raster, method = "bilinear")
    
  }
  
  # generate the mean pixel value for all pixels within a country
  poly_mean <- zonal(r_i, admin0raster, fun = 'mean')
  
  # create a dataframe with the zonal output
  names(poly_mean) <- c("country_id_raster",
                        names(raster_list[[i]]))
  
  poly_mean <- as.data.frame(poly_mean)
  
  # convert to a binary surface indicating values for a country
  poly_mean[[2]] <- ifelse(poly_mean[[2]] >0, 1, NA)
  
  # combine results across each raster
  if(i == 1){
    
    combined <- poly_mean
    
  } else {
    
    combined <- merge(combined, poly_mean, by = "country_id_raster")
    
  }
  
}

# add back ISO code 
iso3 <- as.data.frame(admin0shapefile[, 1:2])
names(combined)[1] <- "GAUL_CODE"
combined <- merge(combined, iso3, by = "GAUL_CODE")

# write out combined dataset
write.csv(combined,
          'combined_lookup.csv',
          row.names = FALSE)
