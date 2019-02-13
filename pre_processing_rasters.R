# script to process MAP obtained raster files
pacman::p_load(raster, rgdal)

# load in lookup for raster paths
raster_lookup <- read.csv('data/raster_paths.csv',
                          stringsAsFactors = FALSE)

# read in a district raster
districts <- raster('data/getRaster/admin1_raster.flt')
districts_shp <- shapefile('data/districts/admin_1.shp')

raster_lookup$stats_path <- NA

# loop through each available surface, and generate district level statistics
for(i in 1:length(raster_lookup$path)){
  
  # get the path for the raster
  raster_path <- raster_lookup$path[[i]]
  
  # if the path isn't blank, read in the raster and generate zonal statistics
  if(raster_path != ""){
    
    # source raster
    raster_i <- raster(raster_path)
    
    # crop 'districts' by extent of raster_i
    districts_c <- crop(districts, raster_i)
    raster_i <- crop(raster_i, districts_c)
    
    # if the resolution of the two rasters isn't the same, resample the input raster to match the 5km district raster
    if(ncol(raster_i) != ncol(districts_c)){
      
      raster_i <- resample(raster_i, districts_c, method = "bilinear")
      
    }
    
    # generate statistics
    raster_i_mean <- zonal(raster_i, districts_c, fun = 'mean')
    raster_i_min <- zonal(raster_i, districts_c, fun = 'min')
    raster_i_max <- zonal(raster_i, districts_c, fun = 'max')
    raster_i_sd <- zonal(raster_i, districts_c, fun = "sd")
    
    # merge into one dataframe
    raster_i_stats <- merge(raster_i_mean, raster_i_max, by = "zone")
    raster_i_stats <- merge(raster_i_stats, raster_i_min, by = "zone")
    raster_i_stats <- merge(raster_i_stats, raster_i_sd, by = "zone")
    
    # write out the merged dataframe
    # define a file name
    file_name_i <- substr(raster_path, 16, 1000)
    file_name_i <- gsub("\\.tiff", "_stats.csv", file_name_i)
    
    write.csv(raster_i_stats,
              file = paste0("data/processed/", file_name_i),
              row.names = FALSE)
    
    # add the filepath back into the raster_lookup dataframe
    raster_lookup$stats_path[i] <- paste0("data/processed/", file_name_i)
  }
  
}

# write out the updated filepath document
write.csv(raster_lookup,
          'data/raster_stats_paths.csv',
          row.names = FALSE)
