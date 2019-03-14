# download-rasters.r
# andy south March 2019

# to download and save rasters for Africa from Malaria Atlas Project
# doesn't need to be run by the shiny app, run beforehand to save data

# needs packages declared in server.r

# seemingly had to convert sf back to sp to get bbox in format wanted by getRaster
extent_afr <- extent(c(-20,52), c(-38,38))
extent_afr <- bbox(extent_afr)

pfpr2_10_2015 <- getRaster(surface = "Plasmodium falciparum PR2-10", extent=extent_afr, year = 2015)
pf_incidence_2015 <- getRaster(surface = "Plasmodium falciparum Incidence", extent=extent_afr, year = 2015)
itn_2015 <- getRaster(surface = "Insecticide-treated bednet (ITN) coverage", extent=extent_afr, year = 2015)
time_to_city_2015 <- getRaster(surface = "A global map of travel time to cities to assess inequalities in accessibility in 2015", extent=extent_afr)

# could sample to reduce num pixels
# because this is all that are displayed by leaflet anyway
maxpixels <- 500000
time_to_city_2015 <- raster::sampleRegular(time_to_city_2015, maxpixels, asRaster = TRUE, useGDAL = TRUE)

#to make sure rasters are in memory
pfpr2_10_2015 <- readAll(pfpr2_10_2015)
pf_incidence_2015 <- readAll(pf_incidence_2015)
itn_2015 <- readAll(itn_2015)
time_to_city_2015 <- readAll(time_to_city_2015)

save(pfpr2_10_2015, file='data/rasters/pfpr2_10_2015.rda')
save(pf_incidence_2015, file='data/rasters/pf_incidence_2015.rda')
save(itn_2015, file='data/rasters/itn_2015.rda')
save(time_to_city_2015, file='data/rasters/time_to_city_2015.rda')
