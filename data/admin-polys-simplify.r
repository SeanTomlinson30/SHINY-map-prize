# admin-polys-simplify.r
# andy south 2/3/2019

# script to create faster admin polygons file for malaria atlas project shiny viewer

# create one polygons object that has both admin0 and admin1 boundaries in it.
# make district lookups simpler with consistent codes
# save as an R object rather than a shapefile for faster loading
# move to using package sf the modern replacement for sp that makes looking at polygon attributes easier.
# simplify polygons so object is smaller and display is faster, country maps look cleaner (polygon detail not needed)


# get iso country ids for africa from existing shapefile (then it may not be needed further)
# this could be replaced by a different method
library(raster)
admin_1 <- raster::shapefile('data/districts/admin_1.shp')
country_ids <- unique(admin_1$COUNTRY_ID)

#get polygons for admin0 and admin1 from as a spatialpolygonsdataframe
spdf_africa <- malariaAtlas::getShp(ISO=country_ids) 
#convert to sf 
library(sf)
sf_africa <- sf::st_as_sf(spdf_africa)
#get rid of dodgy countrycodes
sf_africa <- sf_africa[sf_africa$country_id != 'XXX',]
#save as an R object that we can then read into the shiny app
#save(sf_africa, file='test.rda')
#then to plot the countries or districts just subset by admn_level
#ad0ago <- sf_africa[sf_africa$admn_level==0 & sf_africa$country_id=='AGO',]
#st_geometry needed to plot just the polygons
#plot(sf::st_geometry(ad0ago))

#sf_africa has spain in too !
#plot(sf::st_geometry(sf_africa))

#library(mapview)
#mapview(sf_africa)

#and has much more detail than we need e.g. fiddly islands.

#remove spain
sf_africa <- sf_africa[sf_africa$country_id!='ESP',]

# simplify
library(rmapshaper)
#default keeps 0.05 of points keep=0.05
#keep all polygons keep_shapes=TRUE
sf_afr_simp <- rmapshaper::ms_simplify(sf_africa, keep_shapes=TRUE)

save(sf_afr_simp, file='data/sf_afr_simp.rda')

library(mapview)
mapview(sf_afr_simp, zcol='country_id', legend=FALSE)

ggplot(sf_afr_simp) +
geom_sf(aes(fill=country_id))

# subset by country and then plot coloured named districts

#shorter name
sfafsi <- sf_afr_simp

#subset districts for one country
sfago <- sfafsi[sfafsi$admn_level==1 & sfafsi$country_id=='AGO',]

#datum=na needed to remove gridlines now, may not be in future

ggplot(sfago) +
geom_sf(aes(fill=name)) + 
coord_sf(datum = NA) + 
theme_void()

# if I wanted to add district labels
ggplot(sfafsi) +
geom_sf() +
geom_text_repel(aes(x=X, y=Y, label=name))
