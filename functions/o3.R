
o3 <- function(raster, census, shapefile_save_location) {
  
  # read in raster
  print("starting step 1/7: reading in raster")
  raster <- raster::stack(raster)

  # read in boundary
  print("starting step 2/7: done reading in DA boundaries") 
  census <- sf::read_sf(census) 
  
  # make consistent crs
  print("starting step 3/7: reprojecting census (DA) layer and raster to same CRS")
  raster <- raster::projectRaster(raster, crs= sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  census <- sf::st_transform(census, crs= sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # crop
  print("starting step 4/7: cropping raster to match census extent")
  raster <- raster::crop(raster, raster::extent(census))
  
  # mask raster with boundary
  print("starting step 5/7: making raster to match census boundary")
  raster <- raster::mask(raster, census)
  
  
  # extract mean value within each DA level
  print("starting step 6/7: extracting mean value for each DA boundary")
  raster_DA <- raster::extract(raster, census, fun=mean, df = T, exact = T, sp=T, na.rm=T)
  raster_DA <- sf::st_as_sf(raster_DA)
  
  # save raster to specified location
  print("starting step 7/7: saving shapefile to specified location")
  sf::write_sf(raster_DA, shapefile_save_location)
  
  return(raster_DA)
  
}
  

