no2 <- function(raster, census, shapefile_save_location) {
  
  # read in raster
  print("starting step 1/7: reading in raster")
  raster <- raster::stack(raster)
  
  # read in boundary
  print("starting step 2/7: done reading in DA boundaries") 
  census <- sf::read_sf(census) 
  
  # reproject census to match raster
  print("starting step 3/7: reprojecting census to match raster")
  census <- sf::st_transform(census, crs= sp::CRS("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  
  # crop
  print("starting step 4/7: cropping raster to match census extent")
  raster <- raster::crop(raster, raster::extent(census))
  
  # mask raster with boundary
  print("starting step 5/7: making raster to match census boundary")
  raster <- raster::mask(raster, census)
  
  #rename layer
  names(raster) <- c("no2")
  
  # extract mean value within each DA level
  print("starting step 6/7: extracting mean value for each DA boundary")
  raster_DA <- raster::extract(raster, census, fun=mean, df = T, exact = T, sp=T, na.rm=T)
  raster_DA <- sf::st_as_sf(raster_DA)
  
  # make consistent crs
  # print("starting step 3/7: reproject raster to match other projec")
  # raster <- raster::projectRaster(raster, crs= sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  # 
  
  # save raster to specified location
  print("starting step 7/7: saving shapefile to specified location")
  sf::write_sf(raster_DA, shapefile_save_location)
  
  return(raster_DA)
  
}
