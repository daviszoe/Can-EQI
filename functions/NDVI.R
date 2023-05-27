
NDVI <- function(raster, census, shapefile_save_location) {
  
  # read in raster
  print("starting step 1/7: done reading in raster")
  raster1 <- raster::stack(raster) # read in raster
  years <- c("X2014", "X2015", "X2016", "X2017", "X2018") # identify bands/years of interest
  raster_years <- which(names(raster1) %in% years) # select only those bands
  
  raster <- raster::stack(raster, bands = raster_years) # read in raster with only those bads

  
   # read in boundary
  print("starting step 2/7: done reading in DA boundaries") 
  census <- sf::read_sf(census) 
 
  # make consistent crs
  print("starting step 3/7: reprojecting census (DA) layer and raster to same CRS")
  raster <- raster::projectRaster(raster, crs=sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  census <- sf::st_transform(census, crs=3347)
  
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

