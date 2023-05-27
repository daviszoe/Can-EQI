

uv <- function(raster, census, shapefile_save_loc){
  
  # read in UV data
  print("Step 1/6: reading in interpolated UV data")
  raster <- raster::raster(raster)

  
  # read in DA information
  print("Step 2/6: reading in census")
  census <- sf::read_sf(census)
  
  # reproject to common crs
  print("Step 3/6: reprojecting")
  census <- sf::st_transform(census, raster::crs(raster))
  #raster <- raster::projectRaster(raster, crs=sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  

  # spatial join to nearest featuer - identifies the nearest point and assigns that information to the DA
  print("Step 4/6: clip to boundaries")
  # make extent
  ex <- raster::extent(census)
  
  # clip to extent
  uv_clip <- raster::crop(raster, ex, snap="out")


  # extract mean value within each DA level
  print("starting step 5/6: extracting mean value for each DA boundary")
  raster_DA <- raster::extract(uv_clip, census, fun=mean, df = T, exact = T, sp=T, na.rm=T)
  raster_DA <- sf::st_as_sf(raster_DA)
  
  raster_DA <- sf::st_transform(raster_DA, crs=sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  # write file
  print("Step 6/6: saving shapefile")
  sf::st_write(raster_DA, shapefile_save_loc, append = F)
  
  return(raster_DA)
  
} # end function
