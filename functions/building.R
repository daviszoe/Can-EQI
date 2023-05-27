
building <- function(path_to_DA, path_to_building, city_name, save_file_path, tf = tempdir()){
  
  # identify temporary folder
  print(tf)  
  
  # create folder in temp folder for the city (otherwise it will just dump everthing into a single folder)
  tc <- paste0(tf, "/", city_name, "/")
  print(tc)
  dir.create(tc)
  
  # read in files
  print("Step 1/8: read in each file")
  census <- sf::read_sf(path_to_DA) 
  vector <- sf::read_sf(path_to_building)
  
  # make sure census (DAs) and vector (building) are in the same projection
  print("Step 2/8: transform CRS to be Statistics Canada NAD83 Lambert (ESPG code 3347)")
  census <- sf::st_transform(census, crs=3347)
  vector <- sf::st_transform(vector, crs=3347)
  
  # crop building layer to the extent of the DA file to reduce toll on memory
  print("Step 3/8: crop file to extent")
  vector <- sf::st_crop(vector, raster::extent(census))
  #sp::plot(vector$geometry) # run if problem
  
  # make list of DAs to run loop on
  print("Step 4/8: create list of DAs")
  DAUID <- unique(census$DAUID)
  
  # begin building calculation loop
  #loop that actually runs the building summarization In short, it reads in each DA, crops the building to the DA file extent, calculates the DA area, building area, and buidling density. It also counts the number of buildings in the DA.
  print("Step 5/8: Starting loop")
  for (i in seq_along(DAUID)) {
   
     # filter single DA
    DA_select <- census %>% 
      filter(DAUID == DAUID[i])
   
     # crop building file to DA boundary
    building_crop <- sf::st_crop(vector, raster::extent(DA_select))
    
    # count the number of buildings inside the DA
    building_count <- lengths(sf::st_intersects(DA_select, building_crop))
    
    # calculate the area of the DA, area of the buidlings, and building density
    building_area <- sum(sf::st_area(building_crop))
    DA_area <- sum(sf::st_area(DA_select))
    building_density <- building_area/DA_area
    
    # bind areas of interest together
    out <- as.data.frame(cbind(DAUID[i], DA_area, building_count, building_density))
    
    # save temp file for DA (necessary because of tax on memory)
    save_loc <- paste0(tc, city_name, "_", i, ".shp")
    sf::write_sf(out, paste0(tc, city_name, "_", i, ".shp"))
    
    # give update
    #print(paste('on postal code', i, 'of', length(DAUID), "---", save_loc))
    cat("\r", "on DA", i, 'of', length(DAUID))
    
  } # end building calculation loop
  
 
 # read in and combines temp files into single file for entire city of interest
 # function is found below
 print("Step 6/8: combine files into one for the city")
  files <- list.files(tc, full.names = T)
  x <- lapply(files, read_and_combine_dbf)
  combined_building <- do.call(rbind, x)
  
  
  
  # merge with DA shapefile
  print("Step 7/8: merging with DA shapefile")
  building <- merge(census, combined_building, by=c("DAUID"), all.x=T)
  
  # write file
  print("Step 8/8: saving shapefile")
  sf::st_write(building, paste0(save_file_path))
  
  return(building)
  

} # end of building function




#combine files into single file
read_and_combine_dbf <- function(path){
  
  dbf <- foreign::read.dbf(path)
  
  colnames(dbf) <- c("DAUID", "DA_area","building_count", "building_density")
  
  
  return(dbf)
} # end and combine function
  