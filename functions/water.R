
water <- function(path_to_DA, fresh, sea, city_name, save_file_path, tf = tempdir()){
  suppressWarnings(suppressMessages(library(sf)))
  # identify temporary folder
  print(tf)  
  
  # create folder in temp folder for the city (otherwise it will just dump everthing into a single folder)
  tc <- paste0(tf, "/", city_name, "/")
  print(tc)
  dir.create(tc)
  
  # read in files
  print("Step 1/8: read in each file")
  census <- sf::read_sf(path_to_DA) 
  water_fresh <- fresh
  water_salt <- sea
  
  # make sure census (DAs) and vector (water) are in the same projection - because the water is so large, we're going to
  # reproject the census to be the same as the water
  print("Step 2/8: transform CRS to be Statistics Canada NAD83 Lambert (ESPG code 4269)")
  census <- suppressWarnings(sf::st_transform(census, crs=4269))
  # vector <- sf::st_transform(vector, crs=3347)
  
  # crop building layer to the extent of the DA file to reduce toll on memory
  print("Step 3/8: crop file to extent")
  options(sf.st_crop.inform = FALSE)
  water_fresh <- sf::st_make_valid(water_fresh)
  water_salt <- sf::st_make_valid(water_salt)
  census <- sf::st_make_valid(census)
  
  water_fresh <- suppressWarnings(sf::st_crop(water_fresh, raster::extent(census)))
  water_salt <- suppressWarnings(sf::st_crop(water_salt, raster::extent(census)))
  
  water_salt <- sf::st_make_valid(water_salt)
  
  # combine water
  water <- rbind(water_fresh, water_salt)
  


  
  #sp::plot(census$geometry, add=T) # run if problem
  
  # make list of DAs to run loop on
  print("Step 4/8: create list of DAs")
  DAUID <- unique(census$DAUID)
  

  
  # begin building calculation loop
  #loop that actually runs the water summarization In short, it reads in each DA, crops the building to the DA file extent, calculates the DA area, building area, and buidling density. It also counts the number of buildings in the DA.
  print("Step 5/8: Starting loop")
  for (i in seq_along(DAUID)) {
    
    # filter single DA
    DA_select <- census %>% 
      filter(DAUID == DAUID[i])
    
    DA_select <- sf::st_transform(DA_select, crs=3347)
    water <- sf::st_transform(water, crs=3347)

    #
    # water_d <- sf::st_union(water, by="geometry") |> 
    #   sf::st_sf() |> 
    #   sf::st_make_valid()
    

    
    
    # distance to nearest water body
    DA_select_sp <- DA_select %>% 
      sf::as_Spatial()
    water_sp <- water |> 
      sf::as_Spatial()
    
    water_dist <-  rgeos::gDistance(DA_select_sp, water_sp)
    
    # count the number of water inside the DA
    #water <- sf::st_make_valid(water_d)

    water_within <- (sf::st_make_valid(sf::st_crop(water, DA_select)))
    
   # calculate area
      water_area <- sum(suppressWarnings(sf::st_area(water_within)))
      
  
      
  
    # bind areas of interest together
    out <- as.data.frame(cbind(DAUID[i], water_dist, water_area))
    
    # save temp file for DA (necessary because of tax on memory)
    save_loc <- paste0(tc, city_name, "_", i, ".shp")
    sf::write_sf(out, paste0(tc, "/",city_name, "_", i, ".shp"))
    
    # give update
    #print(paste('on postal code', i, 'of', length(DAUID), "---", save_loc))
    cat("\r", "on DA", i, 'of', length(DAUID))
    
  } # end building calculation loop
  
  
  # read in and combines temp files into single file for entire city of interest
  # function is found below
  print("Step 6/8: combine files into one for the city")
  files <- list.files(tc, full.names = T)
  x <- lapply(files, read_and_combine_dbf)
  combined_water <- do.call(rbind, x)
  

  # merge with DA shapefile
  print("Step 7/8: merging with DA shapefile")
  water <- merge(census, combined_water, by=c("DAUID"), all.x=T)
  water$water_area <- as.numeric(as.character(water$water_area))
  water$water_dist <- as.numeric(as.character(water$water_dist))
  
  # write file
  print("Step 8/8: saving shapefile")
  sf::st_write(water, paste0(save_file_path))
  
  return(water)
  
  
} # end of building function




#combine files into single file
read_and_combine_dbf <- function(path){
  
  dbf <- foreign::read.dbf(path)
  
  colnames(dbf) <- c("DAUID", "water_dist", "water_area")
  
  
  return(dbf)
} # end and combine function
