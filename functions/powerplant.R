

powerplant <- function(vector, census, city_name, shapefile_save_loc, tf = tempdir()){
  # identify temporary folder
  print(tf)  
  
  # create folder in temp folder for the city (otherwise it will just dump everthing into a single folder)
  tc <- paste0(tf, "/", city_name, "/")
  print(tc)
  dir.create(tc)
  
  # read in power plant and DA data
  vector <- sf::read_sf(vector)
  census <- sf::read_sf(census)
  
  # filter to fuel types of interest
  vector <- vector %>% 
    filter(fuel1 %in% c("Coal", "Gas", "Oil")) 
  
  # reproject to common crs
  census <- sf::st_transform(census, crs=sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  vector <- sf::st_transform(vector, crs=sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  # make dataset Spatial
  FT <- vector %>% 
    sf::as_Spatial(.)
  
  # identify unique DAUIDs to run loop on
  print("Step 4/8: create list of DAs")
  DAUID <- unique(census$DAUID)
  
  # begin building calculation loop
  #loop that actually runs the building summarization In short, it reads in each DA, crops the building to the DA file extent, calculates the DA area, building area, and buidling density. It also counts the number of buildings in the DA.
  print("Step 5/8: Starting loop")
  for (i in seq_along(DAUID)) {
    
    # filter single DA
    DA_select <- census %>% 
      filter(DAUID == DAUID[i])
    
    # make DA file Spatial
    DA <- sf::as_Spatial(DA_select)
    
    # find nearest powerplant (must by Spatial class)
    nearest <- rgeos::gDistance(DA, FT)
    
    if (i == 1) {
      out <- as.data.frame(cbind(DA_select$DAUID[i], nearest))
    } else {
      out1 <- as.data.frame(cbind(DA_select$DAUID, nearest))
      out <- rbind(out, out1)
      
    } # end ifelse
    
    # save temp file for DA (necessary because of tax on memory)
    #save_loc <- paste0(tc, city_name, "_", i, ".shp")
   
    # give update
    
    cat("\r", "on DA", i, 'of', length(DAUID))
    
  } # end loop
  
  # Save sf
  print("Step 6/8 : Writing shapefile")
  sf::write_sf(out, paste0(tc, city_name, "_", i, ".shp"))
  
  # merge with DA shapefile
  print("Step 7/8: merging with DA shapefile")
  colnames(out) <- c("DAUID", "nearest_pp")
  powerplant <- merge(census, out, by=c("DAUID"), all.x=T)
  
  # write file
  print("Step 8/8: saving shapefile")
  sf::st_write(powerplant, shapefile_save_loc)
  
  return(powerplant)

} # end function
  

