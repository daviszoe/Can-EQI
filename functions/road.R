


road <- function(vector, census, city_name, shapefile_save_loc, tf = tempdir()){
  # identify temporary folder
  print(tf)  
  
  # create folder in temp folder for the city (otherwise it will just dump everthing into a single folder)
  tc <- paste0(tf, "/", city_name, "/")
  print(tc)
  dir.create(tc)
  
  
  print("Step 1/4: reading in files")
  census <- sf::st_read(census)
  #vector <- sf::st_read(vector)
  vector <- vector
  
  vector <- sf::st_crop(vector, raster::extent(census))
  

  # identify unique DAUIDs to run loop on
  print("Step 2/4: create list of DAs")
  DAUID <- unique(census$DAUID)
  
  # begin building calculation loop
  #loop that actually runs the building summarization In short, it reads in each DA, crops the building to the DA file extent, calculates the DA area, building area, and buidling density. It also counts the number of buildings in the DA.
  print("Step 3/4: Starting loop")
  for (i in seq_along(DAUID)) {
    

    # identify single DA
    c <- census %>% 
      filter(DAUID == DAUID[i])
    
    # add 1 metre buffer to reduce edge effects
    d <- sf::st_buffer(c, 1)
    
    options(dplyr.summarise.inform = FALSE)
    # clip roads to DA boundary
    road_clip <- sf::st_intersection(d, vector, tolerance = 1)
    
    # length of roads in DA
    road_clip$road_length <- sf::st_length(road_clip)
    
    # save road length 
    # this works by summing all the road lengths for each type of road and then filtering for just highways (Rank 1-4)
    # and then outputting the sum of that
    sum <- road_clip %>% 
      group_by(RANK) %>% 
      summarise(road_length = sum(road_length)) %>% 
      sf::st_drop_geometry() %>% 
      filter(RANK %in% c(1,2,3,4)) %>% 
      summarise(road_length = sum(road_length))
    
    c$road_length <- sum$road_length
    
    # count number of intersections
    nodes <- road_clip %>% 
      filter(RANK %in% c(1,2,3,4)) 
    
    # if there are no rows in the nodes file (because there's no roads) this will check the dim and skip it
    if (dim(nodes)[1] == 0) {
      node_num = 0
    } else {
      node_num <-  length(sf::st_intersection(nodes))
    }
     
    
    c$num_nodes <- node_num
    
    c <- c %>% 
      select("DAUID", "road_length", "num_nodes") %>% 
      sf::st_drop_geometry()
    
    # # loop to do for all DAs
    # if (i == 1) {
    #   out <- c
    # } else {
    #   out <- rbind(out, c)
    #   
    # } # end ifelse
    
    # save temp file for DA (necessary because of tax on memory)
    save_loc <- paste0(tc,city_name, "_", i, ".shp")
    sf::write_sf(c, paste0(tc, city_name, "_", i, ".shp"))
    
    # give update
    cat("\r", "on DA", i, 'of', length(DAUID))
    
  } # end loop
  
  #read in and combines temp files into single file for entire city of interest
  # function is found below
  print("Step 6/8: combine files into one for the city")
  files <- list.files(tc, full.names = T)
  x <- lapply(files, read_and_combine_dbf)
  combined_roads <- do.call(rbind, x)
  
  # merge with DA shapefile
  print("Step 7/8: merging with DA shapefile")
  roads <- merge(census, combined_roads, by=c("DAUID"), all.x=T)
  
  # write file
  print("Step 8/8: saving shapefile")
  sf::st_write(roads, paste0(shapefile_save_loc))
  
  return(roads)
  
} # end function



#combine files into single file
read_and_combine_dbf <- function(path){
  
  dbf <- foreign::read.dbf(path)
  
  colnames(dbf) <- c("DAUID", "road_length","num_nodes")
  
  
  return(dbf)
} # end and combine function

