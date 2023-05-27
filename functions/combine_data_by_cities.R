

combine_data_by_cities <- function(path_to_city_folder, census, shapefile_save_loc) {
  
  print("Step 1/8 : Set up")
  # list of possible column names for each of the exposure types
  poss_col <- c("DAUID", 
                "DA_area", 
                "mn_cld_", # cold mean temp for each DA
                "d_DA___", # difference in cold temp
                "X2014", 
                "X2015", 
                "X2016", 
                "X2017", 
                "X2018", 
                "no2",
                "nearest_pp", 
                "rd_lngt",
                "num_nds",
                "men_tmp", #hot mean temp
                "d_DAtm_", # difference in cold temp
                "m_d_do_sl", 
                "water_dist", 
                "water_area")
  
  # list of files for each city
  city <- list.files(path_to_city_folder, pattern = ".shp", full.names = T)
  
  
  # read in census
  census <- sf::read_sf(census)
  census <- census %>% 
    select(DAUID, PRNAME, CDNAME, CSDNAME)
  
  print("Step 2/8 : being loop")
  for (x in seq_along(city)) {
    
    print("Step 3/8 : read in files")
    # read in exposure file
    file <- sf::read_sf(city[x])
    
    print("Step 4/8 : selecting columns and dropping geometry")
    # select just the files columns that would contain important information
    file <- file %>% 
      select(any_of(poss_col)) 
    
    # remove geometry
    file <- file %>% 
      sf::st_drop_geometry()
    

    print("Step 5/8 : merge to census/DAUID geometry")

    
    print("Step 6/8 : merge to final form")
    if (x == 1) {
      
      final <- file
    } else {
      
      final <- merge(final, file, by = "DAUID", all.x=T)
    } #end if else 
    
    cat("\r", "on file", x, 'of', length(city))
    
  } # end loop
  
   
   colnames(final) <- c("DAUID", "DA_area", "cold_temp", "d_c_temp",
                      "ndvi_2014", "ndvi_2015", "ndvi_2016", "ndvi_2017", "ndvi_2018", "no2", "pm25_2014", 
                      "pm25_2015", "pm25_2016", "pm25_2017", "pm25_2018", "near_pp", "rd_lngt", "num_nds",
                      "hot_temp", "d_h_temp", "m_d_do_sl", "wtr_dist", "wtr_area")
   
   
   
  out <- merge(census, final, by="DAUID", all.x=T)
  
  
  print ("7/8 : rename columns to be more descriptive")
  
  sf_save <- paste0(shapefile_save_loc, ".shp")
 
  print("Step 8/8: saving shapefile")
  sf::st_write(out, sf_save, append = F)
  
  return(out)
  
} # end function
