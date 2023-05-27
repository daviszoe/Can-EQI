
reorganize_by_year <- function(path_to_combined_files, shapefile_save_folder) {
  
  files <- fs::dir_ls(path_to_combined_files, regexp = "shp$")
  
  for (i in seq_along(files)) {
    n = files[i]
 
    # read in file
    file <- sf::read_sf(n)
    
    # colnames(file) <- c("DAUID", "DA_area", "cold_temp", "d_c_temp",
    #                      "ndvi_2014", "ndvi_2015", "ndvi_2016", "ndvi_2017", "ndvi_2018", "no2", "pm25_2014", 
    #                      "pm25_2015", "pm25_2016", "pm25_2017", "pm25_2018", "near_pp", "rd_lngt", "num_nds",
    #                      "hot_temp", "d_h_temp", "m_d_do_sl", "wtr_dist", "wtr_area")
    
    # identify city name
    city_name <- fs::path_file(files[i]) |> 
      fs::path_ext_remove()
    
    # pivot longer and wider to create a yearly dataset
    f <- file %>% 
      mutate(DA_area          = as.numeric(DA_area),
             cold_temp        = as.numeric(cold_temp), 
             d_c_temp         = as.numeric(d_c_temp),
             hot_temp         = as.numeric(hot_temp),
             d_h_temp        = as.numeric(d_h_temp),
             ndvi_2014        = as.numeric(ndvi_2014),
             ndvi_2015        = as.numeric(ndvi_2015),
             ndvi_2016        = as.numeric(ndvi_2016),
             ndvi_2017        = as.numeric(ndvi_2017),
             ndvi_2018        = as.numeric(ndvi_2018),
             no2              = as.numeric(no2),
             pm25_2014        = as.numeric(pm25_2014),
             pm25_2015        = as.numeric(pm25_2015),
             pm25_2016        = as.numeric(pm25_2016),
             pm25_2017        = as.numeric(pm25_2017),
             pm25_2018        = as.numeric(pm25_2018),
             near_pp          = as.numeric(near_pp),
             rd_lngt          = as.numeric(rd_lngt),
             num_nds          = as.numeric(num_nds),
             m_d_do_sl        = as.numeric(m_d_do_sl),
             wtr_dist         = as.numeric(wtr_dist),
             wtr_area         = as.numeric(wtr_area)
             ) |> 
      pivot_longer(cols = c(ndvi_2014, ndvi_2015, ndvi_2016, ndvi_2017, ndvi_2018, pm25_2014, pm25_2015, pm25_2016, pm25_2017, pm25_2018),
                   names_to = c("type","year"),
                   names_sep = "_",
                   values_to = "value") %>% 
      pivot_wider(
        names_from = "type",
        values_from = "value")
    
    # make spatial again
    f <- sf::st_as_sf(f)
    
    f$city_name <- city_name
    
    # save in EQI folder
    sf::st_write(f, paste0(shapefile_save_folder, city_name, ".shp"), append=F, quiet = T)
    
    cat("\r", "on file", i, 'of', length(files))
    
  } # end loop
  
} # end function
