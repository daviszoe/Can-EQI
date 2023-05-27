
#NDVI


file_list <- c(
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/toronto/toronto_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/montreal/montrealmetro_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/vancouver/vancouver_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/calgary/calgary_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/edmonton/edmonton_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/gatineau/gatineau_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/ottawa/ottawa_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/winnipeg/winnipeg_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/quebec/quebec_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/hamilton/hamilton_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/kitchener/kitchener_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/london/london_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/victoria/victoria_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/halifax/halifax_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/oshawa/oshawa_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/windsor/windsor_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/saskatoon/sask_ndvi.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/stcatharines_niagara/stcatharines_niagara_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/regina/regina_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/stjohns/stjohns_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/kelowna/kelowna_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/barrie/barrie_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/sherbrooke/sherbrooke_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/guelph/guelph_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/abbotsford/abbotsford_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/kingston/kingston_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/troisrivieres/troisrivieres_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/moncton/monc_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/saguenay/saguenay_NDVI.shp",
  "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/milton/milton_NDVI.shp"
  )


read_and_combine_shapefiles <- function(path){
  
  shp <- sf::read_sf(path)
  shp <- shp %>% 
    dplyr::select("DAUID", "PRNAME", "CDNAME", "CSDNAME", "CMANAME", "X2014", "X2015", "X2016", "X2017", "X2018", "geometry")
 
  # assign name for EQI city
  name <- stringr::str_split(path, pattern = "/", simplify = T)
  name <- name[,9]
  shp$EQI_city <- name
  
  return(shp)
}

x <- lapply(file_list, read_and_combine_shapefiles)
combined_ndvi <- do.call(rbind, x)


# reorganize
combined_ndvi <- combined_ndvi %>% 
  pivot_longer(cols = c("X2014", "X2015", "X2016", "X2017", "X2018"), names_to = "year", values_to="NDVI") %>% 
  mutate(year = case_when(year =="X2014" ~ as.numeric(2014),
                          year =="X2015" ~ as.numeric(2015),
                          year =="X2016" ~ as.numeric(2016),
                          year =="X2017" ~ as.numeric(2017),
                          year =="X2018" ~ as.numeric(2018)))


sf::write_sf(combined_ndvi, "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/Derived/combined/combined_ndvi.shp" )

# plot
combined_ndvi %>% 
  ggplot() +
  geom_histogram(aes(x=NDVI)) +
  facet_wrap(.~ EQI_city)

