population <- read.csv("/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DA_2016_population/T1901EN.csv")
colnames(population) <- c("DAUID", "prov_eng", "prov_fre", "geocode_prov", "geocode_censusdivision", "geocode_censussubdivision", "pop_2016", "indiansettle", "nonprivate_dwelling", "privatedwelling", "landarea_sqkm", "popdensity_sqkm")


DA_list <- c("/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/toronto_fixed.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/montrealmetro.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Vancouver_DAs_Final/Vancouver_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Calgary_DAs_Final/Calgary_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Edmonton_DAs_Final/Edmonton_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Gatineau_DAs_Final/Gatineau_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Ottawa_DAs_Final/Ottawa_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Winnipeg_DAs_Final/Winnipeg_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Quebec-Levis_DAs_Final/Quebec-Levis_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Hamilton_Burlington_Grimsby_DAs_Final/Hamilton_Burlington_Grimsby_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/KWC_DAs_Final/KWC_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/London_DAs_Final/London_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Victoria_DAs_Final/Victoria_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Halifax_DAs_Final/HalifaxDA_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Oshawa_DAs_Final/Oshawa_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Windsor_DAs_Final/Windsor_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Saskatoon_DAs_Final/Saskatoon_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/StCath-Niagara_DAs_Final/StCath-Niagara_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Regina_DAs_Final/Regina_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/StJohns_DAs_Final/StJohns_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Kelowna_DAs_Final/Kelowna_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Barrie_DAs_Final/Barrie_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Sherbrooke_DAs_Final/Sherbrooke_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Guelph_DAs_Final/Guelph_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Abbotsford_DAs_Final/Abbotsford_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Kingston_DAs_Final/Kingston_DAs_Final.shp",
             #"/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Kanata_DAs_Final/Kanata_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/TroisRiv_DAs_Final/TroisRiv_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Moncton_DAs_Final/Moncton_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Chicoutimi_DAs_Final/Chicoutimi_DAs_Final.shp",
             "/Users/zoedavis/Documents/Projects/PHAC_EQI_2021/Data/DAs/Milton_DAs_Final/Milton_DAs_Final.shp"
             )


for (i in seq_along(DA_list)) {
  # read in ndvi file and merge
  
  DA <- sf::read_sf(DA_list[i])
  DA_pop_file <- merge(DA, population, by="DAUID", all.x=T)
  
  name <- stringr::str_split(DA_list[i], pattern = "/", n=10, simplify = T)
  name2 <- stringr::str_split(name[,9], pattern = "_", n=2, simplify = T)
  name3 <- name2[,1]
  
  # metrics
  num_DA <- length(unique(DA_pop_file$DAUID))
  DA_pop <- sum(as.numeric(DA_pop_file$pop_2016), na.rm = T)
  avg_pop_den <- mean(DA_pop_file$popdensity_sqkm, na.rm = T)
  #CSDNAMEs <- unique(DA_pop_file$CSDNAME)



  if (i == 1) {
    out <- as.data.frame(cbind(num_DA, DA_pop, avg_pop_den, name3))
  } else {
    out1 <-  as.data.frame(cbind(num_DA, DA_pop, avg_pop_den, name3))
    out <- rbind(out, out1)
    
  } # end ifelse
  
  cat("\r", "on DA", i, 'of', length(DA_list))
  
  
} # end loop

# read in one at at time to get names
x <- sf::read_sf(DA_list[31])
unique(x$CSDNAME)

# metrics
length(unique(x$DAUID))
sum(x$pop_2016)
mean(x$popdensity_sqkm)
unique(x$CSDNAME)
length(is.na(x$DAUID))

