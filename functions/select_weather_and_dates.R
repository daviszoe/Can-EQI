
# function to download weather station data
weather_stations <- function(city_name, prov){
  
  # select weather stations that have hourly data for the 5-year time period of interest
  x <- weathercan::stations_search(name = city_name, interval = "day", dist=20) |> 
    filter(as.numeric(start) <=2014 & as.numeric(end) >= 2018) 
  
  # list of stations
  station_list <- x$station_id
  print(station_list)
  
  # download data for all the stations in the list between the dates of interest. Takes a little while
  weather <- weathercan::weather_dl(station_ids = station_list, 
                                    start = "2014-01-01", 
                                    end = "2018-12-31",
                                    interval = "hour") |> 
    filter(prov == prov) |> 
    select(station_name, station_id, prov, lat, lon, elev, climate_id, date, time, year, month, day, hour, hmdx, rel_hum, temp, temp_dew, 
           wind_chill, wind_spd)
  
  return(weather)
  
}



humid <- function(weather_data) {
  # weather data from weather_stations
   weather <- weather_data
   
   # select humid days
   humid <- weather |> 
     filter(hmdx >= 30) |> 
     group_by(date) |> 
     count()
   humid$type <- "hmdx_30"
   
   humid2 <- weather |> 
     filter(hmdx >= 35) |> 
     group_by(date) |> 
     count()
   humid2$type <- "hmdx_35"
   
   humid3 <- rbind(humid, humid2)
   
   # make the dae a year and calculate if there is consective dates
   humid3$year <- lubridate::year(humid3$date)
   humid3$consecutive <- c(NA,diff(as.Date(humid3$date))==1)
   return(humid3)
  
}



mean_temperature_values <- function(weather_data) {
  # temperature data from weather_stations
  weather <- weather_data

  # calculate the daily maximum temperature, minimum temperature, minimum humidex, maximum humidex, and the rolling temperature average for
  # every day and weather station
  y <- weather |> 
    group_by(station_id, date) |> 
    drop_na(temp) |> 
    summarize(daily_max = max(as.numeric(temp), na.rm=T),
           daily_min = min(as.numeric(temp), na.rm=T),
           humidex_min = min(as.numeric(hmdx)),
           humidex_max = max(as.numeric(hmdx))) |> 
    group_by(station_id) |> 
    # rowwise() |> 
    mutate(mean_max_temp_3day = zoo::rollmean(daily_max, k=3, fill = NA, na.rm=T),
           mean_min_temp_3day = zoo::rollmean(daily_min, k=3, fill = NA, na.rm=T))
         
  return(y)
  
} # end function



cold <- function(weather_data) {
  # weather data from weather_stations
  weather <- weather_data
  
  # select humid days
  cold <- weather |> 
    filter(wind_chill <= -20) |> 
    group_by(date) |> 
    count()
  cold$type <- "20-below"
  
  cold2 <- weather |> 
    filter(wind_chill <= -15) |> 
    group_by(date) |> 
    count()
  cold2$type <- "15-below"
  
  cold3 <- weather |> 
    filter(wind_chill <= -10) |> 
    group_by(date) |> 
    count()
  cold3$type <- "10-below"
  
  cold5 <- weather |> 
    filter(wind_chill <= -5) |> 
    group_by(date) |> 
    count()
  cold5$type <- "5-below"
  
  cold4 <- rbind(cold, cold2, cold3, cold5)
  
  # make the dae a year and calculate if there is consective dates
  cold4$year <- lubridate::year(cold4$date)
  cold4$consecutive <- c(NA,diff(as.Date(cold4$date))==1)
  return(cold4)
  
}
