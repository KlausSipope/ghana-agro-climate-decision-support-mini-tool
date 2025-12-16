### Name: Klaus Sipope
### Description: Create a function that fetches climate data from NASA POWER

# Libraries
library(sf)
library(dplyr)
library(nasapower)

districts <- st_read("ghana-shapefile/gadm41_GHA_2.shp")

# Get parameters from API call on NASAPOWER
get_parameters<- function(district_name) {
  coordinates <- districts %>% 
    filter(NAME_2 == district_name) %>% 
    st_centroid() %>%
    st_coordinates()
  
  output <- get_power(
    community = "AG",
    lonlat = coordinates,
    pars = c("T2M_MIN", "T2M_MAX", "PRECTOTCORR"),
    dates = c("2015-01-01", "2024-12-31"),
    temporal_api = "daily"
  )
  
  return(output)
}

