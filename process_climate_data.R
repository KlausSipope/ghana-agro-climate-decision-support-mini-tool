### Name: Klaus Sipope
### Description: Preprocessing of climatology data for visualization

# Load libraries
library(dplyr)

# Sources
source("get_nasa_power_data.R")

get_rainfall_df <- function(district_name) {
  climate_df <- get_parameters(district_name)
  
  climate_dekad_df <- climate_df %>% 
    mutate(
      dekad_in_month =  case_when(
        DD <= 10 ~ 1L,
        DD <= 20 ~ 2L,
        TRUE     ~ 3L
      ),
      dekad = (MM - 1) * 3 + dekad_in_month,
      dekad_label = paste0("dekad ", dekad)
    ) %>% 
    select(-dekad_in_month)
  
  rainfall_dekad_df <- climate_dekad_df %>%
    group_by(YEAR, dekad) %>% 
    summarise(total_rainfall = sum(PRECTOTCORR))
  
  rainfall_df <- rainfall_dekad_df %>% 
    group_by(dekad) %>% 
    summarise(mean_rainfall = mean(total_rainfall))
  
  return(rainfall_df)
}

get_temperature_df <- function(district_name) {
  climate_df <- get_parameters(district_name)
  
  monthly_temperature_df <- climate_df %>%
    group_by(MM, YEAR) %>% 
    summarise(
      monthly_mean_t2min = mean(T2M_MIN),
      monthly_mean_t2max = mean(T2M_MAX))
  
  temperature_df <- monthly_temperature_df %>% 
    group_by(MM) %>% 
    summarise(
      mean_t2min = mean(monthly_mean_t2min),
      mean_t2max = mean(monthly_mean_t2max))
  
  return(temperature_df)
}

