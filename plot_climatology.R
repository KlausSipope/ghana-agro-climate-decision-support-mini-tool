### Name: Klaus Sipope
### Description: Climatology visualization 

library(dplyr)
library(purrr)
library(highcharter)
source("get_nasa_power_data.R")
source("process_climate_data.R")

# Rainfall visualization function
rainfall_visualization <- function(district_name){
  dekad_labels <- c(
    "Jan 1–10",  "Jan 11–20",  "Jan 21–31",
    "Feb 1–10",  "Feb 11–20",  "Feb 21–28/29",
    "Mar 1–10",  "Mar 11–20",  "Mar 21–31",
    "Apr 1–10",  "Apr 11–20",  "Apr 21–30",
    "May 1–10",  "May 11–20",  "May 21–31",
    "Jun 1–10",  "Jun 11–20",  "Jun 21–30",
    "Jul 1–10",  "Jul 11–20",  "Jul 21–31",
    "Aug 1–10",  "Aug 11–20",  "Aug 21–31",
    "Sep 1–10",  "Sep 11–20",  "Sep 21–30",
    "Oct 1–10",  "Oct 11–20",  "Oct 21–31",
    "Nov 1–10",  "Nov 11–20",  "Nov 21–30",
    "Dec 1–10",  "Dec 11–20",  "Dec 21–31"
  )
  rainfall_df <- process_rainfall_df(district_name)
  rainfall_df$dekad_label <- dekad_labels
  
  series_data <- map2(
    rainfall_climatology$mean_rainfall,
    rainfall_climatology$dekad_label,
    ~ list(y = .x, name = .y)
  )
  
  highchart() %>%
    hc_chart(type = "area") %>%
    hc_title(text = sprintf("Rainfall Climatology (10-Year Average) – %s", district_name)) %>%
    
    hc_xAxis(
      categories = axis_labels,   # Your month-only axis
      tickmarkPlacement = "on"
    ) %>%
    
    hc_yAxis(title = list(text = "Mean rainfall per dekad (mm)")) %>%
    
    hc_add_series(
      name = "Rainfall",
      data = series_data,
      color = "#1E90FF",
      fillOpacity = 0.5
    ) %>%
    
    hc_plotOptions(area = list(
      marker = list(enabled = FALSE)
    )) %>%
    
    hc_tooltip(
      headerFormat = "",
      pointFormat = "<b>dekad:</b> {point.name}<br> <b>Rainfall:</b> {point.y:.2f}mm"
    )
  
}

# Temperature visualization function
temperature_visualization <- function(district_name){
  temperature_df <- process_temperature_df(district_name)
  
  highchart() %>%
    hc_chart(type = "spline") %>%    # smooth line chart (no fill)
    hc_title(text = sprintf("Temperature Climatology (10-Year Average) – %s", district_name)) %>%
    
    hc_xAxis(
      categories = month_labels,
      tickmarkPlacement = "on"
    ) %>%
    
    hc_yAxis(
      title = list(text = "Mean temperature (°C)")
    ) %>%
    
    # Max temperature (orange line)
    hc_add_series(
      name  = "Mean Tmax",
      data  = temperature_df$mean_t2max,
      color = "#FFA500"              
    ) %>%
    
    # Min temperature (red line)
    hc_add_series(
      name  = "Mean Tmin",
      data  = temperature_df$mean_t2min,
      color = "#D62728"              
    ) %>%
    
    hc_tooltip(
      shared = TRUE,
      headerFormat = "<b>{point.key}</b><br>",
      pointFormat = "<span style='color:{series.color}'>{series.name}</span>: <b>{point.y:.2f} °C</b><br/>"
    ) %>% 
    
    hc_plotOptions(
      series = list(
      marker = list(enabled = FALSE)
        )
    )
  
}

