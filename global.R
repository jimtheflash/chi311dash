# library(RSocrata)
library(leaflet)
library(sf)
library(shiny)
library(tidyverse)

# t1 <- Sys.time()
# endpoint <- 'https://data.cityofchicago.org/resource/v6vf-nfxy.csv'
# app_tkn <- 'fYewJkZwzNk00T2RfyMgamtHR'
# data_from_get <- RSocrata::read.socrata(url = endpoint,
#                                         app_token = app_tkn)
# t2 <- Sys.time()
# t2 - t1

# app data should filter out observations that can't be filtered or plotted
app_data <- data_from_get %>%
  filter(!is.na(created_date),
         !is.na(sr_type), 
         !is.na(community_area),
         !is.na(latitude),
         !is.na(longitude))
# community area shapes
areas <- 
  sf::st_read(
    'C:/Users/Jim/Documents/chi_311_dash/geo_export_ffc838ed-75e3-4215-b35d-25ca80a32070.shp')
# vector of service request types
sr_vec <- sort(unique(app_data$sr_type))
