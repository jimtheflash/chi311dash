library(leaflet)
library(plotly)
library(sf)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
# import::from(magrittr, '%>%')

# sr data
all_service_requests <- readRDS('data/311_service_requests.rds')
# vector of service request types
sr_vec <- sort(unique(all_service_requests$sr_type))
# community area shapes
areas <- sf::st_read(
    'data/geo_export_ffc838ed-75e3-4215-b35d-25ca80a32070.shp')
# ca populations
pops <- readr::read_csv('data/chi_ca_population.csv')
ca_lu <- data.frame(ca_num = as.numeric(as.character(areas$area_numbe)),
                    ca_name = as.character(areas$community),
                    stringsAsFactors = FALSE) %>%
  dplyr::inner_join(pops, by = 'ca_num')
