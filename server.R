shiny::shinyServer(function(input, output) {
  #### filter for input to plots, tables ####
  # 311 info and aircraft noise complaints dominate 311 complaints and show up
  # mostly in airports or the 311 call center; these also aren't really services
  # that the city can do anything about, so sensible to remove
  filtered_data <- shiny::reactive({
    app_data %>%
      filter(!(sr_type %in% c('311 INFORMATION ONLY CALL',
                              'Aircraft Noise Complaint'))) %>%
      filter(created_date >= input$daterange[[1]],
             closed_date <= input$daterange[[2]])
  })
  #### ca sr frequency summary ####
  ca_sr_freq <- shiny::reactive({
    table_input <- filtered_data() %>%
      dplyr::mutate(ca_factor = as.factor(community_area),
                    sr_factor = as.factor(sr_type))
    
    table_input %>%
      dplyr::group_by(ca_factor, sr_factor, .drop = FALSE) %>%
      dplyr::summarise(ca_sr_total = n()) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(table_input %>% 
                         dplyr::group_by(sr_factor) %>%
                         dplyr::summarise(sr_total = n()) %>%
                         dplyr::ungroup(), by = "sr_factor") %>%
      dplyr::left_join(table_input %>%
                         dplyr::group_by(ca_factor) %>%
                         dplyr::summarise(ca_total = n()) %>%
                         dplyr::ungroup(), by = "ca_factor") %>%
      dplyr::mutate(ca_num = as.numeric(as.character(ca_factor)))
  })
  #### map input ####
  map_input <- shiny::reactive({
    areas %>%
      dplyr::select(area_numbe, community) %>%
      dplyr::mutate(ca_num = as.numeric(as.character(area_numbe))) %>%
      dplyr::left_join(ca_sr_freq() %>%
                         dplyr::filter(as.character(sr_factor) %in% input$sr_type) %>%
                         dplyr::group_by(ca_factor) %>%
                         dplyr::summarise(
                           ca_num = max(ca_num),
                           selected_sr_total = sum(ca_sr_total, na.rm = TRUE)) %>%
                         dplyr::ungroup(), 
                       by = 'ca_num')
    })
  #### chicago map ####
  output$chi_map <- leaflet::renderLeaflet({
    color_palette <- leaflet::colorBin("YlOrRd", domain = map_input()$selected_sr_total)
    leaflet::leaflet(data = map_input()) %>%
      leaflet:: addPolygons(label = ~community,
                            fillColor = ~color_palette(selected_sr_total),
                            color = "#444444",
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            fillOpacity = 0.5,
                            highlightOptions = highlightOptions(color = "white",
                                                                weight = 2,
                                                                bringToFront = TRUE)) %>%
      leaflet::addTiles()
    })
})

  
  
