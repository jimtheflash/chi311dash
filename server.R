shiny::shinyServer(function(input, output) {
  #### filter for input to plots, tables ####
  # 311 info and aircraft noise complaints dominate 311 complaints and show up
  # mostly in airports or the 311 call center; these also aren't really services
  # that the city can do anything about, so sensible to remove
  selected_sr_types <- shiny::reactive({
    if (is.null(input$sr_type)) {
      selected_sr_types <- sr_vec
    } else {
      selected_sr_types <- as.character(input$sr_type)
    }
    if(input$highvolfilter == TRUE) {
      selected_sr_types <- selected_sr_types[!grepl('311|Aircraft',
                                                    selected_sr_types)]
    }
    selected_sr_types
  })
  filtered_data <- shiny::reactive({
    fd <- all_service_requests %>%
      dplyr::mutate(ca_num = community_area,
                    ca_factor = as.factor(community_area),
                    sr_factor = as.factor(sr_type)) %>%
      dplyr::filter(!is.na(created_date),
               !is.na(sr_type), 
               !is.na(community_area),
               !is.na(latitude),
               !is.na(longitude)) %>%
      dplyr::filter(sr_type %in% selected_sr_types()) %>%
      dplyr::filter(created_date >= input$daterange[[1]],
             closed_date <= input$daterange[[2]])
    if (input$openfilter == TRUE) {
      fd <- dplyr::filter(fd, !is.na(closed_date))
    }
    return(fd)
  })
  #### ca sr frequency summary ####
  ca_sr_freq <- shiny::reactive({
    ca_with_sr <- filtered_data() %>%
      dplyr::group_by(ca_factor, sr_factor, .drop = FALSE) %>%
      dplyr::summarise(ca_sr_total = n()) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(filtered_data() %>% 
                         dplyr::group_by(sr_factor) %>%
                         dplyr::summarise(sr_total = n()) %>%
                         dplyr::ungroup(), by = "sr_factor") %>%
      dplyr::left_join(filtered_data() %>%
                         dplyr::group_by(ca_factor) %>%
                         dplyr::summarise(ca_total = n()) %>%
                         dplyr::ungroup(), by = "ca_factor") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ca_num = as.numeric(as.character(ca_factor))) %>%
      dplyr::full_join(ca_lu, by = "ca_num")
  })
  #### ca sr freq table ####
  output$ca_sr_freq_table <- DT::renderDataTable({
    ca_sr_freq() %>%
      dplyr::inner_join(ca_lu) %>%
      dplyr::filter(sr_factor %in% selected_sr_types()) %>%
      dplyr::select('Community Area' = ca_name,
                    'Service Request Type' = sr_factor,
                    'Count' = ca_sr_total) %>%
      dplyr::arrange(`Service Request Type`, dplyr::desc(`Count`)) %>%
      DT::datatable()
  })
  #### map input ####
  map_input <- shiny::reactive({
    map_input <- areas %>%
      dplyr::select(area_numbe, community) %>%
      dplyr::mutate(ca_num = as.numeric(as.character(area_numbe))) %>%
      dplyr::left_join(ca_sr_freq() %>%
                         dplyr::group_by(ca_factor, .drop = FALSE) %>%
                         dplyr::summarise(
                           ca_num = max(ca_num),
                           selected_sr_total = sum(ca_sr_total, na.rm = FALSE)) %>%
                         dplyr::ungroup(),
                       by = 'ca_num')  %>%
      dplyr::mutate(popup = stringr::str_c("<strong>", community, "</strong>",
                                           "<br/>",
                                           "Service Requests: ", selected_sr_total) %>%
                      purrr::map(htmltools::HTML))
    map_input
    })
  #### chicago map ####
  output$chi_map <- leaflet::renderLeaflet({
    color_palette <- leaflet::colorNumeric("Blues",
                                            domain = map_input()$selected_sr_total)
    leaflet::leaflet(data = map_input(),
                     options = leaflet::leafletOptions(doubleClickZoom = FALSE,
                                                       dragging = FALSE,
                                                       zoomControl = FALSE),
                     height = 800,
                     width = 1500) %>%
      leaflet:: addPolygons(label = ~popup,
                            fillColor = ~color_palette(selected_sr_total),
                            color = "#444444",
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            fillOpacity = 0.75,
                            highlightOptions = highlightOptions(color = "white",
                                                                weight = 2,
                                                                bringToFront = TRUE))
    })
  #### time-series data ####
  ts_input <- shiny::reactive({
    filtered_data() %>%
      dplyr::mutate(Date = as.Date(created_date)) %>%
      dplyr::group_by(Date) %>%
      dplyr::summarise(service_requests = n()) %>%
      dplyr::ungroup()
  })
  #### time-series plot ####
  output$ts_plot <- plotly::renderPlotly({
    gg <- ggplot2::ggplot(data = ts_input(),
                      ggplot2::aes(x = Date, y = service_requests)) +
      ggplot2::geom_point(ggplot2::aes(text = paste0(Date, ': ', 
                                                     service_requests,
                                                     ' total service requests')),
                          alpha = .3, size = .9) +
      ggplot2::geom_smooth(ggplot2::aes(text = NULL), 
                           alpha = .5, color = 'black',
                           se = FALSE, linetype = 'dashed') +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::ggtitle('Selected Service Requests, All Community Areas',
                       subtitle = 'Each dot represents total service requests for a day') +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(face = 'italic'))
    plotly::ggplotly(gg, tooltip = 'text')
  })
})

  
  
