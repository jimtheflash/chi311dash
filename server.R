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
                    sr_factor = as.factor(sr_type),
                    sr_uid = dplyr::if_else(parent_sr_number == '',
                                            sr_number,
                                            parent_sr_number)) %>%
      dplyr::filter(!is.na(created_date),
               !is.na(sr_type), 
               !is.na(community_area),
               !is.na(latitude),
               !is.na(longitude)) %>%
      dplyr::filter(sr_type %in% selected_sr_types()) %>%
      dplyr::filter(created_date >= input$daterange[[1]]) %>% 
      dplyr::filter((is.na(closed_date)|closed_date <= input$daterange[[2]]))
    if (input$openfilter == TRUE) {
      fd <- fd %>%
        dplyr::filter(!is.na(closed_date))
    }
    fd
  })
  #### ca sr frequency summary ####
  ca_sr_freq <- shiny::reactive({
    ca_with_sr <- filtered_data() %>%
      dplyr::group_by(ca_factor, sr_factor, .drop = FALSE) %>%
      dplyr::summarise(ca_sr_total = dplyr::n_distinct(sr_number),
                       unique_ca_sr_total = dplyr::n_distinct(sr_uid)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ca_num = as.numeric(as.character(ca_factor))) %>%
      dplyr::full_join(ca_lu, by = "ca_num")
  })
  #### ca sr freq table ####
  output$ca_sr_freq_table <- DT::renderDataTable({
    table_out <- ca_sr_freq() %>%
      dplyr::filter(sr_factor %in% selected_sr_types()) %>%
      dplyr::transmute('Community Area' = ca_name,
                    'Population' = population,
                    'Service Request Type' = sr_factor,
                    'Total Requests' = ca_sr_total,
                    'Requests Per 10K' = ca_sr_total / population * 10000) %>%
      dplyr::arrange(`Service Request Type`, dplyr::desc(`Total Requests`))
    
    if (input$groupbyca == TRUE) {
      table_out <- table_out %>%
        dplyr::group_by(`Community Area`) %>%
        dplyr::summarise('Population' = max(Population, na.rm = TRUE),
                         'Total Requests' = sum(`Total Requests`, na.rm = TRUE),
                         'Requests Per 10K' = sum(`Total Requests`, na.rm = TRUE) / max(Population, na.rm = TRUE) * 10000) %>%
        dplyr::arrange(dplyr::desc(`Total Requests`))
    }
     table_out %>%
      DT::datatable(filter = 'top',
                    rownames = FALSE, 
                    options = list(pageLength = 100)) %>%
      DT::formatRound(columns = 'Requests Per 10K', digits = 0) %>%
      DT::formatCurrency(columns = c('Population', 'Total Requests'),
                         currency = "", interval = 3, mark = ",", digits = 0)
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
                           selected_sr_total = sum(ca_sr_total, na.rm = FALSE),
                           selected_sr_per_10K = sum(ca_sr_total, na.rm = FALSE) / max(population, na.rm = TRUE) * 10000) %>%
                         dplyr::ungroup(),
                       by = 'ca_num')
    if (input$popcor == TRUE) {
      map_input <- map_input %>%
        dplyr::mutate(plot_val = selected_sr_per_10K,
                      popup = stringr::str_c("<strong>", community, "</strong>",
                                             "<br/>",
                                             "Service Requests Per 10K: ", round(selected_sr_per_10K)) %>%
                        purrr::map(htmltools::HTML))
    } else {
      map_input <- map_input %>%
        dplyr::mutate(plot_val = selected_sr_total,
                      popup = stringr::str_c("<strong>", community, "</strong>",
                                             "<br/>",
                                             "Service Requests: ", selected_sr_total) %>%
                        purrr::map(htmltools::HTML))
    }
    map_input
    })
  #### chicago map ####
  output$chi_map <- leaflet::renderLeaflet({
    color_palette <- leaflet::colorNumeric(palette = "Greens",
                                            domain = map_input()$plot_val)
    leaflet::leaflet(data = map_input(),
                     options = leaflet::leafletOptions(doubleClickZoom = FALSE,
                                                       dragging = FALSE,
                                                       maxZoom = 10,
                                                       minZoom = 10,
                                                       zoomControl = FALSE),
                     height = 800,
                     width = 1500) %>%
      leaflet:: addPolygons(label = ~popup,
                            fillColor = ~color_palette(plot_val),
                            color = "#444444",
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            fillOpacity = 0.75,
                            highlightOptions = highlightOptions(color = "white",
                                                                weight = 2,
                                                                bringToFront = TRUE)) %>%
      leaflet::addLegend(pal = color_palette, 
                         values = map_input()$plot_val,
                         position = "bottomleft",
                         opacity = 1)
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
                                                     service_requests)),
                          alpha = .3, size = .9) +
      ggplot2::geom_smooth(ggplot2::aes(text = NULL), 
                           alpha = .5, color = 'black',
                           se = FALSE, linetype = 'dashed') +
      ggplot2::xlab("Date Service Request Created") +
      ggplot2::ylab("Total Selected Service Requests") +
      ggplot2::theme_minimal()
    plotly::ggplotly(gg, tooltip = 'text')
  })
})

  
  
