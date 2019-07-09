shiny::shinyUI(
  shiny::fluidPage(
    tags$head(tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))),
    shiny::titlePanel("Chicago 311 Service Requests"),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 5,
                    shiny::selectInput(inputId = 'sr_type',
                                       label = "Select Service Request Type: ",
                                       choices = sr_vec,
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       width = '90%')),
      shiny::column(width = 4,
                    shiny::dateRangeInput(inputId = 'daterange',
                                          label = 'Select Date Range: ',
                                          min = min(all_service_requests$created_date),
                                          max = Sys.Date(),
                                          start = as.Date(Sys.Date() - 90),
                                          end = Sys.Date()))),
    shiny::fluidRow(
      shiny::column(width = 4,
                    shiny::checkboxInput(inputId = 'highvolfilter',
                                         label = 'Remove 311 Info Requests and Aircraft Noise Complaints',
                                         value = TRUE,
                                         width = '100%'),
                    shiny::checkboxInput(inputId = 'openfilter',
                                         label = 'Remove Open Service Requests',
                                         value = FALSE))),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 5,
                    shiny::h4("Community Areas"),
                    leaflet::leafletOutput(outputId = 'chi_map')),
      shiny::column(width = 6,
                    shiny::h4("Selected Service Requests Over Time"),
                    plotly::plotlyOutput(outputId = 'ts_plot'))),
    shiny::br(),
    shiny::h3("Explore Data"),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 8,
                    shiny::checkboxInput(inputId = 'groupbyca',
                                         label = 'Sum All Service Request Types',
                                         value = TRUE),
                    DT::dataTableOutput(outputId = 'ca_sr_freq_table')))))

                      