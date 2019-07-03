shiny::shinyUI(
  shiny::fluidPage(
    tags$head(tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))),
    shiny::titlePanel("Chicago 311 Service Requests"),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 6,
                    shiny::selectInput(inputId = 'sr_type',
                                       label = "Select Service Request Type: ",
                                       choices = sr_vec,
                                       multiple = TRUE,
                                       selectize = TRUE)),
      shiny::column(width = 4,
                    shiny::dateRangeInput(inputId = 'daterange',
                                          label = 'Select Date Range: ',
                                          min = min(all_service_requests$created_date),
                                          max = Sys.Date(),
                                          start = as.Date(Sys.Date() - 90),
                                          end = Sys.Date()))),
    shiny::fluidRow(
      shiny::column(width = 10,
                    shiny::checkboxInput(inputId = 'highvolfilter',
                                         label = 'Remove 311 Info Requests and Aircraft Noise Complaints',
                                         value = TRUE,
                                         width = '100%'))),
    shiny::br(),
    shiny::tabsetPanel(type = "pills",
      shiny::tabPanel("Map",
                      shiny::br(),
                      shiny::fluidRow(
                        shiny::column(width = 6,
                                      leaflet::leafletOutput(outputId = 'chi_map')),
                        shiny::column(width = 6,
                                      shiny::plotOutput(outputId = 'ts_plot')))),
      shiny::tabPanel("Data Explorer",
                      shiny::br(),
                      shiny::fluidRow(
                        shiny::column(width = 8,
                                      DT::dataTableOutput(outputId = 'ca_sr_freq_table')))))))
