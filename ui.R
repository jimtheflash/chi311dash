shiny::shinyUI(
  shiny::fluidPage(
    tags$head(tags$style(shiny::HTML("
    .panel-default {background: #FFFFFF;
                    border: none;
                    border-color: #FFFFFF;
                    box-shadow: none;}
    .panel > .panel-heading {background-color: white;
                             border-color: white;
                             color: black;}
    .leaflet-container {background: #FFFFFF;}
    .leaflet .info {background: #FFFFFF; 
                    box-shadow: none;}"))),
    shiny::titlePanel("Chicago 311 Service Requests"),
    shiny::em("First, select service requests and time frame, and any additional filters."),
    shiny::br(),
    shiny::em("Next, navigate to the tab of interest. You can view a map summarizing service request counts by neighborhood, a time series, or explore the data on your own."),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 4,
                    shiny::selectInput(inputId = 'sr_type',
                                       label = shiny::h3("Select Service Request Type"),
                                       choices = sr_vec,
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       width = '90%')),
      shiny::column(width = 4,
                    shiny::dateRangeInput(inputId = 'daterange',
                                          label = shiny::h3("Select Date Range"),
                                          min = min(all_service_requests$created_date),
                                          max = Sys.Date(),
                                          start = as.Date(Sys.Date() - 90),
                                          end = Sys.Date()))),
    shiny::fluidRow(
      shiny::column(width = 4,
        shinyBS::bsCollapsePanel(shiny::em(shiny::HTML("<u>Additional Filters</u>")),
                   shinyWidgets::prettyCheckbox(inputId = 'highvolfilter',
                                        label = 'Remove 311 Info Requests and Aircraft Noise Complaints',
                                        icon = shiny::icon("check"),
                                        status = "default",
                                        value = TRUE,
                                        width = '90%'),
                   shinyWidgets::prettyCheckbox(inputId = 'popcor',
                                        label = 'Adjust Count of Service Requests For Community Area Population',
                                        icon = shiny::icon("check"),
                                        status = "default",
                                        value = TRUE,
                                        width = '100%'),
                   shinyWidgets::prettyCheckbox(inputId = 'openfilter',
                                        label = 'Remove Open Service Requests',
                                        icon = shiny::icon("check"),
                                        status = "default",
                                        value = FALSE)))),
    shiny::fluidRow(
      shiny::column(width = 5,
                    shiny::h3("Community Areas"),
                    leaflet::leafletOutput(outputId = 'chi_map')),
      shiny::column(width = 6,
                    shiny::h3("Selected Service Requests Over Time"),
                    plotly::plotlyOutput(outputId = 'ts_plot'))),
    shiny::br(),
    shiny::h3("Explore Data"),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 11,
                    shiny::checkboxInput(inputId = 'groupbyca',
                                         label = 'Sum All Service Request Types',
                                         value = TRUE),
                    DT::dataTableOutput(outputId = 'ca_sr_freq_table')))))

                      