title_text <- "Chicago 311 Service Requests"
shiny::shinyUI(
  shiny::fluidPage(
    tags$head(tags$style(shiny::HTML("
    a {color: gray;}
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
    shiny::titlePanel(windowTitle = title_text,
      title=shiny::div(
        shiny::img(src="Flag_of_Chicago,_Illinois.png",
                   height=100,
                   width=150),
        title_text)),
    shiny::em("First, select service requests and time frame, and any additional filters."),
    shiny::br(),
    shiny::em("Next, navigate to the tab of interest. You can view a map summarizing service request counts by neighborhood, a time series, or explore the data on your own."),
    shiny::br(),
    shiny::hr(),
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
        shinyBS::bsCollapsePanel(shiny::HTML("<u>Click For Additional Filters</u>"),
                   shinyWidgets::prettyCheckbox(inputId = 'highvolfilter',
                                        label = 'Remove 311 Info Requests and Aircraft Noise Complaints',
                                        icon = shiny::icon("check"),
                                        status = "default",
                                        value = TRUE,
                                        width = '90%'),
                   shinyWidgets::prettyCheckbox(inputId = 'openfilter',
                                        label = 'Remove Open Service Requests',
                                        icon = shiny::icon("check"),
                                        status = "default",
                                        value = FALSE)))),
    shiny::tabsetPanel(
      shiny::tabPanel(shiny::strong("Visualizations"),
        shiny::fluidRow(
          shiny::column(width = 4,
                        shiny::h3("Service Requests By Neighborhood"),
                        shinyWidgets::prettyCheckbox(inputId = 'popcor',
                                                     label = 'Correct For Neighborhood Population',
                                                     icon = shiny::icon("check"),
                                                     status = "default",
                                                     value = FALSE,
                                                     width = '100%'),
                        leaflet::leafletOutput(outputId = 'chi_map')),
          shiny::column(width = 4,
                        shiny::h3("Selected Service Requests Over Time"),
                        plotly::plotlyOutput(outputId = 'ts_plot')))),
      shiny::tabPanel(shiny::strong("Explore Data"),
        shiny::fluidRow(
          shiny::column(width = 10,
                        shiny::br(),
                        shinyWidgets::prettyCheckbox(inputId = 'groupbyca',
                                             label = 'Sum All Service Request Types',
                                             icon = shiny::icon("check"),
                                             status = "default",
                                             value = TRUE),
                        DT::dataTableOutput(outputId = 'ca_sr_freq_table')))))
    )
  
  )




                      