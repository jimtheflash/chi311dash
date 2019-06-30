shiny::shinyUI(
  shiny::fluidPage(
    shiny::titlePanel("Chicago 311 Service Requests"),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 4,
                    shiny::dateRangeInput(inputId = 'daterange',
                                          label = 'Select Date Range: ',
                                          min = min(app_data$created_date),
                                          max = Sys.Date(),
                                          start = min(app_data$created_date),
                                          end = Sys.Date()))),
    shiny::br(),
    shiny::tabsetPanel(type = "pills",
      shiny::tabPanel("Map",
                      shiny::br(),
                      shiny::fluidRow(
                        shiny::column(width = 6,
                                      shiny::selectInput(inputId = 'sr_type',
                                                         label = "Select Service Request Type: ",
                                                         choices = sr_vec[!grepl("311|Aircraft", sr_vec)],
                                                         multiple = TRUE,
                                                         selectize = TRUE))),
                      shiny::fluidRow(
                        shiny::column(width = 6,
                                      leaflet::leafletOutput(outputId = 'chi_map')))),
      shiny::tabPanel("Data Explorer",
                      shiny::br(),
                      shiny::fluidRow(
                        shiny::column(width = 6,
                                      shiny::selectInput(inputId = 'tabletype',
                                                         label = 'Select Summary Table: ',
                                                         choices = c('Service Requests',
                                                                     'Service Requests x Community Areas'),
                                                         selected = 'Service Requests'))),
                      shiny::fluidRow(
                        shiny::column(width = 8,
                                      DT::dataTableOutput(outputId = 'summary_table')))))))
