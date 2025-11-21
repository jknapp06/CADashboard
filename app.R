# A shiny app for exploring the CADashboard datasets

library(shiny)
library(tidyverse)
library(DT)

# Load all datasets created in the CADashboard project
data_files <- list.files("data", pattern = "*.csv", full.names = TRUE)
datasets <- lapply(data_files, read_csv)
names(datasets) <- tools::file_path_sans_ext(basename(data_files))
dataset_names <- names(datasets)
# Define UI
ui <- fluidPage(
  titlePanel("CADashboard Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", choices = dataset_names),
      # Add filters for year, county, district, school
      # These will be dynamically generated based on the selected dataset
      uiOutput("filters_ui"),
      # Add a button to load table with applied filters
      actionButton("load_data", "Load Data")
    ),
    mainPanel(
      # Datatable output to display the filtered data
      DT::DTOutput("data_table")
    )
  )
)
# Define server logic
server <- function(input, output) {
  # Reactive expression to filter data based on inputs
  output$filters_ui <- renderUI({
    req(input$dataset)
    data <- datasets[[input$dataset]]
    ui_elements <- list()
    if ("reportingyear" %in% colnames(data)) {
      ui_elements <- c(
        ui_elements,
        selectInput(
          "year_filter",
          "Select Year:",
          choices = unique(data$year),
          selected = unique(data$year),
          multiple = TRUE
        )
      )
    }
    if ("countyname" %in% colnames(data)) {
      ui_elements <- c(
        ui_elements,
        selectInput(
          "county_filter",
          "Select County:",
          choices = unique(data$county),
          selected = unique(data$county),
          multiple = TRUE
        )
      )
    }
    if ("districtname" %in% colnames(data)) {
      ui_elements <- c(
        ui_elements,
        selectInput(
          "district_filter",
          "Select District:",
          choices = unique(data$district),
          selected = unique(data$district),
          multiple = TRUE
        )
      )
    }
    if ("schoolname" %in% colnames(data)) {
      ui_elements <- c(
        ui_elements,
        selectInput(
          "school_filter",
          "Select School:",
          choices = unique(data$school),
          selected = unique(data$school),
          multiple = TRUE
        )
      )
    }
    do.call(tagList, ui_elements)
  })

  # Load data when the button is clicked
  load_data <- observeEvent(input$load_data, {
    dashboard_data()
  })

  dashboard_data <- reactive({
    # Load and filter data based on inputs

    req(input$dataset)
    data <- datasets[[input$dataset]]
    if (!is.null(input$year_filter)) {
      data <- data %>% filter(reportingyear %in% input$year_filter)
    }
    if (!is.null(input$county_filter)) {
      data <- data %>% filter(countyname %in% input$county_filter)
    }
    if (!is.null(input$district_filter)) {
      data <- data %>% filter(districtname %in% input$district_filter)
    }
    if (!is.null(input$school_filter)) {
      data <- data %>% filter(schoolname %in% input$school_filter)
    }
    data
  })

  output$data_table <- renderDT({
    req(dashboard_data())
    DT::datatable(load_data(), options = list(pageLength = 25, scrollX = TRUE))
  })
}
# Run the application
shinyApp(ui = ui, server = server)
