#

library(shiny)
library(tidyverse)
library(vroom)
library(DT)
library(plotly)
# library(sf)
# library(leaflet)

options(scipen = 999) # so CDS isn't changed in scientific notation
dashboard <- vroom("app_data/dashboard_essa.csv")
teachers <- vroom("app_data/teacher_assignments.csv")

# cds_check <- teachers |>
#   select(
#     cds,
#     county_code,
#     district_code,
#     school_code,
#     countyname,
#     districtname,
#     schoolname
#   ) |>
#   mutate(cds = as.character(cds))

# districts_geo <- read_sf("data/solano_districts.geojson")
# schools_geo <- read_sf("data/solano_schools.geojson")

dashboard <-
  dashboard |>
  mutate(
    statuslevel = as_factor(statuslevel),
    reportingyear = fct_relevel(as_factor(reportingyear), "2019"),
    indicator = fct_relevel(
      as_factor(indicator),
      "ELA",
      "Math",
      "ELPI",
      "absenteeism",
      "graduation",
      "suspension",
      "college/career",
      "science"
    ),
    studentgroup = as_factor(studentgroup),
    student_group_long = fct_relevel(
      as_factor(student_group_long),
      "All students",
      "Black/African American",
      "American Indian or Alaska Native",
      "Asian",
      "Filipino",
      "Hispanic",
      "Pacific Islander",
      "White",
      "Multiple Races/Two or more",
      "Socioeconomically Disadvantaged",
      "Homeless Youth",
      "Students with Disabilities",
      "Foster Youth",
      "English Learner",
      "English Learners Only",
      "Long-Term English Learner",
      "RFEPs Only",
      "English Only",
      "Smarter Balanced Assessment",
      "CA Alternative Assessment"
    ),
    priority_eligible = as_factor(priority_eligible),
    indicator_eligible = as_factor(indicator_eligible),
    charter_flag = if_else(is.na(charter_flag), "N", charter_flag),
    color = as_factor(color)
  )

ca_overall <-
  dashboard |>
  filter(rtype == "X")

years <-
  dashboard |>
  select(reportingyear) |>
  distinct()

counties <-
  dashboard |>
  select(countyname) |>
  distinct()

charters <-
  dashboard |>
  filter(charter_flag == "Y") |>
  select(cds, countyname, schoolname) |>
  distinct()

districts <-
  dashboard |>
  filter(rtype == "D") |>
  select(cds, countyname, districtname) |>
  distinct()

leas <-
  bind_rows(
    select(districts, cds, countyname, leaname = "districtname"),
    select(charters, cds, countyname, leaname = "schoolname")
  ) |>
  drop_na()

schools <-
  dashboard |>
  select(
    cds,
    reportingyear,
    countyname,
    districtname,
    schoolname,
    charter_flag,
    rtype
  ) |>
  distinct()

student_groups <-
  dashboard |>
  select(studentgroup, student_group_long) |>
  distinct()

indicators <-
  dashboard |>
  pull(indicator) |>
  unique()

status_colors <- c(
  "lightgrey",
  "tomato",
  "orange",
  "yellow",
  "mediumseagreen",
  "dodgerblue",
  "darkslategrey"
)

solano_csi <-
  dashboard |>
  select(
    reportingyear,
    countyname,
    districtname,
    schoolname,
    charter_flag,
    student_group_long,
    starts_with("assistance_status"),
    atsi_support
  ) |>
  filter(reportingyear == "2025", countyname == "Solano") |>
  mutate(
    student_group_wrap = str_wrap(student_group_long, 25),
    atsi_support = if_else(atsi_support == 1, "Eligible", "Not Eligible")
  ) |>
  pivot_longer(
    cols = c(
      assistance_status2018,
      assistance_status2019,
      assistance_status2020,
      assistance_status2021,
      assistance_status2022,
      assistance_status2023,
      assistance_status2024,
      assistance_status2025
    ),
    names_prefix = "assistance_status",
    names_to = "year",
    values_to = "csi_assistance_status"
  ) |>
  drop_na(csi_assistance_status) |>
  distinct()

#########################
#                       #
#        UI             #
#                       #
#########################
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "lumen"),
  header = fluidRow(
    column(4, img(height = 130, width = 248, src = "SCOE_Logo.jpg")),
    column(8, fluidRow(h4("Internal use only.", align = "center"))),
  ),

  # Application title
  titlePanel("CA Dashboard    "),

  tabPanel(
    "District Priorities",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "year",
          label = "Select Reporting Year",
          choices = years$reportingyear,
          selected = "2025"
        ),
        # selectInput(
        #   "county",
        #   label = "Select County",
        #   choices = counties$countyname,
        #   selected = "Solano"
        # ),
        selectInput(
          "lea",
          label = "Select LEA",
          choices = NULL,
          selected = NULL
        ),
        selectInput(
          "school",
          label = "Select School",
          choices = NULL,
          selected = NULL
        ),
        h5("The light blue line shows the average for all students in CA."),
        img(height = 300, width = 200, src = "dashboard_key.png"),
        br(),
        uiOutput("da_note"),
        br(),
        a(
          href = "https://www.cde.ca.gov/ta/aC/cm/documents/howcolorsdetermine.pdf",
          "What do the colors mean?"
        ),
        br(),
        a(href = "https://solanocoe.shinyapps.io/CAASPP/", "CAASPP Details"),
        br(),
        a(
          href = "https://solanocoe.shinyapps.io/absenteeism/",
          "Absenteeism Details"
        ),
        br(),
        uiOutput("dashboard_link"),
        br(),
        htmlOutput("teacher_assignments"),
      ),

      mainPanel(
        tabsetPanel(
          tabPanel(
            "Status View",
            htmlOutput("indicator_title"),
            plotlyOutput("dashboard_plotly", height = "100%", width = "100%")
          ),
          tabPanel(
            "Change View",
            htmlOutput("change_title"),
            plotlyOutput("change_plotly", height = "100%", width = "100%")
          ),
          tabPanel("Detail View", DT::dataTableOutput("detail_table"))
        )
      )
    )
  ),

  tabPanel(
    "DA Eligibility",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "da_year",
          label = "Select Reporting Year",
          choices = years$reportingyear,
          selected = "2025"
        ),
        # selectInput(
        #   "da_county",
        #   label = "Select County",
        #   choices = counties$countyname,
        #   selected = "Solano"
        # ),
        checkboxGroupInput(
          "da_districts",
          label = "Select Districts/LEAs",
          choices = NULL,
          selected = NULL
        ),
        checkboxInput(
          "include_charters",
          label = "Include Charters",
          value = FALSE
        ),
        checkboxGroupInput(
          "da_groups",
          label = "Student Groups",
          choices = student_groups$student_group_long,
          selected = student_groups$student_group_long
        )
      ),

      mainPanel(
        h2("Student Groups Eligible for DA"),
        DT::dataTableOutput("county_da_table")
      )
    )
  ),
  tabPanel(
    "ESSA Eligibility",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "essa_districts",
          choices = unique(solano_csi$districtname),
          selected = unique(solano_csi$districtname),
          label = "District"
        ),
        selectInput(
          "essa_charters",
          label = "Include charter schools?",
          choices = c("All schools", "No charters", "Only charters"),
          selected = "All schools"
        )
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("CSI", plotOutput("csi_plot")),
          tabPanel("ATSI", plotOutput("atsi_plot"))
        )
      )
    )
  )
  # ------------------
  # ,
  #   tabPanel("Map",
  #          sidebarLayout(
  #            sidebarPanel(
  #              selectInput("indicator",
  #                          "Select Indicator",
  #                          choices = indicators),
  #              sliderInput("color_range",
  #                          "Choose color range",
  #                          min = 0,
  #                          max = 5,
  #                          step = 1,
  #                          value = c(0, 5)),
  #              selectInput("group",
  #                          "Select Student Group",
  #                          choices = student_groups,
  #                          selected = "All students")
  #            ),
  #            mainPanel(
  #              leafletOutput("solanoIndicatorMap", height = 800)
  #            )
  #          )
  # )
)

#######################################
#                                     #
#              Server                 #
#                                     #
#######################################
server <- function(input, output) {
  # District Priorities ----------

  dashboard_county <- reactive(
    # input$county
    "Solano"
  )

  lea_options <- reactive(
    leas |>
      filter(countyname == dashboard_county())
  )

  observeEvent(lea_options(), {
    updateSelectInput(
      inputId = "lea",
      choices = lea_options()$leaname,
      selected = lea_options()$leaname[1]
    )
  })

  dashboard_year <- reactive(
    input$year
  )

  dashboard_lea <- reactive(
    input$lea
  )

  school_options <- reactive(
    schools |>
      filter(
        reportingyear == dashboard_year(),
        countyname == dashboard_county(),
        (districtname == dashboard_lea() & charter_flag == "N") |
          (schoolname == dashboard_lea())
      )
  )

  observeEvent(school_options(), {
    updateSelectInput(
      inputId = "school",
      choices = school_options()$schoolname,
      selected = school_options()$schoolname[1]
    )
  })

  dashboard_school <- reactive(
    input$school
  )

  dashboard_cds <- reactive(
    schools |>
      filter(
        districtname == dashboard_lea(),
        schoolname == dashboard_school()
      ) |>
      distinct() |>
      pull(cds) |>
      nth(1)
  )

  selected_aggregate <- reactive(
    schools |>
      filter(cds == dashboard_cds()) |>
      pull(rtype) |>
      nth(1)
  )

  dashboard_filtered <- reactive(
    dashboard |>
      filter(reportingyear == dashboard_year())
  )

  dashboard_graphable <- reactive(
    dashboard_filtered() |>
      filter(
        countyname == dashboard_county(),
        districtname == dashboard_lea() |
          schoolname == dashboard_lea(),
        schoolname == dashboard_school()
      )
  )

  dashboard_cds <- reactive(
    dashboard_graphable() |>
      pull(cds) |>
      nth(1) |>
      as.character()
  )

  ca_aggregate <- reactive(
    dashboard_filtered() |>
      filter(rtype == "X", studentgroup == "ALL")
  )

  output$da_note <- renderUI(
    {
      ifelse(
        dashboard_lea() == dashboard_school(),
        "Service for differentiated assistance is based on two years of eligibilty for charter schools. Eligbility indicated here only refers to current year selected.",
        ""
      )
    }
  )

  output$dashboard_link <- renderUI(
    {
      dashboard_url <- paste0(
        "https://www.caschooldashboard.org/reports/",
        dashboard_cds(),
        "/",
        dashboard_year()
      )
      a(href = dashboard_url, "CA Dashboard (CDE)")
    }
  )

  dashboard_validate <- reactive(
    {
      validate(
        need(
          sum(!is.na(dashboard_graphable()$currstatus)) >= 1,
          "There was an error with your selection. Use the link to the CDE Dashboard page to investigate.
           Please choose a different school or year."
        )
      )
      dashboard_graphable()
    }
  )

  lea_title <- reactive(
    ifelse(
      dashboard_lea() == dashboard_school(),
      dashboard_school(),
      paste(dashboard_lea(), dashboard_school())
    )
  )

  output$indicator_title <- renderText(
    paste(
      "<h3>Dashboard Indicator Overview for\n",
      lea_title(),
      dashboard_year(),
      "</h3>"
    )
  )

  teacher_clear_percent <- reactive(
    if (selected_aggregate() == "D") {
      teachers |>
        filter(
          cds == dashboard_cds(),
          dass == "All",
          charter_school == "No",
          school_grade_span == "ALL",
          teacher_experience_level == "ALL",
          teacher_credential_level == "ALL",
          subject_area == "TA"
        ) |>
        distinct() |>
        pull(clear_fte_percent) |>
        nth(1)
    } else if (selected_aggregate() == "S") {
      teachers |>
        filter(
          cds == dashboard_cds(),
          teacher_experience_level == "ALL",
          teacher_credential_level == "ALL",
          subject_area == "TA"
        ) |>
        pull(clear_fte_percent) |>
        nth(1)
    }
  )

  output$teacher_assignments <- renderText(
    {
      # Print teacher clear percent if available
      if (!is.na(teacher_clear_percent())) {
        paste0(
          "Percentage of teachers with clear credentials: <h5>",
          round(teacher_clear_percent(), 1),
          "%</h5>(Data from 2023-24 Academic year)"
        )
      } else {
        "Teacher assignment data not available for this LEA/year."
      }
    }
  )

  # Col Plot of each LEA (ggplot) -- current status in each indicator. ------
  output$dashboard_plot <- renderPlot(
    height = 1200,
    res = 96,
    {
      dashboard_validate() |>
        ggplot() +
        geom_col(
          mapping = aes(
            x = student_group_long,
            y = currstatus,
            fill = color,
            color = indicator_eligible
          ),
          size = 1
        ) +
        geom_point(
          mapping = aes(x = student_group_long, y = priorstatus),
          shape = 10, # Shape for prior year 3 == "+"
          size = 4,
          color = "mediumpurple"
        ) +
        geom_text(
          mapping = aes(
            x = student_group_long,
            y = currstatus / 2,
            label = studentgroup
          ),
          color = "black",
          size = 3
        ) +
        scale_fill_manual(
          drop = FALSE,
          values = status_colors
        ) +
        scale_color_manual(
          values = c("FALSE" = NA, "TRUE" = "darkred"),
          drop = FALSE,
          na.value = "transparent",
          labels = c(
            "TRUE" = "Indicator Eligible for DA",
            "FALSE" = "Not eligible"
          )
        ) +
        geom_hline(yintercept = 0, color = "slategrey") +
        geom_hline(
          data = ca_aggregate(),
          aes(yintercept = currstatus),
          color = "lightskyblue"
        ) +
        facet_wrap(~indicator, ncol = 1, scales = "free_y") +
        labs(
          title = paste(
            "Dashboard Indicators for\n",
            lea_title(),
            dashboard_year()
          ),
          x = "Student Group",
          y = "Current Status",
          fill = "Color/Status",
          color = "DA Eligibility",
          caption = "Purple symbol indicates previous year status.
          Blue line shows the aggregate status for all students in CA."
        ) +
        theme_minimal() +
        theme(
          title = element_text(size = 25),
          axis.text.x = element_text(
            size = 12,
            vjust = 1,
            hjust = 1,
            angle = 45
          ),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          strip.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.caption = element_text(size = 12)
        )
    }
  )

  # Col Plot of each LEA (ggplot + plotly) -- current status in each indicator. --------
  output$dashboard_plotly <- renderPlotly(
    {
      dashboard_all_indicators <-
        dashboard_validate() |>
        ggplot() +
        geom_col(
          mapping = aes(
            x = student_group_long,
            y = currstatus,
            fill = color,
            color = indicator_eligible,
            text = paste0(
              student_group_long,
              "\n",
              "Current status: ",
              currstatus,
              "\n",
              "Change: ",
              change,
              "\nn: ",
              currdenom
            )
          ),
          linewidth = 1
        ) +
        geom_point(
          mapping = aes(
            x = student_group_long,
            y = priorstatus,
            text = paste0(
              student_group_long,
              "\n",
              "prior status: ",
              priorstatus,
              "\n",
              "Change: ",
              change
            )
          ),
          shape = 10, # Shape for prior year 3 == "+"
          size = 4,
          color = "orchid"
        ) +
        geom_text(
          mapping = aes(
            x = student_group_long,
            y = currstatus / 2,
            label = studentgroup
          ),
          color = "black",
          size = 3
        ) +
        scale_fill_manual(
          drop = FALSE,
          values = status_colors,
          labels = c(
            "No color assigned",
            "Red",
            "Orange",
            "Yellow",
            "Green",
            "Blue"
          )
        ) +
        scale_color_manual(
          values = c("FALSE" = NA, "TRUE" = "darkred"),
          drop = FALSE,
          na.value = "transparent",
          labels = c(
            "TRUE" = "Indicator Eligible for DA",
            "FALSE" = "Not eligible"
          )
        ) +
        geom_hline(yintercept = 0, color = "slategrey") +
        geom_hline(
          data = ca_aggregate(),
          aes(
            yintercept = currstatus,
            text = paste0(
              "CA Aggregate All Students",
              "\n",
              "Current status: ",
              currstatus
            )
          ),
          color = "lightskyblue"
        ) +
        facet_wrap(~indicator, ncol = 1, scales = "free_y") +
        labs(
          title = NULL,
          x = "Student Group",
          y = "Current Status",
          fill = "Color/Status",
          color = "DA Eligibility",
          caption = "Purple symbol indicates previous year status.
          Blue line shows the aggregate status for all students in CA."
        ) +
        guides(fill = "none", color = "none") +
        theme_minimal() +
        theme(
          title = element_text(size = 25),
          axis.text.x = element_text(
            size = 12,
            vjust = 1,
            hjust = 1,
            angle = 45
          ),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          strip.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.caption = element_text(size = 12)
        )

      ggplotly(dashboard_all_indicators, height = 1500, tooltip = c("text")) |>
        layout(title = list(y = 2, xref = "paper", yref = "paper"))
    }
  )

  # Change Tab --------------------
  change_check <- reactive({
    validate(
      need(
        dashboard_year() != "2022",
        "Only status was included in the 2022 data. Change was not used to determine status or DA eligibility. 
           
Please switch to the status tab for 2022 data or choose a different year to see the changes."
      )
    )
    dashboard_graphable()
  })

  output$change_title <- renderText(
    paste(
      "<h3>Change in Dashboard Indicators for\n",
      lea_title(),
      dashboard_year(),
      "</h3>"
    )
  )

  # Col Plot of each LEA -- change since last year in each indicator.
  output$change_plotly <- renderPlotly(
    {
      dashboard_change <-
        change_check() |>
        ggplot() +
        geom_col(
          mapping = aes(
            x = student_group_long,
            y = change,
            fill = color,
            color = indicator_eligible,
            text = paste0(
              student_group_long,
              "\n",
              "Change: ",
              change,
              "\nn: ",
              currdenom
            )
          ),
          linewidth = 1
        ) +
        geom_text(
          mapping = aes(
            x = student_group_long,
            y = change / 2,
            label = studentgroup
          ),
          color = "black"
        ) +
        scale_fill_manual(
          drop = FALSE,
          values = status_colors,
          guide = "none"
        ) +
        scale_color_manual(
          values = c("FALSE" = NA, "TRUE" = "darkred"),
          drop = FALSE,
          na.value = "transparent",
          labels = c(
            "TRUE" = "Indicator Eligible for DA",
            "FALSE" = "Not eligible"
          )
        ) +
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~indicator, ncol = 1, scales = "free_y") +
        labs(
          title = NULL,
          x = "Student Group",
          y = "Change",
          caption = "Blue line shows the aggregate change for all students in CA."
        ) +
        guides(fill = "none", color = "none") +
        theme_minimal() +
        theme(
          title = element_text(size = 25),
          axis.text.x = element_text(
            size = 12,
            vjust = 1,
            hjust = 1,
            angle = 45
          ),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          strip.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.caption = element_text(size = 12)
        )

      ggplotly(dashboard_change, height = 1500, tooltip = c("text")) |>
        layout(
          title = list(y = 2, xref = "paper", yref = "paper"),
          margin(list(l = 50, t = 200, r = 100, b = 150))
        )
    }
  )

  # Datatable tab -------------------

  dashboard_wide <- reactive(
    dashboard_graphable() |>
      select(
        reportingyear,
        countyname,
        districtname,
        schoolname,
        charter = charter_flag,
        `student group` = student_group_long,
        `numerator (when applicable)` = currnumer,
        `denominator` = currdenom,
        priority,
        indicator,
        color,
        `current status` = currstatus,
        change,
        indicator_eligible
      ) |>
      mutate(
        color = case_match(
          color,
          "0" ~ "Status only",
          "1" ~ "Red",
          "2" ~ "Orange",
          "3" ~ "Yellow",
          "4" ~ "Green",
          "5" ~ "Blue",
          .default = "No color assigned"
        ),
        charter = if_else(charter == "TRUE", "Yes", "No")
      )
    # |>
    # pivot_wider(names_from = c(priority, indicator),
    #             values_from = c(`current status`, change, color),
    #             names_glue = "Priority {priority} {indicator} {.value}") |>
    # select(reportingyear:`student group`,
    #        starts_with("Priority 4 ELA"), starts_with("Priority 4 Math"), starts_with("Priority 4 ELPI"),
    #        starts_with("Priority 5 absenteeism"), starts_with("Priority 5 graduation"),
    #        starts_with("Priority 6"), starts_with("Priority 8"))
  )

  output$detail_table <- DT::renderDataTable(
    {
      dashboard_wide()
    },
    extensions = "Buttons",
    rownames = FALSE,
    filter = "top",
    # colnames = c("Year", "County", "District", "School",
    #              "Charter", "Student Group",
    #              "Priority 4 - ELA", "Priority 4 - Math", "Priority 4 - ELPI",
    #              "Priority 5 - Absenteeism", "Priority 5 - Graduation",
    #              "Priority 6 - Suspension", "Priority 8 - College/Career"),
    options = list(
      dom = 'frtipB',
      pageLength = 100,
      buttons = c("csv", "excel")
    )
  )

  # DA eligibility --------

  da_district_options <- reactive(
    districts |>
      # filter(countyname == input$da_county) |>
      filter(countyname == "Solano") |>
      distinct()
  )

  observeEvent(da_district_options(), {
    updateCheckboxGroupInput(
      inputId = "da_districts",
      choices = da_district_options()$districtname,
      selected = da_district_options()$districtname
    )
  })

  charter_check <- reactive(
    input$include_charters
  )

  da_filtered <- reactive(
    dashboard |>
      filter(
        rtype == "D" |
          rtype == "X" |
          (charter_check() & charter_flag == "Y" & rtype == "S"),
        reportingyear == input$da_year,
        # countyname == input$da_county,
        countyname == "Solano",
        districtname %in% input$da_districts,
        student_group_long %in% input$da_groups
      )
  )

  output$county_da_table <- DT::renderDataTable(
    {
      da_filtered() |>
        filter(indicator_eligible == T) |>
        mutate(
          change_words = case_when(
            indicator %in%
              c("ELA", "Math", "ELPI", "graduation", "college/career") &
              changelevel == 1 ~
              "Decreased Significantly",
            indicator %in%
              c("ELA", "Math", "ELPI", "graduation", "college/career") &
              changelevel == 2 ~
              "Decreased",
            indicator %in%
              c("ELA", "Math", "ELPI", "graduation", "college/career") &
              changelevel == 3 ~
              "Maintained",
            indicator %in%
              c("ELA", "Math", "ELPI", "graduation", "college/career") &
              changelevel == 4 ~
              "Increased",
            indicator %in%
              c("ELA", "Math", "ELPI", "graduation", "college/career") &
              changelevel == 5 ~
              "Increased Significantly",
            indicator %in%
              c("ELA", "Math", "ELPI", "graduation", "college/career") &
              changelevel == 0 ~
              "No Change Level",
            indicator %in% c("absenteeism", "suspension") & changelevel == 1 ~
              "Increased Significantly",
            indicator %in% c("absenteeism", "suspension") & changelevel == 2 ~
              "Increased",
            indicator %in% c("absenteeism", "suspension") & changelevel == 3 ~
              "Maintained",
            indicator %in% c("absenteeism", "suspension") & changelevel == 4 ~
              "Declined",
            indicator %in% c("absenteeism", "suspension") & changelevel == 5 ~
              "Declined Significantly",
            indicator %in% c("absenteeism", "suspension") & changelevel == 0 ~
              "No Change Level"
          ),
          color_word = if_else(
            reportingyear == "2023" |
              reportingyear == "2024",
            case_match(
              color,
              "1" ~ "Red",
              "2" ~ "Orange",
              "3" ~ "Yellow",
              "4" ~ "Green",
              "5" ~ "Blue",
              "0" ~ "No Color"
            ),
            color
          )
        ) |>
        select(
          districtname,
          schoolname,
          student_group_long,
          currnumer,
          currdenom,
          priority,
          indicator,
          color_word,
          currstatus,
          change,
          change_words
        ) |>
        arrange(desc(currdenom))
    },
    extensions = "Buttons",
    rownames = FALSE,
    filter = "top",
    colnames = c(
      "District",
      "School Name",
      "Student Group",
      "Numerator (when applicable)",
      "Total Students",
      "Priority",
      "Indicator",
      "Color/Status Level",
      "Current Status",
      "Change",
      "Change level"
    ),
    options = list(
      dom = 'Blfrtip',
      pageLength = 150,
      buttons = c("csv", "excel")
    )
  )

  # ESSA -------------------------------------

  essa_schools <- reactive({
    solano_csi |>
      filter(
        solano_csi$districtname %in% input$essa_districts,
        (input$essa_charters == "All schools") |
          (input$essa_charters == "No charters" &
            solano_csi$charter_flag == "N") |
          (input$essa_charters == "Only charters" &
            solano_csi$charter_flag == "Y")
      ) |>
      mutate(
        schoolname = str_wrap(schoolname, 30)
      )
  })

  output$atsi_plot <- renderPlot(
    height = function() {
      selected <- filter(
        essa_schools(),
        year == "2025",
        csi_assistance_status == "ATSI"
      )
      length(unique(selected$schoolname)) * 35 + 100
    },
    {
      essa_schools() |>
        filter(year == "2025", csi_assistance_status == "ATSI") |>
        ggplot() +
        geom_tile(
          mapping = aes(
            x = student_group_wrap,
            y = schoolname,
            fill = factor(atsi_support)
          )
        ) +
        scale_fill_manual(
          values = c("Not Eligible" = "#BBBBBB", "Eligible" = "#CC79A7")
        ) +
        scale_x_discrete(position = "top") +
        labs(
          title = "ATSI Eligibility by student Group in 2025",
          x = "Student Group",
          y = NULL,
          fill = "ATSI Eligibility"
        ) +
        guides(fill = guide_legend(position = "bottom")) +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 0)
        )
    }
  )

  # Tile plot of ESSA eligibility for selected schools by years.
  output$csi_plot <- renderPlot(
    height = function() {
      selected <- essa_schools()
      length(unique(selected$schoolname)) * 30 + 100
    },
    {
      essa_schools() |>
        ggplot(
          mapping = aes(
            x = year,
            y = reorder(schoolname, desc(schoolname))
          )
        ) +
        geom_tile(mapping = aes(fill = csi_assistance_status)) +
        labs(
          x = "Year",
          y = NULL,
          fill = "Eligibility",
          title = "ESSA Eligibility in Solano County",
          caption = "Due to COVID-19 pandemic 2020-2021 is a duplicate of 2019-2020 eligibility."
        ) +
        scale_fill_manual(
          values = c(
            "No Status" = "#BBBBBB",
            "CSI Grad" = "#56B4E9",
            "CSI Low Perform" = "#009E73",
            "ATSI" = "#CC79A7",
            "TSI" = "#F0E442"
          )
        ) +
        scale_x_discrete(position = "top") +
        guides(fill = guide_legend(position = "top")) +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.text = element_text(size = 14)
        )
    }
  )

  # # Map ----------------------
  #
  #   selected_color_range <- reactive(
  #     seq(input$color_range[1], input$color_range[2])
  #   )
  #
  #   districts_for_map <- reactive(
  #     dashboard |>
  #       filter(rtype == "D",
  #              indicator == input$indicator,
  #              color %in% selected_color_range(),
  #              student_group_long == input$group) |>
  #       distinct(cds, .keep_all = T) |>
  #       left_join(districts_geo)
  #   )
  #
  #   schools_for_map <- reactive(
  #     dashboard |>
  #       filter(rtype == "S",
  #              indicator == input$indicator,
  #              color %in% selected_color_range(),
  #              student_group_long == input$group) |>
  #       distinct(cds, .keep_all = T) |>
  #       left_join(schools_geo)
  #   )
  #
  #   output$solanoIndicatorMap <- renderLeaflet({
  #     leaflet() |>
  #       addTiles() |>
  #       addPolygons(data = districts_for_map(),
  #                   fillOpacity = 0.4,
  #                   fillColor = ~indicator_color) |>
  #       addCircleMarkers(data = schools_for_map(),
  #                        label = ~schoolname,
  #                        radius = 6,
  #                        stroke = FALSE,
  #                        fillOpacity = 1,
  #                        color = ~indicator_color)
  #   })
}

# Run the application
shinyApp(ui = ui, server = server)
