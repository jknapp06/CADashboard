library(tidyverse)
library(openxlsx)
library(quarto)
library(here)

schools <- read_csv(here("data/solano_schools.csv"))

schools <-
  schools |>
  filter(schoolname == "District Aggregate")

walk(
  schools$districtname,
  ~ {
    school <- schools |>
      filter(districtname == .x) |>
      pull(schoolname) |>
      nth(1)

    initials <- schools |>
      filter(districtname == .x) |>
      pull(initials) |>
      nth(1)

    print(paste(.x))
    quarto_render(
      here("reports/dashboard_one_pager_any_lea.qmd"),
      output_file = paste0(.x, " 2025 Dashboard Summary.pdf"),
      execute_params = list(
        "school" = school,
        "district" = .x,
        "initials" = initials,
        "year" = "2025"
      )
    )
  }
)
