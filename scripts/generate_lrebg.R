# Generate LREBG reports

library(tidyverse)
library(quarto)
library(here)

dashboard <- read_csv(here("data/solano_dashboard.csv"))
report_year <- 2025

# list all districts and DA eligible charters

districts <-
  dashboard |>
  filter(reportingyear == report_year) |>
  pull(districtname) |>
  unique()

for (district in districts) {
  # lea_output <- str_replace_all(lea, ":", "")

  quarto::quarto_render(
    here("reports/lrebg.qmd"),
    output_file = paste0(
      district,
      " LREBG Needs Assessment ",
      report_year,
      ".pdf"
    ),
    execute_params = list("district" = district, "year" = report_year)
  )
}
