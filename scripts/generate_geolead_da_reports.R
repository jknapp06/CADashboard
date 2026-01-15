# Generate geolead reports

library(tidyverse)
library(here)

solano_dashboard <- read_csv(here("data/solano_dashboard.csv"))

districts <-
  solano_dashboard %>%
  filter(ec52071f_2025 == "Y") |>
  pull(districtname) |>
  unique()

for (district in districts) {
  quarto::quarto_render(
    here("reports/da_report_with_geolead.qmd"),
    output_file = paste(district, "DA Report 2025"),
    execute_params = list("lea" = district)
  )
}
