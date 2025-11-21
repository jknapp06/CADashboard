# Generate 52072 reports

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
    here("reports/geolead_da.qmd"),
    output_file = paste(district, "Geolead DA Report 2025.docx"),
    execute_params = list("lea" = district)
  )
}
