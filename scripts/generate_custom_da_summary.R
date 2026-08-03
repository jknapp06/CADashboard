# Generate custom DA summary

library(tidyverse)
library(here)

solano_dashboard <- read_csv(here("data/solano_dashboard.csv"))

districts <-
  solano_dashboard %>%
  pull(districtname) |>
  unique()

for (district in districts) {
  quarto::quarto_render(
    here("reports/da_custom_summary.qmd"),
    output_file = paste(district, "DA Summary 2025"),
    execute_params = list("lea" = district)
  )
}
