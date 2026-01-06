# Generate CSI reports

library(tidyverse)
library(here)

essa <- read_csv(here("data/dashboard_essa.csv")) |>
  filter(countyname == "Solano", reportingyear == 2025)

# list all districts and DA eligible charters

graduation <-
  essa |>
  filter(essa_status == "CSI Grad") |>
  pull(schoolname) |>
  unique()

low_perform <-
  essa |>
  filter(essa_status == "CSI Low Perform") |>
  pull(schoolname) |>
  unique()

for (school in graduation) {
  quarto::quarto_render(
    "reports/csi_graduation_one_page.qmd",
    output_file = paste(school, "CSI Report.pdf"),
    execute_params = list("school" = school)
  )
}
for (school in low_perform) {
  quarto::quarto_render(
    "reports/csi_low_perform_one_page.qmd",
    output_file = paste(school, "CSI Report.pdf"),
    execute_params = list("school" = school)
  )
}
