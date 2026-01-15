# Generate DA reports

library(tidyverse)
library(here)

dashboard <- read_csv(here("data/solano_dashboard.csv"))

# list all districts and DA eligible charters

districts <-
  dashboard |>
  filter(reportingyear == 2025, indicator_eligible) |>
  pull(districtname) |>
  unique()

da_eligible_charters <-
  dashboard |>
  filter(
    reportingyear == 2025,
    charter_flag == "Y",
    assistance_status == "Differentiated Assistance"
  ) |>
  pull(schoolname) |>
  unique()

leas_to_report <- c(districts, da_eligible_charters)

for (lea in da_eligible_charters) {
  lea_output <- str_replace_all(lea, ":", "")

  quarto::quarto_render(
    here("reports/DA_one_page.qmd"),
    output_file = paste(lea_output, "DA one pager.pdf"),
    execute_params = list("lea" = lea)
  )
}
