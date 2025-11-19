# Output spreadsheet with wide format of DA history

library(tidyverse)
library(openxlsx)

solano_dashboard <- read_csv("data/solano_dashboard.csv") |>
  clean_names() |>
  filter(countyname == "Solano")

da_indicators <-
  solano_dashboard |>
  filter(indicator_eligible == TRUE) |>
  select(
    reportingyear,
    districtname,
    student_group_long,
    indicator
  ) |>
  arrange(reportingyear)

da_history_wide <-
  da_indicators |>
  pivot_wider(
    names_from = reportingyear,
    values_from = indicator,
    # combine indicators into a list separated by semicolon if multiple
    values_fn = \(x) paste(unique(x), collapse = "; ")
  ) |>
  arrange(districtname, student_group_long)

# Write to Excel
write.xlsx(
  da_history_wide,
  file = "data_output/da_history_wide.xlsx",
  overwrite = TRUE
)
