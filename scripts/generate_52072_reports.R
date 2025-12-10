# Generate 52072 reports

library(tidyverse)
library(here)

all_dashboard <- read_csv(here("data/ca_dashboard.csv")) |>
  filter(countyname == "Solano", rtype == "D")

all_da_groups <-
  all_dashboard |>
  filter(
    reportingyear %in% c(2025, 2024, 2023, 2022),
    indicator_eligible == TRUE,
    rtype == "D" |
      charter_flag == "Y" & rtype == "S"
  ) |>
  select(
    reportingyear,
    districtname,
    schoolname,
    student_group_long,
    indicator
  ) |>
  arrange(reportingyear) |>
  pivot_wider(
    names_from = reportingyear,
    values_from = indicator,
    values_fn = list
  ) |>
  arrange(districtname) |>
  mutate(across(where(is.list), ~ map_chr(., ~ paste(., collapse = "; ")))) |>
  mutate(
    da_years = rowSums(across(c(`2022`, `2023`, `2024`, `2025`), ~ . != ""))
  )

da_student_groups <-
  all_da_groups |>
  pull(student_group_long) |>
  unique()

dta_groups <-
  all_da_groups |>
  select(districtname, student_group_long, da_years) |>
  pivot_wider(names_from = student_group_long, values_from = da_years) |>
  mutate(
    group_count = rowSums(
      across(
        all_of(da_student_groups),
        ~ .x >= 3
      ),
      na.rm = TRUE
    )
  )

leas_to_report <-
  dta_groups |>
  filter(group_count >= 3) |>
  pull(districtname) |>
  unique()

for (lea in leas_to_report) {
  quarto::quarto_render(
    here("reports/da_report_with_dta.qmd"),
    output_file = paste(lea, "DA Report 2025.docx"),
    execute_params = list("lea" = lea)
  )
}
