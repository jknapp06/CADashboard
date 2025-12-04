# Generate DA Letters

library(tidyverse)
library(openxlsx)
library(quarto)
library(here)

lea_list <- read.xlsx(here("data/lea_list.xlsx"))

district_list <-
  lea_list %>%
  filter(!is.na(initials))

charter_list <-
  lea_list %>%
  filter(is.na(initials))

for (i in 1:nrow(district_list)) {
  if (district_list$da_status[i] == "DTA") {
    quarto_render(
      here("reports/da_letter_dta_district.qmd"),
      output_file = paste(district_list$lea[i], "DA Letter.docx"),
      execute_params = list(
        "lea" = district_list$lea[i],
        "initials" = district_list$initials[i],
        "superintendent" = district_list$superintendent[i]
      )
    )
  } else if (district_list$da_status[i] == "geolead") {
    quarto_render(
      here("reports/da_letter_district_geolead.qmd"),
      output_file = paste(district_list$lea[i], "DA Letter.docx"),
      execute_params = list(
        "lea" = district_list$lea[i],
        "initials" = district_list$initials[i],
        "superintendent" = district_list$superintendent[i]
      )
    )
  } else if (district_list$da_status[i] == "year 2") {
    quarto_render(
      here("reports/da_letter_year_2_district.qmd"),
      output_file = paste(district_list$lea[i], "DA Letter.docx"),
      execute_params = list(
        "lea" = district_list$lea[i],
        "initials" = district_list$initials[i],
        "superintendent" = district_list$superintendent[i]
      )
    )
  }
}

for (i in 1:nrow(charter_list)) {
  quarto_render(
    here("reports/da_letter_charter.qmd"),
    output_file = paste0(
      gsub("[:/\\*?\"<>|]", "_", charter_list$lea[i]),
      " DA Letter.docx"
    ),
    execute_params = list(
      "lea" = charter_list$lea[i],
      "superintendent" = charter_list$superintendent[i]
    )
  )
}
