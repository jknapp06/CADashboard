# clean.R
# Canonical schema, mapping and cleaning functions to produce assistance, essa, ca_dashboard, and dashboard_essa tibbles.

library(dplyr)
library(stringr)
library(tidyr)

# normalize_assistance_files: reads assistance_xlsx tibbles and maps to canonical column names
# Input: named list of tibbles (raw reads). Output: single canonical assistance tibble.
normalize_assistance <- function(raw_list) {
  # helper to select & rename per file variation
  fix_one <- function(df) {
    df_names <- names(df)
    df |>
      rename_with(~ str_to_lower(.x)) |>
      rename(
        cds = dplyr::any_of(c("cds", "c_d_s", "schoolcode")),
        grades_offered = dplyr::any_of(c("gsoffered", "gsoffered")),
        reportingyear = dplyr::any_of(c(
          "reportingyear",
          "reporting_year",
          "reportingyear"
        )),
        assistance_status = dplyr::any_of(c(
          "assistancestatus2024",
          "assistancestatus2023",
          "assistancestatus2022",
          "assistancestatus2019",
          "assistancestatus2018",
          "assistance_status",
          "assistance_status2018",
          "assistance_status2019"
        ))
      ) |>
      # keep any columns ending with priorities (case-insensitive)
      select(
        cds,
        grades_offered,
        reportingyear,
        assistance_status,
        tidyselect::matches("(?i)priorities")
      ) |>
      # ensure reportingyear exists
      mutate(reportingyear = as.character(reportingyear))
  }
  dplyr::bind_rows(lapply(raw_list, fix_one)) |>
    pivot_longer(
      cols = matches("(?i)priorities"),
      names_to = "studentgroup",
      values_to = "assistance",
      names_pattern = "(.*)priorities",
      values_drop_na = TRUE
    ) |>
    mutate(
      studentgroup = if_else(
        studentgroup == "TOM",
        "MR",
        str_remove_all(studentgroup, "_")
      )
    )
}

# normalize_essa: canonicalize ESSA files, pivot and compute ATSI and CSI summaries
# Input: list of raw essa tibbles (as returned by load_essa_xlsx)
normalize_essa <- function(raw_list) {
  # standardize names; then bind_rows and pivot longer for student groups
  essa_all <- bind_rows(raw_list) |>
    janitor::clean_names()

  # parse numeric enrollment and student group columns if present
  grp_cols <- c(
    "aa",
    "ai",
    "as",
    "el",
    "fi",
    "fos",
    "hi",
    "hom",
    "pi",
    "sed",
    "swd",
    "tom",
    "wh"
  )
  grp_present <- intersect(names(essa_all), grp_cols)

  if (length(grp_present)) {
    essa_all <- essa_all |>
      mutate(across(all_of(grp_present), ~ readr::parse_number(.x)))
  }

  # normalize key names and pivot long for ATSI support
  essa_all |>
    rename_with(~ str_to_lower(.x)) |>
    rename(
      cds = dplyr::any_of(c("cds", "c_d_s")),
      schoolname = dplyr::any_of(c("schoolname", "school_name")),
      districtname = dplyr::any_of(c("districtname", "district_name")),
      countyname = dplyr::any_of(c("countyname", "county_name"))
    ) |>
    pivot_longer(
      cols = intersect(names(.), grp_cols),
      names_to = "studentGroup",
      values_to = "ATSIsupport"
    )
}

# compute_priority4_summary: from ca dashboard-with-assistance, compute priority 4 CAASPP/ELPI eligibility
compute_priority4_summary <- function(df) {
  df |>
    filter(priority == 4, priority_eligible == TRUE) |>
    mutate(
      color = dplyr::case_when(
        reportingyear == 2022 ~ statuslevel,
        reportingyear == 2024 & indicator == "science" ~ currstatus,
        TRUE ~ color
      )
    ) |>
    select(reportingyear, cds, student_group_long, indicator, color) |>
    pivot_wider(names_from = indicator, values_from = color) |>
    mutate(
      caaspp_eligible = (ELA %in% c(1, 2) & Math %in% c(1, 2)) &
        !(is.na(ELA) | is.na(Math)),
      elpi_eligible = ELPI == 1
    )
}

# priority_eligibility_lookup: returns tibble mapping assistance -> allowed priorities
priority_eligibility_lookup <- function() {
  tibble::tribble(
    ~assistance , ~priorities         ,
    "A"         , list(c(4, 5, 6))    ,
    "B"         , list(c(4, 5))       ,
    "C"         , list(c(5, 6))       ,
    "D"         , list(c(4, 6))       ,
    "E"         , list(c(4, 8))       ,
    "F"         , list(c(5, 8))       ,
    "G"         , list(c(6, 8))       ,
    "H"         , list(c(4, 5, 8))    ,
    "I"         , list(c(4, 6, 8))    ,
    "J"         , list(c(5, 6, 8))    ,
    "K"         , list(c(4, 5, 6, 8))
  ) |>
    tidyr::unnest_longer(priorities)
}
