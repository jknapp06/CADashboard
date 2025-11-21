# clean.R
# Canonical schema, mapping and cleaning functions to produce assistance, essa, ca_dashboard, and dashboard_essa tibbles.

library(dplyr)
library(stringr)
library(tidyr)

# normalize_assistance_files: reads assistance_xlsx tibbles and maps to canonical column names
# Input: named list of tibbles (raw reads). Output: single canonical assistance tibble.
# some assistance files do no have a reportingyear column. For those, we need to add it based on the file source.
normalize_assistance <- function(raw_list) {
  # Validate input
  if (!is.list(raw_list)) {
    stop("Input must be a list of data frames")
  }

  # Log the number of input files
  message("Normalizing assistance data from ", length(raw_list), " files")

  # Helper function with more robust error handling
  fix_one <- function(df, file_index) {
    # Validate each input data frame
    if (!is.data.frame(df)) {
      warning("Item ", file_index, " is not a data frame. Skipping.")
      return(NULL)
    }

    # Get column names
    df_names <- names(df)

    # More robust year detection with explicit checks
    assistance_year <- NA_integer_
    assistance_variable_name <- NA_character_

    # Prioritized year detection
    year_checks <- list(
      "assistance_status2025" = 2025,
      "assistance_status2024" = 2024,
      "assistance_status2023" = 2023,
      "assistance_status2022" = 2022,
      "assistance_status2019" = 2019,
      "assistance_status2018" = 2018,
      "assistance_status" = 2017
    )

    for (col_name in names(year_checks)) {
      if (col_name %in% df_names) {
        assistance_year <- year_checks[[col_name]]
        assistance_variable_name <- if (assistance_year == 2017) {
          "assistance_status"
        } else {
          paste0("assistance_status", assistance_year)
        }
        break
      }
    }

    # Throw an error if no year could be detected
    if (is.na(assistance_year)) {
      stop("Could not determine assistance year for file ", file_index)
    }

    # Detailed logging
    message(
      "Processing file ",
      file_index,
      ": Detected year ",
      assistance_year,
      ", Using variable ",
      assistance_variable_name
    )

    # Attempt to process the file with error handling
    tryCatch(
      {
        processed_df <- df |>
          # Add reportingyear and pick only the most recent assistance status column
          mutate(
            reportingyear = assistance_year,
            # Safely select the assistance status column
            assistance_status = if (assistance_variable_name %in% names(df)) {
              .data[[assistance_variable_name]]
            } else {
              NA_character_
            }
          ) |>
          # Flexible renaming of grades offered column
          rename(
            grades_offered = dplyr::any_of(c(
              "gsoffered",
              "gradesoffered",
              "grades_offered"
            ))
          ) |>
          # Keep key columns
          select(
            cds,
            grades_offered,
            reportingyear,
            assistance_status,
            ends_with("priorities"),
            starts_with("ec")
          )

        # Validate key columns
        if (!"cds" %in% names(processed_df)) {
          warning("File ", file_index, " is missing 'cds' column")
        }

        return(processed_df)
      },
      error = function(e) {
        warning("Error processing file ", file_index, ": ", e$message)
        return(NULL)
      }
    )
  }

  # Process all files, filtering out any NULL results
  processed_files <- Filter(
    Negate(is.null),
    lapply(seq_along(raw_list), function(i) fix_one(raw_list[[i]], i))
  )

  # Check if any files were successfully processed
  if (length(processed_files) == 0) {
    stop("No files could be processed")
  }
  # Bind rows and perform final transformations
  result <- dplyr::bind_rows(processed_files) |>
    # Pivot priorities columns
    pivot_longer(
      cols = ends_with("priorities"),
      names_to = "studentgroup",
      values_to = "assistance",
      names_pattern = "(.*)priorities"
    ) |>
    mutate(
      studentgroup = if_else(
        studentgroup == "tom",
        "MR",
        str_to_upper(str_remove_all(studentgroup, "_"))
      )
    ) |>
    # Drop rows with NA assistance
    drop_na(assistance)

  # Log final results
  message(
    "Normalized assistance data: ",
    nrow(result),
    " rows, ",
    n_distinct(result$cds),
    " unique CDSs"
  )

  return(result)
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

  # if (length(grp_present)) {
  #   essa_all <- essa_all |>
  #     mutate(across(all_of(grp_present), ~ readr::parse_number(.x)))
  # }

  # normalize key names and pivot long for ATSI support
  essa_all |>
    rename_with(~ str_to_lower(.x)) |>
    rename(
      schoolname = dplyr::any_of(c("schoolname", "school_name")),
      districtname = dplyr::any_of(c("districtname", "district_name")),
      countyname = dplyr::any_of(c("countyname", "county_name"))
    ) |>
    pivot_longer(
      cols = intersect(names(essa_all), grp_cols),
      names_to = "studentgroup",
      values_to = "atsi_support"
    )
}

# compute_priority4_summary: from ca dashboard-with-assistance, compute priority 4 CAASPP/ELPI eligibility
compute_priority4_summary <- function(df) {
  filtered_df <-
    df |>
    filter(priority == 4, priority_eligible == TRUE)

  # debugging message: print number of rows after filtering and unique indicators
  # message(
  #   "Computing priority 4 summary: ",
  #   nrow(filtered_df),
  #   " rows after filtering. Columns: ",
  #   paste(names(filtered_df), collapse = ", ")
  # )

  filtered_df |>
    mutate(
      color = dplyr::case_when(
        reportingyear == 2022 ~ statuslevel,
        reportingyear == 2024 & indicator == "science" ~ currstatus,
        .default = color
      )
    ) |>
    select(reportingyear, cds, student_group_long, indicator, color) |>
    pivot_wider(names_from = indicator, values_from = color) |>
    mutate(
      caaspp_eligible = (ELA %in% c(1, 2) & Math %in% c(1, 2)) &
        !(is.na(ELA) | is.na(Math)),
      elpi_eligible = ELPI == 1 |
        (student_group_long == "Long-Term English Learner" & ELPI == 2)
    )
}

# priority_eligibility_lookup: returns tibble mapping assistance -> allowed priorities
priority_eligibility_lookup <- tibble::tribble(
  ~assistance , ~priorities   ,
  "A"         , c(4, 5, 6)    ,
  "B"         , c(4, 5)       ,
  "C"         , c(5, 6)       ,
  "D"         , c(4, 6)       ,
  "E"         , c(4, 8)       ,
  "F"         , c(5, 8)       ,
  "G"         , c(6, 8)       ,
  "H"         , c(4, 5, 8)    ,
  "I"         , c(4, 6, 8)    ,
  "J"         , c(5, 6, 8)    ,
  "K"         , c(4, 5, 6, 8)
)

# normalize teacher assignments data
normalize_teacher_assignments <- function(df) {
  df |>
    janitor::clean_names() |>
    # make cds code by concatenating county, district, and school codes with leading zeros
    mutate(
      cds = paste0(
        coalesce(as.character(county_code), "00"),
        coalesce(as.character(district_code), "00000"),
        coalesce(as.character(school_code), "0000000")
      )
    ) |>
    rename(
      reportingyear = academic_year,
      schoolname = school_name,
      districtname = district_name,
      countyname = county_name
    )
}
