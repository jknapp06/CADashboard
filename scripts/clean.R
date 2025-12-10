# clean.R

library(dplyr)
library(stringr)
library(tidyr)

options(scipen = 999) # so CDS isn't changed in scientific notation

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
  process_assistance <- function(df, file_index) {
    # Validate each input data frame
    if (!is.data.frame(df)) {
      warning("Item ", file_index, " is not a data frame. Skipping.")
      return(NULL)
    }

    # Get column names
    df_names <- names(df)
    # message(paste(df_names, collapse = ", "))

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

    # Check if charter
    charter <- "chartername" %in% df_names

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
        slim_df <- df |>
          # Add reportingyear and pick only the most recent assistance status column
          mutate(
            reportingyear = assistance_year,
            # Safely select the assistance status column
            assistance_status = if (assistance_variable_name %in% names(df)) {
              .data[[assistance_variable_name]]
            } else {
              NA_character_
            },
            charter_flag = if_else(charter, "Y", NA_character_)
          ) |>
          # Keep key columns
          select(
            cds,
            charter_flag,
            reportingyear,
            assistance_status,
            ends_with("priorities"),
            ends_with("current"),
            ends_with("prior"),
            starts_with("ec")
          )

        # Define the base patterns for current and prior columns
        current_patterns <- c(
          "a_acurrent",
          "a_icurrent",
          "a_scurrent",
          "e_lcurrent",
          "f_icurrent",
          "fo_scurrent",
          "h_icurrent",
          "ho_mcurrent",
          "se_dcurrent",
          "sw_dcurrent",
          "to_mcurrent",
          "w_hcurrent"
        )
        prior_patterns <- c(
          "a_aprior",
          "a_iprior",
          "a_sprior",
          "e_lprior",
          "f_iprior",
          "fo_sprior",
          "h_iprior",
          "ho_mprior",
          "se_dprior",
          "sw_dprior",
          "to_mprior",
          "w_hprior"
        )

        # For LTEL columns, only include them if they exist
        if ("lte_lcurrent" %in% names(slim_df)) {
          current_patterns <- c(current_patterns, "lte_lcurrent")
        }
        if ("lte_lprior" %in% names(slim_df)) {
          prior_patterns <- c(prior_patterns, "lte_lprior")
        }

        # Combine the patterns for pivot_longer
        cols_to_pivot <- c(current_patterns, prior_patterns)
        cols_to_pivot <- cols_to_pivot[cols_to_pivot %in% names(slim_df)] # Ensure only existing columns are included

        long_df <- tibble()
        if (charter) {
          message(paste(
            "Charter assistance file detected. Assistance year:",
            assistance_year
          ))
          # message(paste(df_names, collapse = ", "))
          long_df <-
            slim_df |>
            pivot_longer(
              cols = cols_to_pivot,
              names_to = c("studentgroup", "current_prior"),
              names_pattern = "(.+)(current|prior)",
              values_to = "assistance",
            ) |>
            pivot_wider(
              names_from = current_prior,
              values_from = assistance,
              names_prefix = "assistance_"
            )
        } else {
          long_df <-
            slim_df |>
            pivot_longer(
              cols = ends_with("priorities"),
              names_to = "studentgroup",
              values_to = "assistance",
              names_pattern = "(.*)priorities"
            )
        }

        processed_df <-
          long_df |>
          mutate(
            studentgroup = if_else(
              studentgroup == "tom",
              "MR",
              str_to_upper(str_remove_all(studentgroup, "_"))
            )
          )

        # Validate key columns
        if (!"cds" %in% names(processed_df)) {
          warning("File ", file_index, " is missing 'cds' column")
        }

        return(processed_df)
      },
      error = function(e) {
        warning("Error processing file ", file_index, ": ", e$message)
        traceback()
        return(NULL)
      }
    )
  }

  # Process all files, filtering out any NULL results
  processed_files <- keep(
    map2(raw_list, seq_along(raw_list), process_assistance),
    Negate(is.null)
  )

  # Check if any files were successfully processed
  if (length(processed_files) == 0) {
    stop("No files could be processed")
  }
  # Bind rows and perform final transformations
  result <- bind_rows(processed_files)

  # Check if result has charters
  message(paste(
    result |>
      filter(charter_flag == "Y") |>
      nrow(),
    "rows where charter_flag is TRUE in processed_files."
  ))

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
      countyname = dplyr::any_of(c("countyname", "county_name")),
      reportingyear = dplyr::any_of(c(
        "reportingyear",
        "reporting_year",
        "ReportingYear"
      ))
    ) |>
    mutate(reportingyear = parse_number(reportingyear)) |>
    # Select the assistance status column that matches the reporting year
    mutate(
      essa_status = case_when(
        reportingyear == 2018 ~ assistance_status2018,
        reportingyear == 2019 ~ assistance_status2019,
        reportingyear == 2020 ~ assistance_status2020,
        reportingyear == 2021 ~ assistance_status2021,
        reportingyear == 2022 ~ assistance_status2022,
        reportingyear == 2023 ~ assistance_status2023,
        reportingyear == 2024 ~ assistance_status2024,
        TRUE ~ NA_character_
      )
    ) |>
    # Pivot student groups
    pivot_longer(
      cols = intersect(names(essa_all), grp_cols),
      names_to = "studentgroup",
      values_to = "atsi_support"
    ) |>
    mutate(
      studentgroup = if_else(
        studentgroup == "tom",
        "MR",
        str_to_upper(studentgroup)
      )
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
