# refresh.R
# Orchestrator to run the full ETL:
# 1) load URLs and helper functions (data-urls.R, load-files.R, clean.R, db.R)
# 2) download & read raw files (with caching)
# 3) clean & normalize into canonical tibbles
# 4) write CSV outputs
# 5) populate DuckDB for app/report use (not implemented)
#
# Usage:
# source("R/data-urls.R")
# source("R/load-files.R")
# source("R/clean.R")
# source("R/db.R")
# run_refresh(force = FALSE, db_path = "data/ca_education.duckdb")

library(tidyverse)
library(here)
library(arrow) # Added for Parquet support

options(scipen = 999) # so CDS isn't changed in scientific notation

# Run the whole pipeline.
# If this script is sourced directly, run with defaults (do not force).
if (identical(environment(), globalenv()) && interactive()) {
  # Ensure helper files are loaded if run interactively
  if (!exists("dashboard_files")) {
    source(here("scripts/data-urls.R"))
  }
  if (!exists("load_dashboard_file")) {
    source(here("scripts/load-files.R"))
  }
  if (!exists("normalize_assistance")) {
    source(here("scripts/clean.R"))
  }
  if (!exists("connect_duckdb")) {
    source(here("scripts/db.R"))
  }

  # - force: re-download remote files when TRUE
  # - db_path: path to DuckDB file to create/populate
  # - out_dir: directory for CSV outputs
  force <- FALSE
  db_path <- "data/ca_education.duckdb"
  out_dir <- "data"
  app_dir <- "app/app_data/"
  are_dir <- "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/Data"

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- 1. Load source lists from data-urls.R (must be sourced before calling) ----
  if (
    !exists("dashboard_files") ||
      !exists("assistance_urls") ||
      !exists("essa_urls")
  ) {
    stop("Please source data-urls.R before running.")
  }

  # ---- 2. Read dashboard indicator files ----
  # Use download_and_load_dashboard_set() from load-files.R
  message("Loading dashboard indicator files...")
  dashboard_raw <- download_and_load_dashboard_set(
    dashboard_files,
    cache_dir = "cache",
    force = force
  )

  # Basic validation
  required_dashboard_cols <- c("cds", "studentgroup", "reportingyear")
  missing <- setdiff(required_dashboard_cols, names(dashboard_raw))
  if (length(missing)) {
    stop(
      "Dashboard data missing required cols: ",
      paste(missing, collapse = ", ")
    )
  }

  # Set all school names to current names from latest year
  max_year <- max(dashboard_raw$reportingyear, na.rm = TRUE)

  current_names_codes <- dashboard_raw |>
    filter(reportingyear == max_year) |>
    select(cds, countyname, districtname, schoolname) |>
    distinct()

  dashboard_raw <- dashboard_raw |>
    select(-countyname, -districtname, -schoolname) |>
    left_join(current_names_codes, by = "cds")

  # ---- 3. Read assistance Excel files ----
  # Use load_assistance_xlsx_from_cache() from load-files.R
  # check 1_clean_assistance.R for sheet and start_row details
  message("Loading assistance files...")
  raw_assistance_list <- list(
    assistance_25 = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_25,
      sheet = 4,
      start_row = 6,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_25_charter = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_25_charter,
      sheet = 4,
      start_row = 6,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_24 = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_24,
      sheet = 4,
      start_row = 6,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_24_charter = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_24_charter,
      sheet = 4,
      start_row = 6,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_23 = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_23,
      sheet = 4,
      start_row = 6,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_23_charter = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_23_charter,
      sheet = 4,
      start_row = 6,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_22 = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_22,
      sheet = 4,
      start_row = 6,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_19 = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_19,
      sheet = 4,
      start_row = 6,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_18 = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_18,
      sheet = 1,
      start_row = 5,
      cache_dir = cache_dir,
      force = force
    ),
    assistance_17 = load_assistance_xlsx_from_cache(
      assistance_urls$assistance_17,
      sheet = 4,
      start_row = 5,
      cache_dir = cache_dir,
      force = force
    )
  )

  message("Normalizing assistance data...")
  assistance <- normalize_assistance(raw_assistance_list)

  # Print number of rows with non-missing assistance for debugging
  num_with_assistance <- assistance |>
    filter(!is.na(assistance)) |>
    nrow()

  message(
    "Number of rows with non-missing assistance after normalization: ",
    num_with_assistance
  )

  # # ---- 4. Read ESSA files ----
  message("Loading ESSA files...")
  raw_essa_list <- list(
    essa25 = load_essa_xlsx_from_cache(
      essa_urls$essa25,
      sheet = 2,
      start_row = 3,
      cache_dir = cache_dir,
      force = force
    ),
    essa24 = load_essa_xlsx_from_cache(
      essa_urls$essa24,
      sheet = 2,
      start_row = 3,
      cache_dir = cache_dir,
      force = force
    ),
    essa23 = load_essa_xlsx_from_cache(
      essa_urls$essa23,
      sheet = 2,
      start_row = 3,
      cache_dir = cache_dir,
      force = force
    ),
    essa22 = load_essa_xlsx_from_cache(
      essa_urls$essa22,
      sheet = 2,
      start_row = 3,
      cache_dir = cache_dir,
      force = force
    ),
    essa21 = load_essa_xlsx_from_cache(
      essa_urls$essa21,
      sheet = 1,
      start_row = 3,
      cache_dir = cache_dir,
      force = force
    ),
    essa19 = load_essa_xlsx_from_cache(
      essa_urls$essa19,
      sheet = 1,
      start_row = 3,
      cache_dir = cache_dir,
      force = force
    )
    # essa18 = load_essa_xlsx_from_cache(
    #   essa_urls$essa18,
    #   sheet = 1,
    #   start_row = 3,
    #   cache_dir = cache_dir,
    #   force = force
    # )
  )

  essa <- normalize_essa(raw_essa_list)

  # ---- 5. Join assistance to dashboard and compute eligibility ----
  message("Merging dashboard with assistance and computing eligibility...")

  # Normalize dashboard column names we use, and add long student group names
  dashboard_clean <- dashboard_raw |>
    rename_with(~ str_to_lower(.x)) |>
    mutate(
      # Some years don't have a student group for ELPI
      studentgroup = if_else(
        is.na(studentgroup) & indicator == "ELPI",
        "EL",
        studentgroup
      ),
      student_group_long = case_match(
        studentgroup,
        "ALL" ~ "All students",
        "AA" ~ "Black/African American",
        "AI" ~ "American Indian or Alaska Native",
        "AS" ~ "Asian",
        "FI" ~ "Filipino",
        "HI" ~ "Hispanic",
        "PI" ~ "Pacific Islander",
        "WH" ~ "White",
        "MR" ~ "Multiple Races/Two or more",
        "EL" ~ "English Learner",
        "ELO" ~ "English Learners Only",
        "RFP" ~ "RFEPs Only",
        "EO" ~ "English Only",
        "SBA" ~ "Smarter Balanced Assessment",
        "CAA" ~ "CA Alternative Assessment",
        "SED" ~ "Socioeconomically Disadvantaged",
        "SWD" ~ "Students with Disabilities",
        "FOS" ~ "Foster Youth",
        "HOM" ~ "Homeless Youth",
        "TOM" ~ "Multiple Races/Two or more",
        "LTEL" ~ "Long-Term English Learner",
        .default = studentgroup
      ),
      countyname = case_match(
        rtype,
        "X" ~ "CA State Aggregate",
        .default = countyname
      ),
      schoolname = case_when(
        rtype == "D" & is.na(schoolname) ~ "District Aggregate",
        TRUE ~ schoolname
      )
    )

  # Print column names of dashboard_clean, assistance, and priority_eligibility_lookup for debugging
  # message(
  #   "Dashboard columns: ",
  #   paste(sort(names(dashboard_clean)), collapse = ", ")
  # )
  # message(
  #   "Assistance columns: ",
  #   paste(sort(names(assistance)), collapse = ", ")
  # )
  # message(
  #   "Priority eligibility lookup columns: ",
  #   paste(sort(names(priority_eligibility_lookup)), collapse = ", ")
  # )

  # print sample of cds, studentgroup, reportingyear from dashboard_clean and assistance for debugging
  # message("Sample of dashboard_clean keys:")
  # print(
  #   head(
  #     dashboard_clean |>
  #       select(cds, studentgroup, reportingyear) |>
  #       distinct(),
  #     10
  #   )
  # )
  # message("Sample of assistance keys:")
  # print(
  #   head(
  #     assistance |>
  #       select(cds, studentgroup, reportingyear) |>
  #       distinct(),
  #     10
  #   )
  # )

  check_priorities <- function(priority, priorities) {
    priority %in% unlist(priorities)
  }

  # left join assistance
  dashboard_with_assistance <- dashboard_clean |>
    left_join(
      assistance,
      by = join_by(cds, studentgroup, reportingyear, charter_flag)
    ) |>
    left_join(
      priority_eligibility_lookup,
      by = join_by("assistance_current" == "assistance")
    ) |>
    rename(priorities_current = priorities) |>
    left_join(
      priority_eligibility_lookup,
      by = join_by("assistance_prior" == "assistance")
    ) |>
    rename(priorities_prior = priorities) |>
    left_join(priority_eligibility_lookup, by = "assistance")

  dashboard_with_elibibility <-
    dashboard_with_assistance |>
    mutate(
      priority_eligible = if_else(
        is.na(charter_flag), # if not a charter
        # check priority against priorites.
        # using purrr checks one row at a time.
        map2_lgl(priority, priorities, check_priorities),
        # check charter eligbility for priority based on current status.
        assistance_status == "Differentiated Assistance" &
          map2_lgl(priority, priorities_current, check_priorities)
      )
    ) |>
    # drop priorities so duckDB can store the table
    select(-starts_with("priorities"))

  # Print the number of rows after join for debugging
  message(
    "Dashboard with assistance has ",
    nrow(dashboard_with_elibibility),
    " rows after join."
  )

  # Print the number of rows with a value in assistance for debugging
  num_with_assistance <- dashboard_with_elibibility |>
    filter(!is.na(assistance)) |>
    nrow()
  message(
    "Number of rows with non-missing assistance after join: ",
    num_with_assistance
  )

  # print number of rows where priorty_eligible is TRUE
  num_priority_eligible <- dashboard_with_elibibility |>
    filter(priority_eligible) |>
    nrow()
  message(
    "Number of rows with priority_eligible == TRUE: ",
    num_priority_eligible
  )

  # Print sample of joined data for debugging
  # message("Sample of joined data:")
  # print(
  #   head(
  #     dashboard_with_elibibility |>
  #       filter(
  #         assistance %in%
  #           c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
  #         countyname == "Solano"
  #       ) |>
  #       select(
  #         cds,
  #         studentgroup,
  #         reportingyear,
  #         assistance,
  #         priority,
  #         # priorities,
  #         priority_eligible
  #       ),
  #     10
  #   )
  # )

  # ---- 6. Compute CA Dashboard eligibility logic ----
  message("Computing indicator eligibility and priority-4 summary...")

  priority_4_tbl <- compute_priority4_summary(dashboard_with_elibibility)

  ca_dashboard <- dashboard_with_elibibility |>
    left_join(
      priority_4_tbl,
      by = join_by(cds, reportingyear, student_group_long)
    ) |>
    mutate(
      indicator_eligible = case_when(
        priority == 8 & priority_eligible ~ TRUE,
        priority != 4 & priority_eligible & color == "1" ~ TRUE,
        priority != 4 &
          priority_eligible &
          reportingyear == 2022 &
          statuslevel == 1 ~ TRUE,
        studentgroup != "LTEL" &
          priority == 4 &
          indicator == "ELA" &
          caaspp_eligible == TRUE ~ TRUE,
        studentgroup != "LTEL" &
          priority == 4 &
          indicator == "Math" &
          caaspp_eligible == TRUE ~ TRUE,
        priority == 4 & indicator == "ELPI" & elpi_eligible == TRUE ~ TRUE,
        TRUE ~ FALSE
      )
    ) |>
    select(-c(ELA, Math, ELPI, caaspp_eligible, elpi_eligible))

  small_dashboard <- ca_dashboard |>
    filter(countyname == "Solano" | countyname == "CA State Aggregate")

  # ---- 7. Download and clean teacher credentialing data ----

  message("Loading teacher credentialing data...")
  teacher_assignments <- load_teacher_files(
    teacher_assignments_url,
    cache_dir = "cache",
    force = force
  )

  message("Normalizing teacher credentialing data...")
  teacher_assignments_clean <- normalize_teacher_assignments(
    teacher_assignments
  )

  solano_teachers <-
    teacher_assignments_clean |>
    filter(countyname == "Solano")

  # # print teacher_assignement_clean column names and sample
  # message(
  #   "Teacher assignments columns: ",
  #   paste(sort(names(teacher_assignments_clean)), collapse = ", ")
  # )
  # message("Sample of teacher assignments data:")
  # print(head(
  #   teacher_assignments_clean |>
  #     select(
  #       cds,
  #       county_code,
  #       district_code,
  #       school_code,
  #       countyname,
  #       districtname,
  #       schoolname,
  #       total_fte
  #     ),
  #   10
  # ))

  # ---- 8. Join with ESSA for local dashboard_essa ----
  dashboard_essa <- ca_dashboard |>
    filter(countyname == "Solano") |>
    left_join(
      essa,
      join_by(
        cds,
        districtname,
        countyname,
        schoolname,
        studentgroup,
        reportingyear
      )
    )

  # ---- 9. Write Parquet outputs ----
  message("Writing Parquet outputs to: ", out_dir)
  write_parquet(ca_dashboard, here(out_dir, "ca_dashboard.parquet"))
  write_parquet(small_dashboard, here(out_dir, "solano_dashboard.parquet"))
  write_parquet(assistance, here(out_dir, "assistance.parquet"))
  write_parquet(essa, here(out_dir, "essa.parquet"))
  write_parquet(dashboard_essa, here(out_dir, "dashboard_essa.parquet"))
  write_parquet(dashboard_essa, here(app_dir, "dashboard_essa.parquet"))
  write_parquet(dashboard_essa, file.path(are_dir, "dashboard_essa.parquet"))
  write_parquet(
    teacher_assignments_clean,
    here(out_dir, "teacher_assignments.parquet")
  )
  write_parquet(
    solano_teachers,
    here(app_dir, "teacher_assignments.parquet")
  )
  write_parquet(
    solano_teachers,
    file.path(are_dir, "teacher_assignments_2025.parquet")
  )

  # ---- 10. Populate DuckDB ----
  # message("Populating DuckDB at: ", db_path)
  # con <- connect_duckdb(db_path = db_path)
  # on.exit(
  #   {
  #     try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  #   },
  #   add = TRUE
  # )

  # tables_to_write <- list(
  #   ca_dashboard = ca_dashboard,
  #   small_dashboard = small_dashboard,
  #   assistance = assistance
  #   # ,
  #   # essa = essa,
  #   # dashboard_essa = dashboard_essa
  # )
  # write_tables(con, tables_to_write)

  # message("Refresh complete. CSVs written and DuckDB populated.")
  # invisible(list(
  #   ca_dashboard = ca_dashboard,
  #   small_dashboard = small_dashboard,
  #   assistance = assistance,
  #   # essa = essa,
  #   # dashboard_essa = dashboard_essa,
  #   db_path = db_path
  # ))
}
