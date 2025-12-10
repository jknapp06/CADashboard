# load-files.R
# Functions to download, cache, and read raw source files.
# - Keeps long-term cached copies with a date-stamped filename (YYYYMMDD)
# - Provides "force" to re-download latest while preserving older copies
# - Gracefully skips files that fail to download and returns informative warnings
# - Helpers to pick the latest cached copy for reading
#
# Dependencies: dplyr, vroom, openxlsx, glue, janitor, fs
library(dplyr)
library(vroom)
library(openxlsx)
library(glue)
library(janitor)
library(fs)

options(scipen = 999) # so CDS isn't changed in scientific notation
# Ensure cache dir exists
default_cache_dir <- "data/cache"
dir_create(default_cache_dir, recurse = TRUE)

# safe_download_file:
# - Attempts to download a URL to cache_dir with a date-stamped filename.
# - If the exact same filename already exists (same date), returns that path unless force = TRUE.
# - Returns the path to the cached file (invisibly) on success; on error returns NULL and
#   emits a warning (does not stop execution).
# - Parameters:
#   url: source URL
#   cache_dir: where to store cached files
#   force: if TRUE always attempt to download a fresh copy (still keeps previous files)
#   date_stamp: use Sys.Date() by default, but can pass custom date (as Date or "YYYYMMDD")
safe_download_file <- function(
  url,
  cache_dir = default_cache_dir,
  force = FALSE,
  date_stamp = Sys.Date()
) {
  # Normalize cache dir
  dir_create(cache_dir, recurse = TRUE)

  # Build dated filename: original basename prefixed with YYYYMMDD_
  stamp <- if (inherits(date_stamp, "Date")) {
    format(date_stamp, "%Y%m%d")
  } else {
    as.character(date_stamp)
  }
  base <- basename(url)
  # sanitize basename to avoid query strings
  base <- sub("\\?.*$", "", base)
  target_name <- glue("{stamp}_{base}")
  target_path <- file.path(cache_dir, target_name)

  # If file exists and not forcing, return it
  if (file_exists(target_path) && !force) {
    return(target_path)
  }

  # Otherwise attempt to download to a temp file then move into place
  tmp <- tempfile(pattern = "dl_")
  res <- tryCatch(
    {
      utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
      # If download.file returns without error, move into place (keep prior versions)
      file_move(tmp, target_path)
      target_path
    },
    error = function(e) {
      # Clean up tmp if exists
      if (file_exists(tmp)) {
        file_delete(tmp)
      }
      warning(glue("Skipping {url} — download failed: {e$message}"))
      NULL
    },
    warning = function(w) {
      # treat warnings as non-fatal but inform
      if (file_exists(tmp)) {
        file_delete(tmp)
      }
      warning(glue("Skipping {url} — download warning: {w$message}"))
      NULL
    }
  )
  invisible(res)
}

# list_cached_versions:
# - For a given URL or basename, returns a tibble of cached files with parsed date and full path.
# - Useful to inspect available historical copies.
list_cached_versions <- function(
  url_or_basename,
  cache_dir = default_cache_dir
) {
  base <- basename(url_or_basename) |>
    stringr::str_remove("\\?.*$")

  pattern <- glue("*_{base}")
  files <- dir_ls(
    cache_dir,
    regexp = glob2rx(pattern),
    type = "file",
    recurse = FALSE
  )

  if (length(files) == 0) {
    return(tibble(path = character(), date = as.Date(character())))
  }

  info <- tibble(path = files) |>
    mutate(
      fname = path_file(path),
      date = map_chr(fname, ~ str_extract(.x, "^\\d{8}")) |>
        as.Date(format = "%Y%m%d")
    ) |>
    arrange(desc(date))

  info
}


# latest_cached_file:
# - Returns the path to the most recent cached copy for a given URL/basename, or NULL if none.
latest_cached_file <- function(url_or_basename, cache_dir = default_cache_dir) {
  v <- list_cached_versions(url_or_basename, cache_dir = cache_dir)
  if (nrow(v) == 0) {
    return(NULL)
  }
  v$path[[1]]
}

# download_all_dashboard_files:
# - Given a data frame/tibble like dashboard_files with columns (url, indicator, priority),
# - attempts to download each URL to the cache directory. Continues on errors.
# - Returns a tibble with columns: url, indicator, priority, cached_path (NA if failed), downloaded (logical)
download_all_dashboard_files <- function(
  dashboard_files,
  cache_dir = default_cache_dir,
  force = FALSE,
  date_stamp = Sys.Date()
) {
  pmap_dfr(
    list(
      dashboard_files$url,
      dashboard_files$indicator,
      dashboard_files$priority
    ),
    function(url, indicator, priority) {
      # Attempt download; handle failures gracefully.
      pth <- safe_download_file(
        url,
        cache_dir = cache_dir,
        force = force,
        date_stamp = date_stamp
      )
      tibble(
        url = url,
        indicator = indicator,
        priority = priority,
        cached_path = if (is.null(pth)) NA_character_ else pth,
        downloaded = !is.null(pth)
      )
    }
  )
}

# load_dashboard_file_from_cache:
# - Reads a dashboard TXT file from a local path (cached file) using vroom,
# - Applies the same col_types logic as before, cleans names, and adds indicator+priority metadata.
# - Returns a tibble or NULL if reading fails (with a warning).
load_dashboard_file_from_cache <- function(
  local_path,
  d_indicator,
  priority,
  col_types = NULL
) {
  if (is.na(local_path) || is.null(local_path) || !file_exists(local_path)) {
    warning(glue(
      "Cannot read dashboard file: missing cached file for indicator {d_indicator}"
    ))
    return(NULL)
  }

  # Determine col_types if not provided
  if (is.null(col_types)) {
    col_types <- switch(
      d_indicator,
      "ELA" = cols(
        coe_flag = col_character(),
        pairshare_method = col_character(),
        .default = col_guess()
      ),
      "Math" = cols(
        coe_flag = col_character(),
        pairshare_method = col_character(),
        .default = col_guess()
      ),
      "ELPI" = cols(coe_flag = col_character(), .default = col_guess()),
      "absenteeism" = cols(
        coe_flag = col_character(),
        certifyflag = col_character(),
        dataerrorflag = col_character(),
        .default = col_guess()
      ),
      "suspension" = cols(
        coe_flag = col_character(),
        certifyflag = col_character(),
        dataerrorflag = col_character(),
        .default = col_guess()
      ),
      cols(.default = col_guess())
    )
  }

  res <- tryCatch(
    {
      vroom::vroom(local_path, col_types = col_types, progress = FALSE) |>
        clean_names() |>
        # remove_empty(c("rows", "columns")) |>
        mutate(indicator = d_indicator, priority = priority) |>
        rename(
          reportingyear = dplyr::any_of(
            c("reportingyear", "reporting_year")
          ),
          changelevel = dplyr::any_of(c("changelevel", "change_level"))
        )
    },
    error = function(e) {
      warning(glue(
        "Failed to read cached dashboard file {local_path}: {e$message}"
      ))
      NULL
    }
  )
  res
}

# download_and_load_dashboard_set:
# - High-level convenience: given dashboard_files tibble, ensures downloads (preserving old versions),
# - then reads the latest cached copy for each row and returns a combined tibble of available files.
# - Parameters:
#   dashboard_files: tibble with columns url, indicator, priority
#   cache_dir, force, date_stamp: passed to safe_download_file
download_and_load_dashboard_set <- function(
  dashboard_files,
  cache_dir = default_cache_dir,
  force = FALSE,
  date_stamp = Sys.Date()
) {
  # Step 1: attempt downloads (non-fatal)
  dl_report <- download_all_dashboard_files(
    dashboard_files,
    cache_dir = cache_dir,
    force = force,
    date_stamp = date_stamp
  )

  # Step 2: for each row, pick the cached_path if available; otherwise try latest_cached_file (in case local older exists)
  dl_report <- dl_report |>
    dplyr::rowwise() |>
    dplyr::mutate(
      cached_path = if (is.na(cached_path)) {
        latest_cached_file(url, cache_dir = cache_dir)
      } else {
        cached_path
      }
    ) |>
    dplyr::ungroup()

  # Step 3: read all available cached files into a single tibble; skip missing
  parts <- purrr::pmap(
    list(dl_report$cached_path, dl_report$indicator, dl_report$priority),
    function(cached_path, indicator, priority) {
      load_dashboard_file_from_cache(cached_path, indicator, priority)
    }
  )

  # Keep only non-NULL tibbles
  available <- purrr::keep(parts, ~ !is.null(.x))

  if (length(available) == 0) {
    warning("No dashboard files were successfully read from cache.")
    return(tibble::tibble())
  }

  # Bind into one tibble (preserve original columns via left join)
  combined <- dplyr::bind_rows(available)
  combined
}

# load_assistance_xlsx_from_cache:
# - Similar to load_assistance_xlsx but supports long-term cache (date-stamped files).
# - If path_or_url is a URL, it will attempt to download and keep the dated copy; returns the latest cached copy.
# - On failure returns NULL with a warning.
load_assistance_xlsx_from_cache <- function(
  path_or_url,
  sheet = 4,
  start_row = 6,
  cache_dir = default_cache_dir,
  force = FALSE,
  date_stamp = Sys.Date()
) {
  # If URL, attempt safe_download_file (non-fatal)
  if (grepl("^https?://", path_or_url)) {
    pth <- safe_download_file(
      path_or_url,
      cache_dir = cache_dir,
      force = force,
      date_stamp = date_stamp
    )
    if (is.null(pth)) {
      # fallback to latest cached (older) if available
      pth <- latest_cached_file(path_or_url, cache_dir = cache_dir)
      if (is.null(pth)) {
        warning(glue(
          "No cached assistance file available for {path_or_url}; skipping."
        ))
        return(NULL)
      }
    }
  } else {
    pth <- path_or_url
  }

  # Attempt to read
  res <- tryCatch(
    {
      openxlsx::read.xlsx(pth, sheet = sheet, startRow = start_row) |>
        as_tibble() |>
        janitor::clean_names()
    },
    error = function(e) {
      warning(glue("Failed to read assistance file {pth}: {e$message}"))
      NULL
    }
  )
  res
}

load_essa_xlsx_from_cache <- function(
  path_or_url,
  sheet = 2,
  start_row = 3,
  cache_dir = default_cache_dir,
  force = FALSE,
  date_stamp = Sys.Date()
) {
  # Validate inputs
  stopifnot(
    "path_or_url must be a character string" = is.character(path_or_url),
    "sheet must be a positive integer" = is.numeric(sheet) && sheet > 0,
    "start_row must be a positive integer" = is.numeric(start_row) &&
      start_row > 0
  )

  # Print a message indicating which file is being processed
  message(glue("Loading ESSA file from {path_or_url}"))

  # Determine file path
  pth <- tryCatch(
    {
      if (grepl("^https?://", path_or_url)) {
        # Attempt to download or find cached file
        downloaded_path <- safe_download_file(
          path_or_url,
          cache_dir = cache_dir,
          force = force,
          date_stamp = date_stamp
        )

        if (is.null(downloaded_path)) {
          cached_path <- latest_cached_file(path_or_url, cache_dir = cache_dir)

          if (is.null(cached_path)) {
            warning(glue(
              "No cached ESSA file available for {path_or_url}; skipping."
            ))
            return(NULL)
          }
          cached_path
        } else {
          downloaded_path
        }
      } else {
        # Local file path
        path_or_url
      }
    },
    error = function(e) {
      warning(glue("Error finding ESSA file: {e$message}"))
      return(NULL)
    }
  )

  # Validate file path
  if (is.null(pth) || !file.exists(pth)) {
    warning(glue("File not found: {pth}"))
    return(NULL)
  }

  # Read and process file
  res <- tryCatch(
    {
      # Attempt to read Excel file
      raw_data <- openxlsx::read.xlsx(
        pth,
        sheet = sheet,
        startRow = start_row,
        colNames = TRUE
      )

      # Convert to tibble with robust error handling
      essa_data <- raw_data |>
        as_tibble() |>
        janitor::clean_names()

      # Safely convert columns to numeric
      numeric_cols <- c(
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
        "wh",
        "enrollment_count"
      )

      # Only attempt conversion for columns that exist
      existing_numeric_cols <- intersect(numeric_cols, names(essa_data))

      essa_data <- essa_data |>
        mutate(across(
          .cols = all_of(existing_numeric_cols),
          .fns = ~ suppressWarnings(as.numeric(.x)),
          .names = "{.col}"
        ))

      # Additional validation
      if (nrow(essa_data) == 0) {
        warning(glue("No data found in ESSA file: {pth}"))
        return(NULL)
      }

      essa_data
    },
    error = function(e) {
      warning(glue(
        "Failed to read ESSA file {pth}: {e$message}\n",
        "File details:\n",
        "  Exists: {file.exists(pth)}\n",
        "  Size: {file.size(pth)} bytes\n",
        "  Permissions: {file.access(pth, 4) == 0}"
      ))
      NULL
    }
  )

  # Return processed data or NULL
  res
}

# Download and load teacher assignments file from cache
load_teacher_files <- function(
  path_or_url,
  cache_dir = default_cache_dir,
  force = FALSE,
  date_stamp = Sys.Date()
) {
  if (grepl("^https?://", path_or_url)) {
    pth <- safe_download_file(
      path_or_url,
      cache_dir = cache_dir,
      force = force,
      date_stamp = date_stamp
    )
    if (is.null(pth)) {
      pth <- latest_cached_file(path_or_url, cache_dir = cache_dir)
      if (is.null(pth)) {
        warning(glue(
          "No cached teacher assignments file available for {path_or_url}; skipping."
        ))
        return(NULL)
      }
    }
  } else {
    pth <- path_or_url
  }

  res <- tryCatch(
    {
      vroom::vroom(
        pth,
        col_types = cols(.default = col_guess()),
        progress = FALSE
      ) |>
        janitor::clean_names()
    },
    error = function(e) {
      warning(glue(
        "Failed to read teacher assignments file {pth}: {e$message}"
      ))
      NULL
    }
  )
  res
}

# Utility: clear_old_cache
# - Optionally remove cached files older than a given number of days to control disk usage.
# - Accepts numeric days; if NULL does nothing.
clear_old_cache <- function(
  cache_dir = default_cache_dir,
  older_than_days = NULL
) {
  if (is.null(older_than_days)) {
    return(invisible(NULL))
  }
  files <- dir_ls(cache_dir, type = "file", recurse = FALSE)
  if (length(files) == 0) {
    return(invisible(NULL))
  }
  file_dates <- tibble::tibble(path = files) |>
    dplyr::mutate(
      fname = path_file(path),
      date = as.Date(sub("_.*$", "", fname), format = "%Y%m%d")
    ) |>
    dplyr::filter(!is.na(date) & (Sys.Date() - date) > older_than_days)
  if (nrow(file_dates) == 0) {
    return(invisible(NULL))
  }
  purrr::walk(
    file_dates$path,
    ~ {
      try(file_delete(.x), silent = TRUE)
    }
  )
  invisible(file_dates)
}
