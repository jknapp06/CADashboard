# load-files.R
# Functions to download, cache, and read raw source files.
# Uses vroom for dashboard text files and openxlsx for xlsx assistance/essa files.

library(vroom)
library(openxlsx)
library(glue)
library(janitor)

# download_and_cache: download remote file to data/cache; returns local path.
# - url: source URL
# - cache_dir: directory for cached files
# - force: if TRUE re-download even if file exists
download_and_cache <- function(url, cache_dir = "data/cache", force = FALSE) {
  fname <- file.path(cache_dir, basename(url))
  if (file.exists(fname) && !force) {
    return(fname)
  }
  tmp <- tempfile()
  tryCatch(
    {
      utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
      file.rename(tmp, fname)
      fname
    },
    error = function(e) {
      if (file.exists(tmp)) {
        file.remove(tmp)
      }
      stop(glue::glue("Failed to download {url}: {e$message}"))
    }
  )
}

# load_dashboard_file: read a dashboard TXT file with appropriate col types, normalize names,
# and attach indicator + priority. Returns a tibble.
load_dashboard_file <- function(
  file_url,
  d_indicator,
  priority,
  cache_dir = "data/cache",
  force = FALSE
) {
  local_path <- tryCatch(
    download_and_cache(file_url, cache_dir = cache_dir, force = force),
    error = function(e) stop(e)
  )
  # choose col types based on indicator to avoid parsing issues
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
  vroom::vroom(local_path, col_types = col_types, progress = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(indicator = d_indicator, priority = priority)
}

# load_assistance_xlsx: read assistance excel sheet and return cleaned tibble
load_assistance_xlsx <- function(
  path_or_url,
  sheet = 4,
  start_row = 6,
  cache_dir = "data/cache",
  force = FALSE
) {
  local_path <- if (grepl("^https?://", path_or_url)) {
    download_and_cache(path_or_url, cache_dir = cache_dir, force = force)
  } else {
    path_or_url
  }
  openxlsx::read.xlsx(local_path, sheet = sheet, startRow = start_row) |>
    as_tibble() |>
    janitor::clean_names()
}

# load_essa_xlsx: read ESSA xlsx with specified sheet/startRow; returns tibble
load_essa_xlsx <- function(
  path_or_url,
  sheet = 2,
  start_row = 3,
  cache_dir = "data/cache",
  force = FALSE
) {
  local_path <- if (grepl("^https?://", path_or_url)) {
    download_and_cache(path_or_url, cache_dir = cache_dir, force = force)
  } else {
    path_or_url
  }
  openxlsx::read.xlsx(local_path, sheet = sheet, startRow = start_row) |>
    as_tibble() |>
    janitor::clean_names()
}
