# db.R
# DuckDB integration helpers: create connection, write tables, create indexes, query.
# Use this from refresh.R to populate a persistent duckdb file for the app.

library(DBI)
library(duckdb)

# connect_duckdb: open a DuckDB connection to a file (create if missing).
# - db_path: path to .duckdb (e.g., data/ca_education.duckdb)
# - read_only: if TRUE opens read-only
connect_duckdb <- function(
  db_path = "data/ca_education.duckdb",
  read_only = FALSE
) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

# write_tables: write a named list of data frames to DuckDB (overwrite by default).
# - con: DBI connection
# - tbls: named list, e.g., list(ca_dashboard = ca_dashboard, assistance = assistance)
# - create_index: vector of SQL index statements or a named list of columns to index
write_tables <- function(
  con,
  tbls,
  overwrite = TRUE,
  index_cols = list(
    ca_dashboard = c("cds", "reportingyear"),
    assistance = c("cds", "studentgroup", "reportingyear")
  )
) {
  for (n in names(tbls)) {
    DBI::dbWriteTable(con, n, tbls[[n]], overwrite = overwrite)
  }
  # create simple indexes for fast filtering if supported
  for (t in names(index_cols)) {
    cols <- index_cols[[t]]
    if (length(cols)) {
      idx_name <- paste0("idx_", t, "_", paste(cols, collapse = "_"))
      sql <- glue::glue(
        "CREATE INDEX IF NOT EXISTS {DBI::dbQuoteIdentifier(con, idx_name)} ON {DBI::dbQuoteIdentifier(con, t)} ({paste(DBI::dbQuoteIdentifier(con, cols), collapse = ', ')})"
      )
      try(DBI::dbExecute(con, sql), silent = TRUE)
    }
  }
}

# safe_query: parameterized query returning a tibble
safe_query <- function(con, sql, params = list()) {
  DBI::dbGetQuery(con, sql, params = params)
}
