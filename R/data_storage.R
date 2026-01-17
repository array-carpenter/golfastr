#' Save Tournament Data to Database
#'
#' Save tournament data to a DuckDB database for fast future loading.
#'
#' @param data Tibble of tournament data (from load_tournament)
#' @param db_path Path to DuckDB database file (default: "data/golfastr.duckdb")
#' @param table_name Table name (default: "leaderboards")
#' @param append If TRUE, append to existing data. If FALSE, replace.
#' @export
#' @examples
#' \dontrun{
#' masters <- load_tournament(2025, "Masters")
#' save_to_db(masters)
#' }
save_to_db <- function(data,
                       db_path = "data/golfastr.duckdb",
                       table_name = "leaderboards",
                       append = TRUE) {

  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' required. Install with: install.packages('duckdb')")
  }

  # Create directory if needed
  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)

  # Connect to database
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  if (append && DBI::dbExistsTable(con, table_name)) {
    # Append data
    DBI::dbAppendTable(con, table_name, data)
    message("Appended ", nrow(data), " rows to ", table_name)
  } else {
    # Create/replace table
    DBI::dbWriteTable(con, table_name, data, overwrite = TRUE)
    message("Saved ", nrow(data), " rows to ", table_name)
  }
}

#' Load Data from Database
#'
#' Load tournament data from local DuckDB database (fast!).
#'
#' @param db_path Path to DuckDB database file
#' @param table_name Table name (default: "leaderboards")
#' @param year Filter by year (optional)
#' @param tournament Filter by tournament name (optional, partial match)
#' @return Tibble with tournament data
#' @export
#' @examples
#' \dontrun{
#' # Load all saved data
#' all_data <- load_from_db()
#'
#' # Filter by year
#' data_2025 <- load_from_db(year = 2025)
#'
#' # Filter by tournament
#' masters <- load_from_db(tournament = "Masters")
#' }
load_from_db <- function(db_path = "data/golfastr.duckdb",
                         table_name = "leaderboards",
                         year = NULL,
                         tournament = NULL) {

  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' required. Install with: install.packages('duckdb')")
  }

  if (!file.exists(db_path)) {
    stop("Database not found: ", db_path,
         "\nUse save_to_db() to save tournament data first.")
  }

  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Build query
  query <- paste0("SELECT * FROM ", table_name)
  conditions <- c()

  if (!is.null(tournament)) {
    conditions <- c(conditions,
                    paste0("tournament_name ILIKE '%", tournament, "%'"))
  }

  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }

  result <- DBI::dbGetQuery(con, query)
  tibble::as_tibble(result)
}

#' Save Tournament Data to RDS
#'
#' Save tournament data to an RDS file.
#'
#' @param data Tibble of tournament data
#' @param file_path Path to RDS file (default: "data/golfastr.rds")
#' @param append If TRUE, append to existing data
#' @export
save_to_rds <- function(data,
                        file_path = "data/golfastr.rds",
                        append = TRUE) {

  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

  if (append && file.exists(file_path)) {
    existing <- readRDS(file_path)
    data <- dplyr::bind_rows(existing, data)
    # Remove duplicates based on key columns
    if ("athlete_id" %in% names(data) && "event_id" %in% names(data)) {
      data <- dplyr::distinct(data, athlete_id, event_id, .keep_all = TRUE)
    }
  }

  saveRDS(data, file_path)
  message("Saved ", nrow(data), " rows to ", file_path)
}

#' Load Tournament Data from RDS
#'
#' @param file_path Path to RDS file
#' @return Tibble with tournament data
#' @export
load_from_rds <- function(file_path = "data/golfastr.rds") {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  readRDS(file_path)
}
