#' Save Tournament Data to RDS
#'
#' Save tournament data to an RDS file.
#'
#' @param data Tibble of tournament data
#' @param file_path Path to RDS file. Must be specified by user.
#' @param append If TRUE, append to existing data
#' @return Invisible NULL. Called for side effects (writes to file).
#' @export
#' @examples
#' \dontrun{
#' masters <- load_tournament(2025, "Masters")
#' save_to_rds(masters, file_path = tempfile(fileext = ".rds"))
#' }
save_to_rds <- function(data,
                        file_path,
                        append = TRUE) {

  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

  if (append && file.exists(file_path)) {
    existing <- readRDS(file_path)
    data <- dplyr::bind_rows(existing, data)
    # Remove duplicates based on key columns
    if ("player_id" %in% names(data) && "event_id" %in% names(data)) {
      data <- dplyr::distinct(data, player_id, event_id, .keep_all = TRUE)
    }
  }

  saveRDS(data, file_path)
  message("Saved ", nrow(data), " rows to ", file_path)
  invisible(NULL)
}

#' Load Tournament Data from RDS
#'
#' @param file_path Path to RDS file. Must be specified by user.
#' @return A tibble with tournament leaderboard data containing columns such as
#'   position, player name, scores, and tournament metadata.
#' @export
#' @examples
#' \dontrun{
#' data <- load_from_rds(file_path = "my_golf_data.rds")
#' }
load_from_rds <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  tibble::as_tibble(readRDS(file_path))
}

#' Save Tournament Data to Parquet
#'
#' Save tournament data to a Parquet file for cross-language compatibility.
#' Requires the arrow package.
#'
#' @param data Tibble of tournament data
#' @param file_path Path to Parquet file. Must be specified by user.
#' @param append If TRUE, append to existing data
#' @return Invisible NULL. Called for side effects (writes to file).
#' @export
#' @examples
#' \dontrun{
#' masters <- load_tournament(2025, "Masters")
#' save_to_parquet(masters, file_path = tempfile(fileext = ".parquet"))
#' }
save_to_parquet <- function(data,
                            file_path,
                            append = TRUE) {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' required. Install with: install.packages('arrow')")
  }

  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

  if (append && file.exists(file_path)) {
    existing <- arrow::read_parquet(file_path)
    data <- dplyr::bind_rows(existing, data)
    # Remove duplicates based on key columns
    if ("player_id" %in% names(data) && "event_id" %in% names(data)) {
      data <- dplyr::distinct(data, player_id, event_id, .keep_all = TRUE)
    }
  }

  arrow::write_parquet(data, file_path)
  message("Saved ", nrow(data), " rows to ", file_path)
  invisible(NULL)
}

#' Load Tournament Data from Parquet
#'
#' Load tournament data from a Parquet file. Requires the arrow package.
#'
#' @param file_path Path to Parquet file. Must be specified by user.
#' @return A tibble with tournament leaderboard data containing columns such as
#'   position, player name, scores, and tournament metadata.
#' @export
#' @examples
#' \dontrun{
#' data <- load_from_parquet(file_path = "my_golf_data.parquet")
#' }
load_from_parquet <- function(file_path) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' required. Install with: install.packages('arrow')")
  }

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  tibble::as_tibble(arrow::read_parquet(file_path))
}

#' Load Tournament Data
#'
#' Load tournament data from a file. Auto-detects format based on extension.
#'
#' @param file_path Path to data file (.rds or .parquet)
#' @param tournament Optional tournament name filter (partial match)
#' @return A tibble with tournament leaderboard data
#' @export
#' @examples
#' \dontrun{
#' # Load all data
#' data <- load_data("golf_data.rds")
#'
#' # Load and filter to Masters
#' masters <- load_data("golf_data.rds", tournament = "Masters")
#' }
load_data <- function(file_path, tournament = NULL) {
  ext <- tolower(tools::file_ext(file_path))

  data <- if (ext == "parquet") {
    load_from_parquet(file_path)
  } else if (ext == "rds") {
    load_from_rds(file_path)
  } else {
    stop("Unsupported file type: ", ext, ". Use .rds or .parquet")
  }

  if (!is.null(tournament)) {
    data <- data[grepl(tournament, data$tournament_name, ignore.case = TRUE), ]
  }

  data
}
