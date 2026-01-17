#' Get Player Results
#'
#' Look up a player's results across all tournaments in the database.
#'
#' @param name Player name (partial match, case-insensitive)
#' @param db_path Path to database file
#' @return Tibble with player's tournament results
#' @export
#' @examples
#' \dontrun{
#' # Get Rory McIlroy's results
#' get_player("McIlroy")
#'
#' # Get Scottie Scheffler's results
#' get_player("Scheffler")
#' }
get_player <- function(name, db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path)

  # Filter by name (case-insensitive partial match)
  matches <- data[grepl(name, data$display_name, ignore.case = TRUE), ]

  if (nrow(matches) == 0) {
    stop("No player found matching '", name, "'")
  }

  # Check if multiple players matched
  unique_players <- unique(matches$display_name)
  if (length(unique_players) > 1) {
    message("Multiple players found: ", paste(unique_players, collapse = ", "))
  }

  # Sort by tournament date (using event_id as proxy)
  matches <- matches[order(matches$event_id), ]

  # Select useful columns
  result <- matches[, c("tournament_name", "position", "score_display",
                        "total_score", "display_name", "event_id", "year")]

  return(result)
}

#' Get Tournament Winners
#'
#' Get all tournament winners from the database.
#'
#' @param year Optional year filter
#' @param db_path Path to database file
#' @return Tibble with tournament winners
#' @export
#' @examples
#' \dontrun{
#' # Get all winners
#' get_winners()
#'
#' # Get 2025 winners
#' get_winners(2025)
#' }
get_winners <- function(year = NULL, db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path)

  # Filter to winners only
  winners <- data[data$position == 1, ]

  # Filter by year if specified
 if (!is.null(year)) {
    winners <- winners[winners$year == year, ]
  }

  # Sort by event_id
  winners <- winners[order(winners$event_id), ]

  # Select useful columns
  result <- winners[, c("tournament_name", "display_name", "score_display",
                        "total_score", "event_id", "year")]

  return(result)
}

#' Get Major Championships
#'
#' Get results from the four major championships.
#'
#' @param year Season year
#' @param db_path Path to database file
#' @return Tibble with major championship results
#' @export
#' @examples
#' \dontrun{
#' # Get 2025 majors
#' get_majors(2025)
#' }
get_majors <- function(year, db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path)

  # Filter by year
  data <- data[data$year == year, ]

  # Major championship patterns
  major_patterns <- c(
    "Masters",
    "PGA Championship",
    "U.S. Open",
    "Open Championship|British Open"
  )

  # Filter to majors
  is_major <- grepl(paste(major_patterns, collapse = "|"),
                    data$tournament_name, ignore.case = TRUE)
  majors <- data[is_major, ]

  if (nrow(majors) == 0) {
    message("No major championships found for ", year,
            ". They may not be loaded yet.")
    return(tibble::tibble())
  }

  # Sort by position within each tournament
  majors <- majors[order(majors$tournament_name, majors$position), ]

  return(majors)
}
