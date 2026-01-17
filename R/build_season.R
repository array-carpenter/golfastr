#' Build Season Database
#'
#' Incrementally load all tournaments for a season into the database.
#' Skips tournaments already saved. Safe to interrupt and resume.
#'
#' @param year Season year
#' @param tour Tour name (default: "pga")
#' @param db_path Path to database file
#' @return Invisibly returns number of tournaments added
#' @export
#' @examples
#' \dontrun{
#' # Build 2025 season (run multiple times if needed)
#' build_season(2025)
#' }
build_season <- function(year, tour = "pga", db_path = "data/golfastr.duckdb") {

  # Get all tournaments for the year
  schedule <- list_tournaments(year, tour)
  cat("Found", nrow(schedule), "tournaments for", year, "\n\n")

 # Check what's already in database
  existing_events <- character(0)
  if (file.exists(db_path)) {
    tryCatch({
      existing <- load_from_db(db_path)
      existing_events <- unique(existing$event_id)
      cat("Already in database:", length(existing_events), "tournaments\n")
    }, error = function(e) NULL)
  }

  # Find missing tournaments
  missing <- schedule[!schedule$event_id %in% existing_events, ]
  cat("Remaining to fetch:", nrow(missing), "tournaments\n\n")

  if (nrow(missing) == 0) {
    cat("All tournaments already loaded!\n")
    return(invisible(0))
  }

  # Load each missing tournament
  added <- 0
  for (i in 1:nrow(missing)) {
    event_id <- missing$event_id[i]
    tournament_name <- missing$tournament_name[i]

    cat(sprintf("[%d/%d] %s\n", i, nrow(missing), tournament_name))

    tryCatch({
      # Fetch leaderboard
      data <- get_tournament_leaderboard(event_id)

      # Add metadata
      data$tournament_name <- tournament_name
      data$event_id <- event_id
      data$year <- year

      # Save to database
      save_to_db(data, db_path = db_path, append = TRUE)
      added <- added + 1
      cat("  Saved!\n\n")

    }, error = function(e) {
      cat("  ERROR:", e$message, "\n\n")
    })

    # Small delay to be nice to the API
    Sys.sleep(0.5)
  }

  cat("\n=== Done ===\n")
  cat("Added", added, "tournaments\n")

  return(invisible(added))
}

#' Check Season Progress
#'
#' See which tournaments are loaded vs missing for a season.
#'
#' @param year Season year
#' @param tour Tour name (default: "pga")
#' @param db_path Path to database file
#' @return Tibble showing status of each tournament
#' @export
check_season <- function(year, tour = "pga", db_path = "data/golfastr.duckdb") {

  schedule <- list_tournaments(year, tour)

  # Check database
  existing_events <- character(0)
  if (file.exists(db_path)) {
    tryCatch({
      existing <- load_from_db(db_path)
      existing_events <- unique(existing$event_id)
    }, error = function(e) NULL)
  }

  schedule$status <- ifelse(schedule$event_id %in% existing_events,
                            "loaded", "missing")

  cat("Season", year, "progress:\n")
  cat("  Loaded:", sum(schedule$status == "loaded"), "\n")
  cat("  Missing:", sum(schedule$status == "missing"), "\n\n")

  return(schedule)
}
