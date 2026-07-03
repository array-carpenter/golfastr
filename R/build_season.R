#' Build Season Data File
#'
#' Incrementally load all tournaments for a season into a local file.
#' Skips tournaments already saved. Safe to interrupt and resume.
#'
#' @param year Season year
#' @param file_path Path to data file (.rds or .parquet). Must be specified by user.
#' @param tour Tour name (default: "pga")
#' @return Invisibly returns number of tournaments added (integer).
#' @export
#' @examples
#' \dontrun{
#' # Build 2025 season (run multiple times if needed)
#' build_season(2025, file_path = tempfile(fileext = ".rds"))
#' }
build_season <- function(year, file_path, tour = "pga") {

  # Determine format from extension
  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% c("rds", "parquet")) {
    cli::cli_abort("{.arg file_path} must end in {.val .rds} or {.val .parquet}.")
  }

  # Get all tournaments for the year
  schedule <- list_tournaments(year, tour)
  cli::cli_inform("Found {nrow(schedule)} tournaments for {year}.")

  # Check what's already in file
  existing_events <- character(0)
  if (file.exists(file_path)) {
    tryCatch({
      existing <- if (ext == "rds") load_from_rds(file_path) else load_from_parquet(file_path)
      existing_events <- unique(existing$event_id)
      cli::cli_inform("Already saved: {length(existing_events)} tournaments.")
    }, error = function(e) NULL)
  }

  # Find missing tournaments
  missing <- schedule[!schedule$event_id %in% existing_events, ]
  cli::cli_inform("Remaining to fetch: {nrow(missing)} tournaments.")

  if (nrow(missing) == 0) {
    cli::cli_inform("All tournaments already loaded.")
    return(invisible(0))
  }

  # Load each missing tournament
  added <- 0
  for (i in seq_len(nrow(missing))) {
    event_id <- missing$event_id[i]
    tournament_name <- missing$tournament_name[i]

    cli::cli_inform("[{i}/{nrow(missing)}] {tournament_name}")

    tryCatch({
      # Fetch leaderboard
      data <- fetch_leaderboard_fast(event_id, as.integer(year), tour)

      # Add metadata
      data$tournament_name <- tournament_name
      data$event_id <- event_id
      data$year <- year

      # Save to file
      if (ext == "rds") {
        save_to_rds(data, file_path = file_path, append = TRUE)
      } else {
        save_to_parquet(data, file_path = file_path, append = TRUE)
      }
      added <- added + 1
      cli::cli_inform("  Saved.")

    }, error = function(e) {
      cli::cli_inform("  ERROR: {conditionMessage(e)}")
    })

    # Small delay to be nice to the API
    Sys.sleep(0.5)
  }

  cli::cli_inform("Done.")
  cli::cli_inform("Added {added} tournaments.")

  invisible(added)
}

#' Check Season Progress
#'
#' See which tournaments are loaded vs missing for a season.
#'
#' @param year Season year
#' @param file_path Path to data file (.rds or .parquet). Must be specified by user.
#' @param tour Tour name (default: "pga")
#' @return A tibble showing status of each tournament with columns: event_id,
#'   tournament_name, start_date, end_date, and status (either "loaded" or "missing").
#' @export
#' @examples
#' \dontrun{
#' progress <- check_season(2025, file_path = "my_golf_data.rds")
#' }
check_season <- function(year, file_path, tour = "pga") {

  # Determine format from extension
  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% c("rds", "parquet")) {
    cli::cli_abort("{.arg file_path} must end in {.val .rds} or {.val .parquet}.")
  }

  schedule <- list_tournaments(year, tour)

  # Check file
  existing_events <- character(0)
  if (file.exists(file_path)) {
    tryCatch({
      existing <- if (ext == "rds") load_from_rds(file_path) else load_from_parquet(file_path)
      existing_events <- unique(existing$event_id)
    }, error = function(e) NULL)
  }

  schedule$status <- ifelse(schedule$event_id %in% existing_events,
                            "loaded", "missing")

  cli::cli_inform("Season {year} progress:")
  cli::cli_inform("  Loaded: {sum(schedule$status == 'loaded')}")
  cli::cli_inform("  Missing: {sum(schedule$status == 'missing')}")

  schedule
}
