#' Get Player Results
#'
#' Look up a player's results across tournaments. Pass a data frame from
#' [load_leaderboard()] or a local file created with [save_to_rds()].
#'
#' @param name Player name (partial match, case-insensitive)
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with player's tournament results
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' get_player("McIlroy", data = season)
#' get_player("Scheffler", file_path = "golf_data.rds")
#' }
get_player <- function(name, file_path = NULL, data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path)

  matches <- data[grepl(name, data$display_name, ignore.case = TRUE), ]

  if (nrow(matches) == 0) {
    cli::cli_abort("No player found matching {.val {name}}.")
  }

  unique_players <- unique(matches$display_name)
  if (length(unique_players) > 1) {
    cli::cli_inform("Multiple players found: {unique_players}")
  }

  matches <- matches[order(matches$event_id), ]

  result <- matches[, c("tournament_name", "position", "score_display",
                        "total_score", "display_name", "event_id", "year")]

  result
}

#' Get Tournament Winners
#'
#' Get all tournament winners from leaderboard data.
#'
#' @param year Optional year filter
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with tournament winners
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' get_winners(data = season)
#' get_winners(2025, file_path = "golf_data.rds")
#' }
get_winners <- function(year = NULL, file_path = NULL, data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path)

  winners <- data[data$position == 1, ]

  if (!is.null(year)) {
    winners <- winners[winners$year == year, ]
  }

  winners <- winners[order(winners$event_id), ]

  result <- winners[, c("tournament_name", "display_name", "score_display",
                        "total_score", "event_id", "year")]

  result
}

#' Get Major Championships
#'
#' Get results from the four major championships.
#'
#' @param year Season year
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with major championship results
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' get_majors(2025, data = season)
#' }
get_majors <- function(year, file_path = NULL, data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path)
  data <- data[data$year == year, ]

  major_patterns <- c(
    "Masters",
    "PGA Championship",
    "U.S. Open",
    "Open Championship|British Open"
  )

  is_major <- grepl(paste(major_patterns, collapse = "|"),
                    data$tournament_name, ignore.case = TRUE)
  majors <- data[is_major, ]

  if (nrow(majors) == 0) {
    cli::cli_inform("No major championships found for {year}. They may not be loaded yet.")
    return(tibble::tibble())
  }

  majors <- majors[order(majors$tournament_name, majors$position), ]

  majors
}
