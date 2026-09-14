#' Align leaderboard column names across hosted, live, and saved files.
#'
#' Older files used `display_name`, `event_id`, and `score_display`. Current
#' loaders use `player_name`, `tournament_id`, and `score_to_par`. Both sets
#' are filled in so analysis code can use either name.
#'
#' @param data A leaderboard-like data frame.
#' @return The same data with both naming conventions present when possible.
#' @noRd
standardize_leaderboard <- function(data) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(data)
  }

  nms <- names(data)

  if ("player_name" %in% nms && !"display_name" %in% nms) {
    data$display_name <- data$player_name
  }
  if ("display_name" %in% nms && !"player_name" %in% nms) {
    data$player_name <- data$display_name
  }

  if ("tournament_id" %in% nms && !"event_id" %in% nms) {
    data$event_id <- as.character(data$tournament_id)
  }
  if ("event_id" %in% nms && !"tournament_id" %in% nms) {
    data$tournament_id <- as.character(data$event_id)
  }

  if ("score_to_par" %in% nms && !"score_display" %in% nms) {
    data$score_display <- data$score_to_par
  }
  if ("score_display" %in% nms && !"score_to_par" %in% nms) {
    data$score_to_par <- data$score_display
  }

  data
}

#' Load or reuse leaderboard data for analysis helpers.
#'
#' @param data Optional data frame already in memory.
#' @param file_path Optional path to an .rds or .parquet file.
#' @param tournament Optional tournament name filter.
#' @return A standardized tibble.
#' @noRd
resolve_leaderboard_data <- function(data = NULL, file_path = NULL,
                                     tournament = NULL) {
  if (is.null(data)) {
    if (is.null(file_path)) {
      cli::cli_abort("Provide {.arg data} or {.arg file_path}.")
    }
    data <- load_data(file_path, tournament = tournament)
  } else if (!is.null(tournament) && "tournament_name" %in% names(data)) {
    data <- data[grepl(tournament, data$tournament_name, ignore.case = TRUE), ]
  }

  standardize_leaderboard(data)
}

#' Unique event identifiers from a leaderboard file.
#' @noRd
existing_event_ids <- function(data) {
  data <- standardize_leaderboard(data)
  if (!"event_id" %in% names(data)) {
    return(character(0))
  }
  unique(as.character(data$event_id))
}

#' Drop duplicate player-event rows after an append.
#' @noRd
dedupe_leaderboard <- function(data) {
  data <- standardize_leaderboard(data)
  if ("player_id" %in% names(data) && "event_id" %in% names(data)) {
    data <- dplyr::distinct(data, player_id, event_id, .keep_all = TRUE)
  }
  data
}
