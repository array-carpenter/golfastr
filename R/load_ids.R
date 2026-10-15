#' Load Player IDs
#'
#' Links ESPN player IDs (used by [load_holes()], [load_leaderboard()] and
#' [add_win_probability()]) to PGA Tour player IDs (used by [load_shots()],
#' [load_tee_times()] and [load_strokes_gained()]).
#'
#' Pairs come from events that appear in both sources (2012 on): a player
#' with the same name in the same event votes for a pair, and a pair needs at
#' least two such events. IDs that pair with more than one partner (for
#' example two players with the same name) are left out rather than guessed.
#'
#' @param player Optional player name filter (partial match,
#'   case-insensitive).
#' @return A tibble with \code{player_name}, \code{espn_player_id},
#'   \code{pga_player_id}, \code{first_season}, \code{last_season} and
#'   \code{events_matched}.
#' @export
#' @examples
#' \donttest{
#' load_player_ids("Scheffler")
#' }
load_player_ids <- function(player = NULL) {
  data <- ids_load("player_ids")
  if (!is.null(player) && nrow(data)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
  }
  data
}

#' Load Event IDs
#'
#' Links ESPN tournament IDs to PGA Tour tournament IDs for events in both
#' sources.
#'
#' @param year Optional season filter.
#' @return A tibble with \code{espn_tournament_id}, \code{pga_tournament_id},
#'   \code{year}, \code{tournament_name}, \code{pga_tournament_name},
#'   \code{start_date} and \code{field_overlap} (share of the smaller field
#'   found in both sources).
#' @export
#' @examples
#' \donttest{
#' load_event_ids(2025)
#' }
load_event_ids <- function(year = NULL) {
  data <- ids_load("event_ids")
  if (!is.null(year) && nrow(data)) {
    data <- data[data$year %in% year, ]
  }
  data
}

#' @noRd
ids_load <- function(name) {
  data <- release_load("ids", paste0(name, ".rds"))
  if (is.null(data)) {
    cli::cli_inform("ID crosswalk could not be downloaded.")
    return(tibble::tibble())
  }
  tibble::as_tibble(data)
}
