#' Load Player Directory
#'
#' Retrieves a directory of players for a tour. Since ESPN doesn't have
#' a dedicated player directory endpoint, this aggregates players from
#' recent tournament leaderboards.
#'
#' @param year Season year to pull players from. Defaults to current year.
#' @param tour Tour identifier. Currently supports "pga" (default).
#' @return A tibble with player data including:
#'   \itemize{
#'     \item \code{player_id}: ESPN athlete ID
#'     \item \code{player_name}: Player display name
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Get player directory
#' players <- load_players()
#' }
load_players <- function(year = as.integer(format(Sys.Date(), "%Y")),
                         tour = "pga") {

  # Get schedule

  schedule <- load_schedule(year, tour)

  if (nrow(schedule) == 0) {
    return(tibble::tibble(player_id = character(), player_name = character()))
  }

  # Get players from recent completed tournaments
  all_players <- list()

  # Try up to 5 recent tournaments to build player list
  for (i in seq_len(min(5, nrow(schedule)))) {
    event_id <- schedule$event_id[i]

    tryCatch({
      lb <- fetch_leaderboard_fast(event_id, year, tour)
      if (nrow(lb) > 0) {
        all_players[[length(all_players) + 1]] <- tibble::tibble(
          player_id = lb$player_id,
          player_name = lb$player_name
        )
      }
    }, error = function(e) {
      # Skip tournaments that fail
    })
  }

  if (length(all_players) == 0) {
    return(tibble::tibble(player_id = character(), player_name = character()))
  }

  # Combine and deduplicate
  players <- dplyr::bind_rows(all_players)
  players <- dplyr::distinct(players, player_id, .keep_all = TRUE)
  players <- dplyr::arrange(players, player_name)

  players
}
