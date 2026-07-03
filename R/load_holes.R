#' Load Hole-by-Hole Scoring
#'
#' Retrieves hole-by-hole scoring data for tournaments.
#'
#' For PGA Tour data, completed tournaments are served from pre-built
#' full-field files hosted on GitHub releases (updated daily), avoiding
#' the one-request-per-player cost of the live ESPN API. The live API is
#' used as a fallback for in-progress tournaments, other tours, or when
#' the hosted data is unavailable.
#'
#' @param year Season year (e.g., 2026). Defaults to current year.
#' @param tournament Tournament identifier - either event_id or partial name match.
#' @param top_n Number of top finishers to include. Default NULL returns all.
#' @param tour Tour identifier. Currently supports "pga" (default).
#' @param live If TRUE, skip the hosted data and fetch directly from the
#'   ESPN API. Defaults to FALSE.
#' @return A tibble with hole-by-hole data including:
#'   \itemize{
#'     \item \code{player_id}: ESPN athlete ID
#'     \item \code{player_name}: Player display name
#'     \item \code{position}: Final leaderboard standing
#'     \item \code{round}: Round number (1-4)
#'     \item \code{hole}: Hole number (1-18)
#'     \item \code{par}: Par for the hole
#'     \item \code{score}: Player's score on the hole
#'     \item \code{score_type}: BIRDIE, PAR, BOGEY, EAGLE, etc.
#'     \item \code{tournament_id}: Event ID
#'   }
#' @export
#' @examples
#' \donttest{
#' # Load hole-by-hole for top 10 at Sony Open
#' holes <- load_holes(2026, "Sony", top_n = 10)
#' }
load_holes <- function(year = as.integer(format(Sys.Date(), "%Y")),
                       tournament = NULL,
                       top_n = NULL,
                       tour = "pga",
                       live = FALSE) {

  if (is.null(tournament)) {
    cli::cli_abort("{.arg tournament} is required for {.fn load_holes}.")
  }

  # Try pre-built data from GitHub releases first
  if (!live && tolower(tour) == "pga") {
    hosted <- release_load("holes", sprintf("holes_%d.rds", year))
    if (!is.null(hosted)) {
      if (tournament %in% hosted$tournament_id) {
        result <- hosted[hosted$tournament_id == tournament, ]
      } else {
        matches <- grepl(tournament, hosted$tournament_name, ignore.case = TRUE)
        result <- hosted[matches, ]
      }
      if (nrow(result) > 0) {
        if (!is.null(top_n)) {
          players <- result[!duplicated(result$player_id),
                            c("player_id", "position")]
          players <- players[order(players$position), ]
          keep <- players$player_id[seq_len(min(top_n, nrow(players)))]
          result <- result[result$player_id %in% keep, ]
        }
        return(result)
      }
      cli::cli_inform("Tournament not in hosted data (may be in progress); fetching from ESPN...")
    }
  }

  # Get schedule to find tournament
  schedule <- load_schedule(year, tour)

  # Find matching tournament
  if (tournament %in% schedule$event_id) {
    event_id <- tournament
    tourn_name <- schedule$tournament_name[schedule$event_id == tournament]
  } else {
    matches <- grepl(tournament, schedule$tournament_name, ignore.case = TRUE)
    if (!any(matches)) {
      cli::cli_abort(c(
        "Tournament not found: {.val {tournament}}.",
        "i" = "Use {.code load_schedule({year})} to see the season schedule."
      ))
    }
    event_id <- schedule$event_id[matches][1]
    tourn_name <- schedule$tournament_name[matches][1]
  }

  # Get leaderboard to find player IDs
  leaderboard <- fetch_leaderboard_fast(event_id, year, tour)

  if (nrow(leaderboard) == 0) {
    return(tibble::tibble())
  }

  # Filter to top_n if specified
  if (!is.null(top_n)) {
    leaderboard <- leaderboard[1:min(top_n, nrow(leaderboard)), ]
  }

  # Fetch scorecards for each player
  cli::cli_inform("Fetching scorecards for {nrow(leaderboard)} players...")

  all_holes <- list()

  for (i in seq_len(nrow(leaderboard))) {
    player_id <- leaderboard$player_id[i]
    player_name <- leaderboard$player_name[i]

    tryCatch({
      scorecard <- fetch_player_holes(event_id, player_id, tour)
      if (nrow(scorecard) > 0) {
        scorecard$player_id <- player_id
        scorecard$player_name <- player_name
        scorecard$position <- leaderboard$position[i]
        scorecard$tournament_id <- event_id
        scorecard$tournament_name <- tourn_name
        scorecard$year <- year
        all_holes[[length(all_holes) + 1]] <- scorecard
      }
    }, error = function(e) {
      # Silently skip players without scorecard data
    })
  }

  if (length(all_holes) == 0) {
    return(tibble::tibble())
  }

  result <- dplyr::bind_rows(all_holes)

  # Reorder columns
  dplyr::select(result,
    player_id, player_name, position, tournament_id, tournament_name,
    round, hole, par, score, score_type, dplyr::everything()
  )
}

#' Fetch hole-by-hole for a single player
#' @keywords internal
fetch_player_holes <- function(event_id, player_id, tour = "pga") {
  endpoint <- paste0("events/", event_id, "/competitions/", event_id,
                     "/competitors/", player_id, "/linescores")

  data <- espn_core_request(endpoint, tour = tour)

  if (is.null(data$items) || length(data$items) == 0) {
    return(tibble::tibble())
  }

  num_rounds <- nrow(data$items)
  all_rounds <- list()

  for (r in seq_len(num_rounds)) {
    round_num <- data$items$period[r]
    holes <- data$items$linescores[[r]]

    if (is.null(holes) || length(holes) == 0) next

    round_data <- tibble::tibble(
      round = as.integer(round_num),
      hole = as.integer(holes$period),
      par = as.integer(holes$par),
      score = as.integer(holes$value),
      score_type = holes$scoreType$name
    )
    all_rounds[[length(all_rounds) + 1]] <- round_data
  }

  if (length(all_rounds) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(all_rounds)
}

#' @describeIn load_holes Deprecated, use `load_holes()` instead
#' @param event_id ESPN event identifier (for legacy function)
#' @param athlete_id ESPN athlete identifier (for legacy function)
#' @return A tibble with hole-by-hole scoring data
#' @export
get_player_scorecards <- function(event_id, athlete_id) {
  deprecate_warn("0.2.0", "get_player_scorecards()", "load_holes()")
  fetch_player_holes(event_id, athlete_id, "pga")
}
