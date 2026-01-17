#' Load Tournament Leaderboard
#'
#' Retrieves leaderboard data for tournaments. Can load a single tournament
#' or all tournaments for a year.
#'
#' @param year Season year (e.g., 2026). Defaults to current year.
#' @param tournament Tournament identifier - either event_id or partial name match.
#'   If NULL, returns all tournaments for the year.
#' @param tour Tour identifier. Currently supports "pga" (default).
#' @return A tibble with leaderboard data including:
#'   \itemize{
#'     \item \code{position}: Final standing
#'     \item \code{player_id}: ESPN athlete ID
#'     \item \code{player_name}: Player display name
#'     \item \code{total_score}: Total strokes
#'     \item \code{score_to_par}: Score relative to par (e.g., "-11")
#'     \item \code{tournament_id}: Event ID
#'     \item \code{tournament_name}: Tournament name
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Load specific tournament by name
#' sony <- load_leaderboard(2026, "Sony")
#'
#' # Load by event ID
#' lb <- load_leaderboard(2026, "401703504")
#' }
load_leaderboard <- function(year = as.integer(format(Sys.Date(), "%Y")),
                             tournament = NULL,
                             tour = "pga") {

  # Get schedule to find tournament(s)
  schedule <- load_schedule(year, tour)

  if (!is.null(tournament)) {
    # Filter to matching tournament
    if (tournament %in% schedule$event_id) {
      schedule <- schedule[schedule$event_id == tournament, ]
    } else {
      # Try name match
      matches <- grepl(tournament, schedule$tournament_name, ignore.case = TRUE)
      if (!any(matches)) {
        stop("Tournament not found: ", tournament)
      }
      schedule <- schedule[matches, ]
    }
  }

  if (nrow(schedule) == 0) {
    return(tibble::tibble())
  }

  # Fetch leaderboard for each tournament
  all_data <- list()

  for (i in seq_len(nrow(schedule))) {
    event_id <- schedule$event_id[i]
    tourn_name <- schedule$tournament_name[i]

    tryCatch({
      lb <- fetch_leaderboard_fast(event_id, year, tour)
      if (nrow(lb) > 0) {
        lb$tournament_id <- event_id
        lb$tournament_name <- tourn_name
        lb$year <- year
        all_data[[length(all_data) + 1]] <- lb
      }
    }, error = function(e) {
      message(sprintf("Failed to load %s: %s", tourn_name, e$message))
    })
  }

  if (length(all_data) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(all_data)
}

#' Fast leaderboard fetch using site API
#' @keywords internal
fetch_leaderboard_fast <- function(event_id, year, tour = "pga") {
  # Use site API which returns more data in single call
  response <- espn_site_request(
    paste0("scoreboard/", event_id),
    tour = tour
  )

  # competitions is a data.frame, access first row's competitors
  competitions <- response$competitions
  if (is.null(competitions) || nrow(competitions) == 0) {
    return(tibble::tibble())
  }

  # competitors is a list column in the data.frame
  competitors <- competitions$competitors[[1]]

  if (is.null(competitors) || nrow(competitors) == 0) {
    return(tibble::tibble())
  }

  n_players <- nrow(competitors)

  # score field contains score-to-par (e.g., "-11", "E", "+3")
  score_to_par <- competitors$score

  # Calculate total strokes from round scores in linescores
  total_score <- tryCatch({
    sapply(seq_len(n_players), function(i) {
      ls <- competitors$linescores[[i]]
      if (!is.null(ls) && is.data.frame(ls) && nrow(ls) > 0) {
        # Sum only completed rounds (periods 1-4)
        round_scores <- ls$value[ls$period %in% 1:4]
        round_scores <- round_scores[!is.na(round_scores)]
        if (length(round_scores) > 0) sum(round_scores) else NA_integer_
      } else {
        NA_integer_
      }
    })
  }, error = function(e) rep(NA_integer_, n_players))

  # Handle status safely
  status <- tryCatch({
    competitors$status$type$description
  }, error = function(e) rep(NA_character_, n_players))

  tibble::tibble(
    position = as.integer(competitors$order),
    player_id = as.character(competitors$id),
    player_name = competitors$athlete$displayName,
    total_score = as.integer(total_score),
    score_to_par = score_to_par,
    status = status
  )
}

#' @describeIn load_leaderboard Legacy function for backwards compatibility
#' @param event_id ESPN event identifier (for legacy function)
#' @export
get_tournament_leaderboard <- function(event_id) {
  fetch_leaderboard_fast(event_id, as.integer(format(Sys.Date(), "%Y")), "pga")
}
