#' List Available Tournaments
#'
#' Get a list of all available tournaments for a given year.
#'
#' @param year Season year (e.g., 2025)
#' @param tour Tour name: "pga" (default)
#' @return Tibble with event_id, tournament_name, start_date, end_date
#' @export
#' @examples
#' \dontrun{
#' # See all 2025 PGA tournaments
#' list_tournaments(2025)
#' }
list_tournaments <- function(year, tour = "pga") {

  tour <- match.arg(tour, choices = c("pga"))

  if (tour == "pga") {
    get_pga_schedule(year)
  }
}

#' Load Tournament Data
#'
#' Fetch leaderboard data for a tournament. You can specify either the
#' event_id or search by tournament name.
#'
#' @param year Season year (e.g., 2025)
#' @param tournament Tournament name (partial match) or event_id
#' @param tour Tour name: "pga" (default)
#' @return Tibble with tournament leaderboard
#' @export
#' @examples
#' \dontrun{
#' # Load by name (partial match works)
#' masters <- load_tournament(2025, "Masters")
#' pga_champ <- load_tournament(2025, "PGA Championship")
#'
#' # Load by event_id
#' masters <- load_tournament(2025, "401703504")
#' }
load_tournament <- function(year, tournament, tour = "pga") {
  tour <- match.arg(tour, choices = c("pga"))

  # Get schedule to find event_id

  schedule <- list_tournaments(year, tour)

  # Check if tournament is an event_id or a name

  if (tournament %in% schedule$event_id) {
    event_id <- tournament
    tournament_name <- schedule$tournament_name[schedule$event_id == event_id]
  } else {
    # Search by name (case-insensitive partial match)
    matches <- schedule[grepl(tournament, schedule$tournament_name, ignore.case = TRUE), ]

    if (nrow(matches) == 0) {
      stop("No tournament found matching '", tournament, "'. Use list_tournaments(",
           year, ") to see available tournaments.")
    }

    if (nrow(matches) > 1) {
      message("Multiple matches found:")
      print(matches[, c("event_id", "tournament_name")])
      stop("Please be more specific or use the event_id.")
    }

    event_id <- matches$event_id
    tournament_name <- matches$tournament_name
  }

  message("Loading: ", tournament_name, " (", event_id, ")")
  get_tournament_leaderboard(event_id)
}

#' Load Tournament with Hole-by-Hole Scores
#'
#' Fetch full tournament data including hole-by-hole scorecards.
#'
#' @param year Season year (e.g., 2025)
#' @param tournament Tournament name (partial match) or event_id
#' @param top_n Only fetch scorecards for top N finishers (default: 10)
#' @param tour Tour name: "pga" (default)
#' @return List with 'leaderboard' and 'scorecards' tibbles
#' @export
#' @examples
#' \dontrun{
#' # Get Masters with top 10 scorecards
#' masters_detail <- load_tournament_detail(2025, "Masters", top_n = 10)
#' }
load_tournament_detail <- function(year, tournament, top_n = 10, tour = "pga") {
  tour <- match.arg(tour, choices = c("pga"))

  # Get schedule to find event_id
  schedule <- list_tournaments(year, tour)

  # Check if tournament is an event_id or a name
  if (tournament %in% schedule$event_id) {
    event_id <- tournament
    tournament_name <- schedule$tournament_name[schedule$event_id == event_id]
  } else {
    matches <- schedule[grepl(tournament, schedule$tournament_name, ignore.case = TRUE), ]

    if (nrow(matches) == 0) {
      stop("No tournament found matching '", tournament, "'")
    }

    if (nrow(matches) > 1) {
      message("Multiple matches found:")
      print(matches[, c("event_id", "tournament_name")])
      stop("Please be more specific or use the event_id.")
    }

    event_id <- matches$event_id
    tournament_name <- matches$tournament_name
  }

  message("Loading: ", tournament_name, " (", event_id, ") with top ", top_n, " scorecards")

  # Get leaderboard
  leaderboard <- fetch_leaderboard_fast(event_id, year, tour)

  # Get scorecards for top_n players
  if (!is.null(top_n) && top_n < nrow(leaderboard)) {
    athlete_ids <- leaderboard$player_id[1:top_n]
  } else {
    athlete_ids <- leaderboard$player_id
  }

  scorecards_list <- lapply(athlete_ids, function(aid) {
    tryCatch(
      fetch_player_holes(event_id, aid, tour),
      error = function(e) NULL
    )
  })
  scorecards <- dplyr::bind_rows(scorecards_list)

  list(leaderboard = leaderboard, scorecards = scorecards)
}
