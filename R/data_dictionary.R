#' Get Field Descriptions for PGA Data
#'
#' Returns a data frame with field names and descriptions for
#' leaderboard or hole-by-hole data.
#'
#' @param data_type Character. Either "leaderboard" or "holes".
#' @return A tibble with field and description columns.
#' @export
#' @examples
#' pga_field_descriptions("leaderboard")
#' pga_field_descriptions("holes")
pga_field_descriptions <- function(data_type = c("leaderboard", "holes")) {
  data_type <- match.arg(data_type)

  if (data_type == "leaderboard") {
    tibble::tibble(
      field = c(
        "athlete_id",
        "athlete_uid",
        "position",
        "movement",
        "amateur",
        "full_name",
        "display_name",
        "total_score",
        "score_display",
        "tournament_name",
        "event_id",
        "year",
        "round1",
        "round2",
        "round3",
        "round4",
        "total",
        "thru",
        "today"
      ),
      description = c(
        "ESPN athlete ID",
        "ESPN unique identifier for athlete",
        "Current tournament position (1, 2, T3, etc.)",
        "Position change from previous round (numeric)",
        "Whether player is an amateur (TRUE/FALSE)",
        "Player's full name",
        "Player's display name",
        "Total score relative to par (numeric)",
        "Displayed score string (e.g., '-10', 'E', '+5')",
        "Name of the tournament",
        "ESPN event ID",
        "Tournament year",
        "Round 1 score",
        "Round 2 score",
        "Round 3 score",
        "Round 4 score",
        "Total strokes",
        "Holes completed in current round",
        "Today's score relative to par"
      )
    )
  } else {
    tibble::tibble(
      field = c(
        "athlete_id",
        "athlete_name",
        "event_id",
        "tournament_name",
        "round_num",
        "hole_num",
        "par",
        "score",
        "score_type",
        "score_display",
        "cumulative",
        "year"
      ),
      description = c(
        "ESPN athlete ID",
        "Player's name",
        "ESPN event ID",
        "Name of the tournament",
        "Round number (1-4)",
        "Hole number (1-18)",
        "Par for the hole",
        "Strokes taken on the hole",
        "Score classification (EAGLE, BIRDIE, PAR, BOGEY, etc.)",
        "Display string for the score",
        "Cumulative score through the hole",
        "Tournament year"
      )
    )
  }
}


#' Get PGA Score Types
#'
#' Returns a data frame with score type classifications
#' used in hole-by-hole data.
#'
#' @return A tibble with score_type, strokes_vs_par, and description columns.
#' @export
#' @examples
#' pga_score_types()
pga_score_types <- function() {
  tibble::tibble(
    score_type = c(
      "DOUBLE_EAGLE",
      "EAGLE",
      "BIRDIE",
      "PAR",
      "BOGEY",
      "DOUBLE_BOGEY",
      "TRIPLE_BOGEY",
      "OTHER"
    ),
    strokes_vs_par = c(-3L, -2L, -1L, 0L, 1L, 2L, 3L, NA_integer_),
    description = c(
      "Three under par (also called albatross)",
      "Two under par",
      "One under par",
      "Equal to par",
      "One over par",
      "Two over par",
      "Three over par",
      "Four or more over par"
    )
  )
}


#' Get PGA Major Championships
#'
#' Returns a data frame with the four major championships
#' and their typical schedule.
#'
#' @return A tibble with tournament, month, and course columns.
#' @export
#' @examples
#' pga_majors()
pga_majors <- function() {
  tibble::tibble(
    tournament = c(
      "Masters Tournament",
      "PGA Championship",
      "U.S. Open",
      "The Open Championship"
    ),
    month = c("April", "May", "June", "July"),
    course = c(
      "Augusta National Golf Club",
      "Varies",
      "Varies",
      "Varies (UK links courses)"
    )
  )
}
