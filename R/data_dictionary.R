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
        "position",
        "player_id",
        "player_name",
        "total_score",
        "score_to_par",
        "status",
        "tournament_id",
        "tournament_name",
        "year"
      ),
      description = c(
        "Final tournament standing",
        "ESPN athlete ID",
        "Player display name",
        "Total strokes across completed rounds",
        "Score relative to par (e.g., '-11', 'E', '+5')",
        "Player status (live API only)",
        "ESPN event ID",
        "Name of the tournament",
        "Tournament year"
      )
    )
  } else {
    tibble::tibble(
      field = c(
        "player_id",
        "player_name",
        "position",
        "tournament_id",
        "tournament_name",
        "round",
        "hole",
        "par",
        "score",
        "score_type",
        "year",
        "field_avg",
        "to_par",
        "vs_field",
        "cume_to_par"
      ),
      description = c(
        "ESPN athlete ID",
        "Player display name",
        "Final leaderboard standing",
        "ESPN event ID",
        "Name of the tournament",
        "Round number (5 = playoff)",
        "Hole number (1-18)",
        "Par for the hole",
        "Strokes taken on the hole",
        "Score classification (EAGLE, BIRDIE, PAR, BOGEY, etc.)",
        "Tournament year",
        "Field scoring average on the hole in that round (hosted data only)",
        "Score relative to par on the hole (hosted data only)",
        "Strokes vs the field average on the hole (hosted data only)",
        "Running tournament score to par through the hole, playoff holes included (hosted data only)"
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
