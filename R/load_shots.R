#' Load Shot-Level Data
#'
#' Returns every stroke from PGA Tour ShotLink for a season: where each shot
#' started and finished, its distance, and launch monitor (radar) readings
#' when the tour captured them.
#'
#' Data comes from pre-built files on the package's GitHub releases, built
#' from the PGA Tour's shot tracking feed. Older seasons have strokes, lies
#' and distances but no coordinates or radar data. Events without ShotLink
#' coverage are not included.
#'
#' @param year Season year (e.g., 2025).
#' @param tournament Optional PGA Tour tournament ID (e.g., `"R2025011"`) or
#'   partial tournament name match.
#' @param player Optional player name filter (partial match, case-insensitive).
#' @return A tibble with one row per stroke, including:
#'   \itemize{
#'     \item \code{tournament_id}, \code{tournament_name}: PGA Tour event
#'     \item \code{player_id}, \code{player_name}: PGA Tour player
#'     \item \code{round}, \code{hole}, \code{par}, \code{hole_score}
#'     \item \code{stroke}: Stroke number on the hole
#'     \item \code{stroke_type}: \code{"STROKE"}, \code{"PENALTY"}, \code{"DROP"} and similar
#'     \item \code{distance}: Shot distance in yards
#'     \item \code{distance_remaining}: Distance to the hole after the shot, in yards
#'     \item \code{from_location}, \code{to_location}: Lie before and after
#'       (e.g., \code{"Tee Box"}, \code{"Fairway"}, \code{"Green"})
#'     \item \code{from_code}, \code{to_code}: ShotLink lie codes
#'     \item \code{play_by_play}: Text description of the shot
#'     \item \code{from_x}, \code{from_y}, \code{to_x}, \code{to_y}: Shot
#'       position on the hole diagram, scaled 0 to 1
#'     \item \code{club_speed}, \code{ball_speed} (mph), \code{smash_factor},
#'       \code{spin_rate} (rpm), \code{spin_axis}, \code{launch_angle},
#'       \code{launch_direction} (degrees), \code{apex_height},
#'       \code{apex_range}, \code{apex_side} (feet), \code{flight_time}: radar
#'       readings, \code{NA} when not captured
#'   }
#' @export
#' @examples
#' \donttest{
#' shots <- load_shots(2025, "Players")
#' }
load_shots <- function(year, tournament = NULL, player = NULL) {
  shotlink_load("shots", year, tournament, player)
}

#' Load Tee Times and Pairings
#'
#' Returns the tee time, group and starting hole for every player in every
#' round of a PGA Tour season.
#'
#' @inheritParams load_shots
#' @return A tibble with one row per player per round: \code{tournament_id},
#'   \code{tournament_name}, \code{round}, \code{group}, \code{tee_time}
#'   (UTC), \code{timezone} (local zone of the event), \code{start_tee}
#'   (1 or 10), \code{course_id}, \code{course_name}, \code{player_id},
#'   \code{player_name}.
#' @export
#' @examples
#' \donttest{
#' tee_times <- load_tee_times(2025, "Players")
#' }
load_tee_times <- function(year, tournament = NULL, player = NULL) {
  shotlink_load("tee_times", year, tournament, player)
}

#' Load Course Setup by Round
#'
#' Returns par, yardage and the pin position for every hole in every round of
#' a PGA Tour season.
#'
#' @inheritParams load_shots
#' @return A tibble with one row per hole per round: \code{tournament_id},
#'   \code{tournament_name}, \code{round}, \code{course_id}, \code{hole},
#'   \code{par}, \code{yardage}, and \code{pin_x}, \code{pin_y} (pin position
#'   on the green diagram, scaled 0 to 1; \code{NA} in older seasons).
#' @export
#' @examples
#' \donttest{
#' setup <- load_course_setup(2025, "Players")
#' }
load_course_setup <- function(year, tournament = NULL) {
  shotlink_load("course_setup", year, tournament)
}

#' @noRd
shotlink_load <- function(tag, year, tournament = NULL, player = NULL) {
  data <- release_load(tag, sprintf("%s_%d.rds", tag, as.integer(year)))
  if (is.null(data)) {
    cli::cli_abort(c(
      "No {tag} data available for {.val {year}}.",
      "i" = "The season may not be built yet, or the download failed."
    ))
  }

  if (!is.null(tournament)) {
    keep <- data$tournament_id == tournament |
      grepl(tournament, data$tournament_name, ignore.case = TRUE)
    if (!any(keep)) {
      cli::cli_abort("No tournament matching {.val {tournament}} in {year}.")
    }
    data <- data[keep, ]
  }

  if (!is.null(player)) {
    keep <- grepl(player, data$player_name, ignore.case = TRUE)
    if (!any(keep)) {
      cli::cli_abort("No player matching {.val {player}}.")
    }
    data <- data[keep, ]
  }

  data
}
