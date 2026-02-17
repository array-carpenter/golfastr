#' Load Strokes Gained Statistics
#'
#' Returns pre-built strokes gained data from the PGA Tour, with all six
#' SG categories per player: putting, around the green, approach, off the tee,
#' tee to green, and total.
#'
#' Data is sourced from the PGA Tour and updated with each package release.
#' To filter by player, use standard dplyr operations on the returned tibble.
#'
#' @param player Optional player name filter (partial match, case-insensitive).
#' @return A tibble with one row per player and columns:
#'   \itemize{
#'     \item \code{player_id}: PGA Tour player ID
#'     \item \code{player_name}: Player display name
#'     \item \code{country}: Three-letter country code
#'     \item \code{sg_putt}: Strokes Gained: Putting (per round avg)
#'     \item \code{sg_arg}: Strokes Gained: Around the Green (per round avg)
#'     \item \code{sg_app}: Strokes Gained: Approach the Green (per round avg)
#'     \item \code{sg_ott}: Strokes Gained: Off the Tee (per round avg)
#'     \item \code{sg_t2g}: Strokes Gained: Tee to Green (per round avg)
#'     \item \code{sg_total}: Strokes Gained: Total (per round avg)
#'     \item \code{rounds}: Number of measured (ShotLink) rounds
#'     \item \code{season}: PGA Tour season year
#'   }
#' @export
#' @examples
#' # Get all strokes gained data
#' sg <- load_strokes_gained()
#'
#' # Look up a specific player
#' load_strokes_gained("Scheffler")
load_strokes_gained <- function(player = NULL) {
  data <- strokes_gained

  if (!is.null(player)) {
    matches <- grepl(player, data$player_name, ignore.case = TRUE)
    if (!any(matches)) {
      stop("No player found matching '", player, "'")
    }
    data <- data[matches, ]
  }

  data
}
