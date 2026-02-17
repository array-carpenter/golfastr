#' PGA Tour Strokes Gained Statistics
#'
#' Pre-built dataset of strokes gained statistics from the PGA Tour.
#' Contains per-round averages for all six strokes gained categories
#' for players in the current season. Updated with each package release.
#'
#' @format A tibble with one row per player and 11 columns:
#' \describe{
#'   \item{player_id}{PGA Tour player ID}
#'   \item{player_name}{Player display name}
#'   \item{country}{Three-letter country code (e.g., "USA", "ENG")}
#'   \item{sg_putt}{Strokes Gained: Putting (per round average)}
#'   \item{sg_arg}{Strokes Gained: Around the Green (per round average)}
#'   \item{sg_app}{Strokes Gained: Approach the Green (per round average)}
#'   \item{sg_ott}{Strokes Gained: Off the Tee (per round average)}
#'   \item{sg_t2g}{Strokes Gained: Tee to Green (per round average)}
#'   \item{sg_total}{Strokes Gained: Total (per round average)}
#'   \item{rounds}{Number of ShotLink-measured rounds}
#'   \item{season}{PGA Tour season year}
#' }
#' @source PGA Tour (\url{https://www.pgatour.com/stats})
"strokes_gained"
