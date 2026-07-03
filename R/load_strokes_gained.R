#' Load Strokes Gained Statistics
#'
#' Returns strokes gained data from the PGA Tour, with all six SG categories
#' per player: putting, around the green, approach, off the tee, tee to green,
#' and total.
#'
#' By default, downloads the latest data from GitHub releases (updated weekly).
#' Falls back to the bundled dataset shipped with the package if the download
#' fails (e.g., no internet connection).
#'
#' @param player Optional player name filter (partial match, case-insensitive).
#' @param use_bundled If \code{TRUE}, skip the download and use the dataset
#'   bundled with the package. Defaults to \code{FALSE}.
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
#' # Get all strokes gained data (downloads latest)
#' sg <- load_strokes_gained()
#'
#' # Use bundled data (no internet needed)
#' sg <- load_strokes_gained(use_bundled = TRUE)
#'
#' # Look up a specific player
#' load_strokes_gained("Scheffler")
load_strokes_gained <- function(player = NULL, use_bundled = FALSE) {
  if (use_bundled) {
    data <- strokes_gained
  } else {
    data <- sg_from_release()
    if (is.null(data)) {
      cli::cli_inform("Using bundled strokes gained data (may not be latest).")
      data <- strokes_gained
    }
  }

  if (!is.null(player)) {
    matches <- grepl(player, data$player_name, ignore.case = TRUE)
    if (!any(matches)) {
      cli::cli_abort("No player found matching {.val {player}}.")
    }
    data <- data[matches, ]
  }

  data
}

#' Download strokes gained from GitHub release
#' @noRd
sg_from_release <- function() {
  url <- paste0(
    "https://github.com/array-carpenter/golfastr/",
    "releases/download/strokes_gained/strokes_gained.rds"
  )
  tryCatch(
    {
      con <- url(url)
      on.exit(close(con))
      tibble::as_tibble(readRDS(con))
    },
    error = function(e) NULL
  )
}
