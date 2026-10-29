#' Load What's in the Bag
#'
#' Player equipment from the what's in the bag database (WITBDB): one row per
#' club, parsed from GolfWRX and PGA Tour Club Tracker bag posts. Each bag
#' keeps its \code{source} and \code{source_url}.
#'
#' \code{player_id} is the PGA Tour player ID used by [load_shots()]; it is
#' \code{NA} for players without one (for example LPGA players and
#' amateurs). Fields a post does not list are \code{NA}.
#'
#' @param player Optional player name filter (partial match,
#'   case-insensitive).
#' @param year Optional year filter on \code{as_of_date}.
#' @param slot Optional filter: one or more of \code{"driver"},
#'   \code{"fairway"}, \code{"hybrid"}, \code{"utility"}, \code{"iron"},
#'   \code{"wedge"} and \code{"putter"}.
#' @return A tibble with one row per club: bag columns (\code{bag_id},
#'   \code{player_id}, \code{player_name}, \code{as_of_date},
#'   \code{event_name}, \code{tournament_id}, \code{source},
#'   \code{source_url}), the club (\code{slot}, \code{club_label},
#'   \code{club_order}), head, shaft, grip and putter specs, and
#'   \code{qc_flags}. See [pga_field_descriptions()] with \code{"witb"}.
#' @export
#' @examples
#' \donttest{
#' load_witb("Scheffler", year = 2026, slot = "putter")
#' }
load_witb <- function(player = NULL, year = NULL, slot = NULL) {
  data <- witb_load("witb_clubs")
  if (!nrow(data)) {
    return(data)
  }
  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
  }
  if (!is.null(year)) {
    data <- data[as.integer(format(data$as_of_date, "%Y")) %in% year, ]
  }
  if (!is.null(slot)) {
    data <- data[data$slot %in% slot, ]
  }
  data
}

#' Load Equipment Switches
#'
#' Equipment changes found by comparing each player's bags in date order
#' (see [load_witb()]): one row per change of head, shaft or grip in a slot.
#' Only players with a PGA Tour ID are included.
#'
#' @param player Optional player name filter (partial match,
#'   case-insensitive).
#' @param slot Optional slot filter, as in [load_witb()].
#' @return A tibble with \code{player_id}, \code{player_name}, \code{slot},
#'   \code{component}, \code{old_value}, \code{new_value},
#'   \code{last_seen_old}, \code{first_seen_new}, \code{first_seen_event},
#'   \code{tournament_id}, \code{old_bag_id}, \code{new_bag_id},
#'   \code{gap_days}, \code{uncertain}, \code{reason} and
#'   \code{brand_change}. See [pga_field_descriptions()] with
#'   \code{"witb_switches"}.
#' @export
#' @examples
#' \donttest{
#' load_witb_switches("McIlroy", slot = "driver")
#' }
load_witb_switches <- function(player = NULL, slot = NULL) {
  data <- witb_load("witb_switches")
  if (!nrow(data)) {
    return(data)
  }
  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
  }
  if (!is.null(slot)) {
    data <- data[data$slot %in% slot, ]
  }
  data
}

#' @noRd
witb_load <- function(name) {
  data <- release_load("witb", paste0(name, ".rds"))
  if (is.null(data)) {
    cli::cli_inform("What's in the bag data could not be downloaded.")
    return(tibble::tibble())
  }
  tibble::as_tibble(data)
}
