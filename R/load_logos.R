#' Load Logos
#'
#' Logo image URLs for tours, tournaments, ShotLink courses and famous clubs
#' (every host of a men's or women's major, the Ryder Cup, Presidents Cup or
#' Solheim Cup since 1960). Only URLs are provided, not images; logos remain
#' the property of their owners.
#'
#' Tournament logos come from the PGA Tour schedule (2012 on, joined to
#' [load_event_ids()]) and from Wikimedia. A course takes its club's logo,
#' from Wikimedia or else the club's own site. Every URL returned an image
#' on \code{checked_at}.
#'
#' @param type Optional filter: one or more of \code{"tour"},
#'   \code{"tournament"}, \code{"course"} and \code{"club"}.
#' @param name Optional name filter (partial match, case-insensitive).
#' @return A tibble with \code{logo_type}, \code{name}, \code{logo_url},
#'   \code{logo_source}, \code{source_page}, \code{tour},
#'   \code{pga_tournament_id}, \code{espn_tournament_id}, \code{course_id},
#'   \code{club_id}, \code{primary_color}, \code{secondary_color} and
#'   \code{checked_at}.
#' @export
#' @examples
#' \donttest{
#' load_logos("club", "Augusta")
#' }
load_logos <- function(type = NULL, name = NULL) {
  data <- release_load("logos", "logos.rds")
  if (is.null(data)) {
    cli::cli_inform("Logos could not be downloaded.")
    return(tibble::tibble())
  }
  data <- tibble::as_tibble(data)
  if (!is.null(type)) {
    data <- data[data$logo_type %in% type, ]
  }
  if (!is.null(name)) {
    data <- data[grepl(name, data$name, ignore.case = TRUE), ]
  }
  data
}
