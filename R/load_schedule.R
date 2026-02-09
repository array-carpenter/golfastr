#' Load Golf Schedule
#'
#' Retrieves the tournament schedule for a given year and tour.
#'
#' @param year Season year (e.g., 2026). Defaults to current year.
#' @param tour Tour identifier. Currently supports "pga" (default).
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{event_id}: ESPN event identifier
#'     \item \code{tournament_name}: Tournament name
#'     \item \code{start_date}: Tournament start date
#'     \item \code{end_date}: Tournament end date
#'   }
#' @export
#' @examples
#' \donttest{
#' # Get current year schedule
#' schedule <- load_schedule()
#'
#' # Get specific year
#' schedule_2025 <- load_schedule(2025)
#' }
load_schedule <- function(year = as.integer(format(Sys.Date(), "%Y")),
                          tour = "pga") {
  response <- espn_site_request("scoreboard", tour = tour,
                                query_params = list(dates = year))

  if (is.null(response$leagues) || length(response$leagues) == 0) {
    stop("No schedule data available for year: ", year, ", tour: ", tour)
  }

  calendar <- response$leagues$calendar[[1]]

  tibble::tibble(
    event_id = as.character(calendar$id),
    tournament_name = calendar$label,
    start_date = as.Date(calendar$startDate),
    end_date = as.Date(calendar$endDate)
  )
}

#' @describeIn load_schedule Legacy function for backwards compatibility
#' @export
get_pga_schedule <- function(year = format(Sys.Date(), "%Y")) {
  load_schedule(year = as.integer(year), tour = "pga")
}
