#' ESPN API Request Helpers
#'
#' Internal functions for making requests to ESPN's Golf APIs.
#' Supports multiple tours via the tour parameter.
#'
#' @name api_espn
#' @keywords internal
NULL

#' Get ESPN tour code
#'
#' @param tour Tour identifier ("pga", "lpga", "euro", etc.)
#' @return ESPN tour code string
#' @keywords internal
get_espn_tour_code <- function(tour = "pga") {
 tour <- tolower(tour)
 codes <- list(
   pga = "pga",
   lpga = "lpga",
   euro = "euro",
   dp = "euro",
   champions = "champions"
 )
 if (!tour %in% names(codes)) {
   stop("Unknown tour: ", tour, ". Supported: ", paste(names(codes), collapse = ", "))
 }
 codes[[tour]]
}

#' ESPN Core API Request
#'
#' Makes requests to ESPN's core sports API.
#'
#' @param endpoint API endpoint path
#' @param tour Tour code (default "pga")
#' @param query_params Optional query parameters
#' @return Parsed JSON response
#' @keywords internal
#' @importFrom httr2 request req_url_query req_perform resp_body_json
espn_core_request <- function(endpoint, tour = "pga", query_params = NULL) {
 tour_code <- get_espn_tour_code(tour)
 base_url <- sprintf("http://sports.core.api.espn.com/v2/sports/golf/leagues/%s/", tour_code)
 full_url <- paste0(base_url, endpoint)

 req <- httr2::request(full_url)

 default_params <- list(lang = "en", region = "us")
 all_params <- c(default_params, query_params)

 if (length(all_params) > 0) {
   req <- httr2::req_url_query(req, !!!all_params)
 }

 resp <- httr2::req_perform(req)
 httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' ESPN Site API Request
#'
#' Makes requests to ESPN's site API (for scoreboard, schedule, etc.).
#'
#' @param endpoint API endpoint path
#' @param tour Tour code (default "pga")
#' @param query_params Optional query parameters
#' @return Parsed JSON response
#' @keywords internal
#' @importFrom httr2 request req_url_query req_perform resp_body_json
espn_site_request <- function(endpoint, tour = "pga", query_params = NULL) {
 tour_code <- get_espn_tour_code(tour)
 base_url <- sprintf("http://site.api.espn.com/apis/site/v2/sports/golf/%s/", tour_code)
 full_url <- paste0(base_url, endpoint)

 req <- httr2::request(full_url)

 if (!is.null(query_params) && length(query_params) > 0) {
   req <- httr2::req_url_query(req, !!!query_params)
 }

 resp <- httr2::req_perform(req)
 httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# Backwards compatibility alias
espn_golf_request <- function(endpoint, query_params = NULL) {
 espn_core_request(endpoint, tour = "pga", query_params = query_params)
}
