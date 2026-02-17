## Update Strokes Gained Data
## Run this script to refresh the pre-built strokes gained dataset.
## The data is shipped with the package so users don't need to hit the API.

# This script uses the internal PGA Tour GraphQL API functions from api_pga.R.
# Run with: source("data-raw/update_strokes_gained.R")

library(httr2)
library(tibble)

# --- PGA Tour GraphQL helpers (standalone, no package dependency) ---

pga_graphql_request <- function(query) {
  api_key <- Sys.getenv("PGA_TOUR_API_KEY")
  if (api_key == "") {
    stop("PGA_TOUR_API_KEY environment variable not set.\n",
         "Set it in your .Renviron: usethis::edit_r_environ()")
  }
  url <- "https://orchestrator.pgatour.com/graphql"
  req <- httr2::request(url)
  req <- httr2::req_headers(req,
    "Content-Type" = "application/json",
    "x-api-key" = api_key
  )
  req <- httr2::req_body_json(req, list(query = query))
  resp <- httr2::req_perform(req)
  result <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (!is.null(result$errors)) {
    stop("PGA Tour API error: ", result$errors$message[1])
  }
  result$data
}

fetch_sg_stat <- function(stat_id, year, tour_code = "R") {
  query <- sprintf(
    '{ statDetails(tourCode: %s, statId: "%s", year: %d) {
      statTitle statHeaders
      rows {
        ... on StatDetailsPlayer {
          playerId playerName country countryFlag rank
          stats { statName statValue }
        }
      }
    } }',
    tour_code, stat_id, year
  )
  data <- pga_graphql_request(query)
  details <- data$statDetails
  if (is.null(details) || is.null(details$rows) || length(details$rows) == 0) {
    return(tibble::tibble())
  }
  rows <- details$rows
  if (is.null(rows$playerId)) return(tibble::tibble())
  rows <- rows[!is.na(rows$playerId), ]
  if (nrow(rows) == 0) return(tibble::tibble())

  avg_values <- vapply(seq_len(nrow(rows)), function(i) {
    stats <- rows$stats[[i]]
    if (is.null(stats) || nrow(stats) == 0) return(NA_real_)
    avg_row <- stats[stats$statName == "Avg", ]
    if (nrow(avg_row) == 0) return(NA_real_)
    as.numeric(avg_row$statValue[1])
  }, numeric(1))

  rounds_values <- vapply(seq_len(nrow(rows)), function(i) {
    stats <- rows$stats[[i]]
    if (is.null(stats) || nrow(stats) == 0) return(NA_integer_)
    rounds_row <- stats[stats$statName == "Measured Rounds", ]
    if (nrow(rounds_row) == 0) return(NA_integer_)
    as.integer(rounds_row$statValue[1])
  }, integer(1))

  tibble::tibble(
    player_id   = as.character(rows$playerId),
    player_name = rows$playerName,
    country     = rows$countryFlag,
    avg         = avg_values,
    rounds      = rounds_values
  )
}

# --- Fetch and build the dataset ---

sg_stat_ids <- list(
  putting      = "02564",
  around_green = "02569",
  approach     = "02568",
  off_tee      = "02567",
  tee_to_green = "02674",
  total        = "02675"
)

col_names <- c("sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g", "sg_total")

# Fetch current season
current_year <- as.integer(format(Sys.Date(), "%Y"))
message("Fetching strokes gained data for ", current_year, " season...")

results <- list()
for (i in seq_along(sg_stat_ids)) {
  stat_name <- names(sg_stat_ids)[i]
  col_name <- col_names[i]
  message("  Fetching SG: ", stat_name, " (", sg_stat_ids[[i]], ")...")
  data <- fetch_sg_stat(sg_stat_ids[[i]], current_year)
  if (nrow(data) > 0) {
    results[[col_name]] <- data
    names(results[[col_name]])[names(results[[col_name]]) == "avg"] <- col_name
    names(results[[col_name]])[names(results[[col_name]]) == "rounds"] <-
      paste0("rounds_", col_name)
  }
}

# Merge all stats by player_id
merged <- results[[1]]
for (i in 2:length(results)) {
  merged <- merge(
    merged,
    results[[i]][, c("player_id", names(results[[i]])[4:5])],
    by = "player_id",
    all = TRUE
  )
}

rounds_col <- if ("rounds_sg_total" %in% names(merged)) {
  merged$rounds_sg_total
} else {
  rounds_cols <- grep("^rounds_", names(merged), value = TRUE)
  if (length(rounds_cols) > 0) merged[[rounds_cols[1]]] else NA_integer_
}

strokes_gained <- tibble::tibble(
  player_id   = merged$player_id,
  player_name = merged$player_name,
  country     = merged$country,
  sg_putt     = if ("sg_putt" %in% names(merged)) merged$sg_putt else NA_real_,
  sg_arg      = if ("sg_arg" %in% names(merged)) merged$sg_arg else NA_real_,
  sg_app      = if ("sg_app" %in% names(merged)) merged$sg_app else NA_real_,
  sg_ott      = if ("sg_ott" %in% names(merged)) merged$sg_ott else NA_real_,
  sg_t2g      = if ("sg_t2g" %in% names(merged)) merged$sg_t2g else NA_real_,
  sg_total    = if ("sg_total" %in% names(merged)) merged$sg_total else NA_real_,
  rounds      = as.integer(rounds_col),
  season      = current_year
)

strokes_gained <- strokes_gained[order(strokes_gained$sg_total,
                                       decreasing = TRUE, na.last = TRUE), ]

message("Built strokes_gained: ", nrow(strokes_gained), " players")
print(head(strokes_gained, 10))

# Save as package data
usethis::use_data(strokes_gained, overwrite = TRUE)
message("Done! strokes_gained dataset saved to data/strokes_gained.rda")
