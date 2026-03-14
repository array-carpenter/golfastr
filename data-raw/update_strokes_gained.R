## Update Strokes Gained Data
## Scrapes pgatour.com stats pages — no API key needed.
## Run with: source("data-raw/update_strokes_gained.R")

library(httr2)
library(tibble)

# --- Scrape __NEXT_DATA__ from pgatour.com/stats/detail/{stat_id} ---

fetch_sg_stat <- function(stat_id) {
  url <- paste0("https://www.pgatour.com/stats/detail/", stat_id)
  message("  Fetching ", url)

  resp <- httr2::request(url) |>
    httr2::req_headers(
      "User-Agent" = "golfastr R package (https://github.com/array-carpenter/golfastr)"
    ) |>
    httr2::req_perform()

  html <- httr2::resp_body_string(resp)

  # Extract __NEXT_DATA__ JSON from between script tags
  start <- regexpr("__NEXT_DATA__.*?type=.application/json.>", html)
  if (start == -1) stop("Could not find __NEXT_DATA__ in page")
  json_start <- start + attr(start, "match.length")
  rest <- substring(html, json_start)
  json_end <- regexpr("</script>", rest) - 1
  json_str <- substring(rest, 1, json_end)

  page_data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  # Find the statDetails query with year set
  queries <- page_data$props$pageProps$dehydratedState$queries
  stat_data <- NULL
  for (i in seq_along(queries)) {
    key <- queries[[i]]$queryKey
    if (is.list(key) && length(key) >= 2 &&
        identical(key[[1]], "statDetails") &&
        !is.null(key[[2]]$year)) {
      stat_data <- queries[[i]]$state$data
      break
    }
  }

  if (is.null(stat_data)) stop("No statDetails data found for stat ", stat_id)

  rows <- stat_data$rows
  if (is.null(rows) || length(rows) == 0) return(tibble::tibble())

  year <- stat_data$year

  # Extract player data from nested list
  player_ids   <- vapply(rows, function(r) as.character(r$playerId %||% NA), character(1))
  player_names <- vapply(rows, function(r) r$playerName %||% NA_character_, character(1))
  countries    <- vapply(rows, function(r) r$countryFlag %||% NA_character_, character(1))

  avg_values <- vapply(rows, function(r) {
    for (s in r$stats) {
      if (identical(s$statName, "Avg")) return(as.numeric(s$statValue))
    }
    NA_real_
  }, numeric(1))

  rounds_values <- vapply(rows, function(r) {
    for (s in r$stats) {
      if (identical(s$statName, "Measured Rounds")) return(as.integer(s$statValue))
    }
    NA_integer_
  }, integer(1))

  out <- tibble::tibble(
    player_id   = player_ids,
    player_name = player_names,
    country     = countries,
    avg         = avg_values,
    rounds      = rounds_values,
    year        = as.integer(year)
  )
  out[!is.na(out$player_id), ]
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

message("Fetching strokes gained data from pgatour.com...")

results <- list()
for (i in seq_along(sg_stat_ids)) {
  stat_name <- names(sg_stat_ids)[i]
  col_name <- col_names[i]
  message("  SG: ", stat_name, " (", sg_stat_ids[[i]], ")...")
  data <- fetch_sg_stat(sg_stat_ids[[i]])
  if (nrow(data) > 0) {
    results[[col_name]] <- data
    names(results[[col_name]])[names(results[[col_name]]) == "avg"] <- col_name
    names(results[[col_name]])[names(results[[col_name]]) == "rounds"] <-
      paste0("rounds_", col_name)
  }
  Sys.sleep(1)  # be polite
}

# Merge all stats by player_id
merged <- results[[1]]
season_year <- merged$year[1]
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
  season      = as.integer(season_year)
)

strokes_gained <- strokes_gained[order(strokes_gained$sg_total,
                                       decreasing = TRUE, na.last = TRUE), ]

message("Built strokes_gained: ", nrow(strokes_gained), " players")
print(head(strokes_gained, 10))

# Save as package data (bundled fallback)
usethis::use_data(strokes_gained, overwrite = TRUE)
message("Saved bundled data to data/strokes_gained.rda")

# Also save as .rds for GitHub release upload
release_dir <- file.path(tempdir(), "golfastr_release")
dir.create(release_dir, showWarnings = FALSE)
release_path <- file.path(release_dir, "strokes_gained.rds")
saveRDS(strokes_gained, release_path)
message("Saved release .rds to ", release_path)

# Upload to GitHub release via piggyback
if (requireNamespace("piggyback", quietly = TRUE)) {
  message("Uploading to GitHub release 'strokes_gained'...")
  piggyback::pb_upload(
    release_path,
    repo = "array-carpenter/golfastr",
    tag = "strokes_gained",
    overwrite = TRUE
  )
  message("Done! Release asset updated.")
} else {
  message("Install piggyback to auto-upload: install.packages('piggyback')")
  message("Then run: piggyback::pb_upload('", release_path, "', ",
          "repo = 'array-carpenter/golfastr', tag = 'strokes_gained', ",
          "overwrite = TRUE)")
}
