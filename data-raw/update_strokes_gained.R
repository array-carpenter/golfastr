## Update Strokes Gained Data
## Uses PGA Tour's public GraphQL API (no private key needed).
## Run with: source("data-raw/update_strokes_gained.R")

library(httr2)
library(tibble)

# --- PGA Tour GraphQL API (public key from pgatour.com JS bundle) ---

pga_graphql_request <- function(query) {
  api_key <- "da2-gsrx5bibzbb4njvhl7t37wqyl4"
  resp <- httr2::request("https://orchestrator.pgatour.com/graphql") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-api-key" = api_key
    ) |>
    httr2::req_body_json(list(query = query)) |>
    httr2::req_perform()
  result <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (!is.null(result$errors)) {
    stop("PGA Tour API error: ", result$errors$message[1])
  }
  result$data
}

fetch_sg_stat <- function(stat_id, year) {
  query <- sprintf(
    '{ statDetails(tourCode: R, statId: "%s", year: %d) {
      statTitle year
      rows {
        ... on StatDetailsPlayer {
          playerId playerName countryFlag
          stats { statName statValue }
        }
      }
    } }',
    stat_id, year
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

# --- Config ---

sg_stat_ids <- list(
  putting      = "02564",
  around_green = "02569",
  approach     = "02568",
  off_tee      = "02567",
  tee_to_green = "02674",
  total        = "02675"
)

col_names <- c("sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g", "sg_total")
years <- 2019:as.integer(format(Sys.Date(), "%Y"))

# --- Fetch all years ---

all_seasons <- list()

for (year in years) {
  message("Fetching ", year, " season...")

  results <- list()
  for (i in seq_along(sg_stat_ids)) {
    stat_name <- names(sg_stat_ids)[i]
    col_name <- col_names[i]
    message("  SG: ", stat_name)
    data <- tryCatch(
      fetch_sg_stat(sg_stat_ids[[i]], year),
      error = function(e) {
        message("    Error: ", e$message)
        tibble::tibble()
      }
    )
    if (nrow(data) > 0) {
      results[[col_name]] <- data
      names(results[[col_name]])[names(results[[col_name]]) == "avg"] <- col_name
      names(results[[col_name]])[names(results[[col_name]]) == "rounds"] <-
        paste0("rounds_", col_name)
    }
    Sys.sleep(0.5)
  }

  if (length(results) == 0) {
    message("  No data for ", year, ", skipping.")
    next
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

  season_df <- tibble::tibble(
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
    season      = as.integer(year)
  )

  all_seasons[[as.character(year)]] <- season_df
  message("  ", nrow(season_df), " players")
}

strokes_gained <- do.call(rbind, all_seasons)
strokes_gained <- strokes_gained[order(strokes_gained$season,
                                       strokes_gained$sg_total,
                                       decreasing = c(FALSE, TRUE),
                                       method = "radix"), ]
rownames(strokes_gained) <- NULL

message("\nBuilt strokes_gained: ", nrow(strokes_gained), " rows across ",
        length(unique(strokes_gained$season)), " seasons")
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
