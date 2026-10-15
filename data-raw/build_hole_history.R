## Build hole-by-hole files for past seasons from ESPN's annual scoreboard
##
## ESPN's season scoreboard (site.api.espn.com/.../golf/pga/scoreboard?dates=YYYY)
## carries every competitor's hole scores, so a whole season is one request.
## The raw responses were parsed to one parquet file per season; this script
## reshapes them to the hosted `holes` schema and uploads them.
##
##   Rscript --vanilla data-raw/build_hole_history.R 2004 2005 ... [--upload]

library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
upload <- "--upload" %in% args
years <- as.integer(args[args != "--upload"])

src_dir <- Sys.getenv("HOLE_HISTORY_DIR", "~/Code/golf-wp-planning/data/holes_clean")
out_dir <- Sys.getenv("HOLE_HISTORY_OUT", file.path(tempdir(), "hole_history"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# same per-hole context as data-raw/update_season_data.R
add_hole_stats <- function(holes) {
  holes |>
    arrange(tournament_id, player_id, round, hole) |>
    group_by(tournament_id, round, hole) |>
    mutate(field_avg = round(mean(score, na.rm = TRUE), 3)) |>
    ungroup() |>
    mutate(
      to_par = score - par,
      vs_field = round(score - field_avg, 3)
    ) |>
    group_by(tournament_id, player_id) |>
    mutate(cume_to_par = cumsum(coalesce(to_par, 0L))) |>
    ungroup()
}

score_type <- function(to_par) {
  out <- rep("OTHER", length(to_par))
  out[to_par <= -3] <- "DOUBLE_EAGLE"
  out[to_par == -2] <- "EAGLE"
  out[to_par == -1] <- "BIRDIE"
  out[to_par == 0] <- "PAR"
  out[to_par == 1] <- "BOGEY"
  out[to_par == 2] <- "DOUBLE_BOGEY"
  out[to_par == 3] <- "TRIPLE_BOGEY"
  out[is.na(to_par)] <- NA_character_
  out
}

# Par in the source is score minus ESPN's to-par label, and a few players
# carry a wrong label (par 6 holes and the like). Replace a par that under 5%
# of the field had on that hole and round with the most common one; real
# differences on multi-course weeks are far more common than that.
fix_par <- function(holes) {
  key <- paste(holes$tournament_id, holes$round, holes$hole)
  share <- stats::ave(seq_along(key), key, holes$par, FUN = length) /
    stats::ave(seq_along(key), key, FUN = length)
  mode_par <- tapply(holes$par, key, function(p) as.integer(names(which.max(table(p)))))
  bad <- share < 0.05
  holes$par[bad] <- mode_par[key[bad]]
  holes
}

build_season <- function(year) {
  raw <- arrow::read_parquet(file.path(path.expand(src_dir), sprintf("holes_%d.parquet", year)))
  raw <- raw[raw$round <= 4 & raw$hole <= 18 & !is.na(raw$score) & !is.na(raw$par), ]
  holes <- tibble::tibble(
    player_id = as.character(raw$player_id),
    player_name = raw$player_name,
    tournament_id = as.character(raw$event_id),
    tournament_name = raw$event_name,
    round = as.integer(raw$round),
    hole = as.integer(raw$hole),
    par = as.integer(raw$par),
    score = as.integer(raw$score),
    year = as.integer(year),
    finish_order = as.integer(raw$finish_order)
  )
  holes <- holes[!duplicated(holes[c("tournament_id", "player_id", "round", "hole")]), ]
  holes <- fix_par(holes)
  # a few early events have hole scores for only one or two players
  field <- tapply(holes$player_id, holes$tournament_id, function(x) length(unique(x)))
  holes <- holes[holes$tournament_id %in% names(field)[field >= 10], ]
  holes$position <- holes$finish_order
  holes <- add_hole_stats(holes)
  holes$score_type <- score_type(holes$to_par)
  holes[c("player_id", "player_name", "position", "tournament_id", "tournament_name",
          "round", "hole", "par", "score", "score_type", "year", "field_avg",
          "to_par", "vs_field", "cume_to_par")]
}

upload_with_retry <- function(path, tag, attempts = 5L) {
  for (i in seq_len(attempts)) {
    ok <- tryCatch({
      piggyback::pb_upload(path, repo = "array-carpenter/golfastr", tag = tag, overwrite = TRUE)
      TRUE
    }, error = function(e) {
      message("  upload failed (", conditionMessage(e), "), retrying")
      FALSE
    })
    if (ok) return(invisible(TRUE))
    Sys.sleep(10 * i)
  }
  stop("could not upload ", basename(path))
}

for (year in years) {
  holes <- build_season(year)
  message(year, ": ", nrow(holes), " rows, ", length(unique(holes$tournament_id)), " events")
  base <- file.path(out_dir, sprintf("holes_%d", year))
  saveRDS(holes, paste0(base, ".rds"))
  utils::write.csv(holes, paste0(base, ".csv"), row.names = FALSE)
  arrow::write_parquet(holes, paste0(base, ".parquet"))
  if (upload) {
    for (ext in c(".rds", ".csv", ".parquet")) {
      message("Uploading ", basename(base), ext)
      upload_with_retry(paste0(base, ext), "holes")
    }
  }
}
