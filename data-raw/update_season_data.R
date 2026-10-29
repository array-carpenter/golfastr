## Update Season Data
## Builds full-field schedule, leaderboard, and hole-by-hole files per season
## and uploads them to GitHub releases. Leaderboards are rebuilt each run
## (cheap, one request per tournament); hole-by-hole data is incremental,
## only fetching tournaments not already in the hosted file.
##
## Run with: Rscript data-raw/update_season_data.R [year ...]
## Defaults to the current year if no years are given.

library(golfastr)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
args <- args[args != ""]
years <- if (length(args) > 0) {
  as.integer(args)
} else {
  as.integer(format(Sys.Date(), "%Y"))
}

repo <- "array-carpenter/golfastr"
release_dir <- file.path(tempdir(), "golfastr_season_data")
dir.create(release_dir, showWarnings = FALSE)

## Every asset is published as rds (fast for R users), parquet, and csv
## (so the releases are usable from Python etc. without R).
save_asset <- function(x, name) {
  saveRDS(x, file.path(release_dir, paste0(name, ".rds")))
  utils::write.csv(x, file.path(release_dir, paste0(name, ".csv")),
                   row.names = FALSE)
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(x, file.path(release_dir, paste0(name, ".parquet")))
  }
}

## Per-hole context computed at build time: score relative to par, the field
## average on that hole in that round, strokes vs the field, and each
## player's running score to par through the tournament. Recomputed from raw
## scores on every run so incremental updates stay consistent.
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

# Same par fix as data-raw/build_hole_history.R: some players carry a wrong
# to-par label from ESPN (a par 6 on a par 4 and the like).
# Each course is a par card (all 18 pars) shared by at least 10% of the
# players in a round; multi-course weeks have two or three. A player takes
# the pars of the closest card, so every par is the most common one for that
# hole on that player's course. Rounds with no common card (or Stableford
# events, where par holds points) are left alone.
fix_par <- function(holes) {
  key <- paste(holes$tournament_id, holes$round)
  pr <- paste(key, holes$player_id)
  for (k in unique(key)) {
    i <- which(key == k)
    cards <- tapply(seq_along(i), pr[i], function(j) {
      card <- rep(NA_integer_, 18)
      card[holes$hole[i[j]]] <- holes$par[i[j]]
      card
    }, simplify = FALSE)
    full <- vapply(cards, function(x) !anyNA(x), TRUE)
    if (!any(full)) next
    ids <- vapply(cards[full], paste, "", collapse = ",")
    n <- table(ids)
    common <- names(n)[n >= max(3, 0.1 * length(cards))]
    common <- common[vapply(strsplit(common, ","), function(x) all(x %in% 3:6), TRUE)]
    if (!length(common)) next
    common <- lapply(strsplit(common[order(-n[common])], ","), as.integer)
    for (p in names(cards)) {
      miss <- vapply(common, function(cc) sum(cc != cards[[p]], na.rm = TRUE), 0)
      if (min(miss) == 0) next
      card <- common[[which.min(miss)]]
      j <- i[pr[i] == p]
      holes$par[j] <- card[holes$hole[j]]
    }
  }
  holes
}

download_existing <- function(tag, filename) {
  asset_url <- sprintf("https://github.com/%s/releases/download/%s/%s",
                       repo, tag, filename)
  tryCatch(
    {
      con <- url(asset_url)
      on.exit(close(con))
      readRDS(con)
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )
}

upload_with_retry <- function(path, tag, attempts = 3L) {
  for (attempt in seq_len(attempts)) {
    error <- tryCatch(
      {
        piggyback::pb_upload(path, repo = repo, tag = tag, overwrite = TRUE)
        NULL
      },
      error = identity
    )
    if (is.null(error)) return(invisible(TRUE))
    if (attempt == attempts) stop(error)

    wait <- 2^attempt
    message(
      "Upload failed (attempt ", attempt, "/", attempts, "): ",
      conditionMessage(error), ". Retrying in ", wait, " seconds..."
    )
    Sys.sleep(wait)
  }
}

for (year in years) {
  message("=== Season ", year, " ===")

  schedule <- load_schedule(year)
  completed <- schedule[!is.na(schedule$end_date) &
                          schedule$end_date < Sys.Date(), ]
  message(nrow(completed), " completed tournaments")

  ## Schedule
  save_asset(schedule, sprintf("schedule_%d", year))

  ## Leaderboards (full rebuild)
  lb_all <- list()
  for (i in seq_len(nrow(completed))) {
    eid <- completed$event_id[i]
    nm <- completed$tournament_name[i]
    lb <- tryCatch(golfastr:::fetch_leaderboard_fast(eid, year, "pga"),
                   error = function(e) NULL)
    if (is.null(lb) || nrow(lb) == 0) {
      message("  no leaderboard for ", nm, ", skipping")
      next
    }
    lb$tournament_id <- eid
    lb$tournament_name <- nm
    lb$year <- year
    lb_all[[length(lb_all) + 1]] <- lb
    Sys.sleep(0.2)
  }
  leaderboards <- dplyr::bind_rows(lb_all)
  if (nrow(leaderboards) == 0) {
    stop("No leaderboard data was fetched. Existing assets were not changed.")
  }
  save_asset(leaderboards, sprintf("leaderboards_%d", year))
  message("Leaderboards: ", nrow(leaderboards), " rows")

  ## Hole-by-hole (incremental)
  holes_file <- sprintf("holes_%d.rds", year)
  existing <- download_existing("holes", holes_file)
  have_ids <- if (!is.null(existing)) unique(existing$tournament_id) else character(0)
  todo <- completed[
    !completed$event_id %in% have_ids &
      completed$event_id %in% unique(leaderboards$tournament_id),
  ]
  message(nrow(todo), " tournaments need hole-by-hole data")

  holes_new <- list()
  for (i in seq_len(nrow(todo))) {
    eid <- todo$event_id[i]
    nm <- todo$tournament_name[i]
    lb <- leaderboards[leaderboards$tournament_id == eid, ]
    if (nrow(lb) == 0) next
    message(sprintf("[%d/%d] %s (%d players)", i, nrow(todo), nm, nrow(lb)))

    for (j in seq_len(nrow(lb))) {
      sc <- tryCatch(golfastr:::fetch_player_holes(eid, lb$player_id[j], "pga"),
                     error = function(e) {
                       message("    failed for ", lb$player_name[j], ": ",
                               conditionMessage(e))
                       NULL
                     })
      if (is.null(sc) || nrow(sc) == 0) next
      sc$player_id <- lb$player_id[j]
      sc$player_name <- lb$player_name[j]
      sc$position <- lb$position[j]
      sc$tournament_id <- eid
      sc$tournament_name <- nm
      sc$year <- year
      holes_new[[length(holes_new) + 1]] <- sc
      Sys.sleep(0.1)
    }
  }

  holes <- dplyr::bind_rows(c(list(existing), holes_new))
  if (nrow(holes) > 0) {
    # win probability columns come from update_win_probability.R; keep them
    # for events that already have them
    wp_cols <- intersect(c("win_prob", "win_prob_before", "wpa", "skill_rating",
                           "hole_order_source"), names(holes))
    holes <- holes[, c("player_id", "player_name", "position",
                       "tournament_id", "tournament_name",
                       "round", "hole", "par", "score", "score_type", "year",
                       wp_cols)]
    holes <- add_hole_stats(fix_par(holes))
  }
  save_asset(holes, sprintf("holes_%d", year))
  message("Holes: ", nrow(holes), " rows")
}

## Upload to GitHub releases
uploads <- list(
  schedules    = sprintf("schedule_%d", years),
  leaderboards = sprintf("leaderboards_%d", years),
  holes        = sprintf("holes_%d", years)
)

if (requireNamespace("piggyback", quietly = TRUE)) {
  for (tag in names(uploads)) {
    for (f in uploads[[tag]]) {
      for (ext in c(".rds", ".csv", ".parquet")) {
        path <- file.path(release_dir, paste0(f, ext))
        if (!file.exists(path)) next
        message("Uploading ", basename(path), " to release '", tag, "'...")
        upload_with_retry(path, tag)
      }
    }
  }
  message("Done! Release assets updated.")
} else {
  message("Install piggyback to auto-upload, or run for each file:")
  message("  gh release upload <tag> ", release_dir, "/<file> --clobber")
}
