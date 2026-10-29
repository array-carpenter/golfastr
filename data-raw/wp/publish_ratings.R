## Publish skill ratings: each player's rating before every event, and each
## player's latest rating (used for events not yet in the first file).
##
##   Rscript --vanilla data-raw/wp/publish_ratings.R [--upload]

devtools::load_all(quiet = TRUE)
suppressMessages(library(dplyr))

cache <- "data-raw/wp/cache"
holes <- readRDS(file.path(cache, "holes_all.rds"))
events <- readRDS(file.path(cache, "events.rds"))
events <- events[!events$stableford, ]
ratings <- readRDS(file.path(cache, "skill_ratings.rds"))
strength <- readRDS(file.path(cache, "field_strength.rds"))
p <- attr(ratings, "params")

rounds <- wp_round_values(holes[holes$tournament_id %in% events$tournament_id, ])
rounds$sg <- rounds$sg + strength[rounds$tournament_id]
latest_event <- tibble::tibble(tournament_id = "latest", start_date = Sys.Date() + 1)
ev <- rbind(events[c("tournament_id", "start_date")], latest_event)
stub <- tibble::tibble(tournament_id = "latest", player_id = unique(rounds$player_id), round = 1L,
                       strokes = NA_real_, sg = 0)
latest <- wp_skill_ratings(rbind(rounds, stub), ev, p[["half_life"]], p[["prior_mean"]], p[["prior_n"]])
latest <- latest[latest$tournament_id == "latest", c("player_id", "rating", "n_rounds")]

names_tbl <- holes[!duplicated(holes$player_id, fromLast = TRUE), c("player_id", "player_name")]
latest <- left_join(latest, names_tbl, by = "player_id")
latest$as_of <- max(events$start_date, na.rm = TRUE)
latest <- latest[order(-latest$rating), c("player_id", "player_name", "rating", "n_rounds", "as_of")]
print(head(latest[latest$n_rounds >= 50, ], 10))

skill_ratings <- left_join(ratings, names_tbl, by = "player_id")
skill_ratings <- skill_ratings[c("tournament_id", "player_id", "player_name", "rating", "n_rounds")]
attr(skill_ratings, "params") <- NULL

out <- file.path(tempdir(), "skill_ratings")
dir.create(out, showWarnings = FALSE)
for (x in list(list(skill_ratings, "skill_ratings"), list(latest, "skill_ratings_latest"))) {
  base <- file.path(out, x[[2]])
  saveRDS(x[[1]], paste0(base, ".rds"))
  utils::write.csv(x[[1]], paste0(base, ".csv"), row.names = FALSE)
  arrow::write_parquet(x[[1]], paste0(base, ".parquet"))
}
if ("--upload" %in% commandArgs(trailingOnly = TRUE)) {
  for (f in list.files(out, full.names = TRUE)) {
    piggyback::pb_upload(f, repo = "array-carpenter/golfastr", tag = "skill_ratings", overwrite = TRUE)
  }
}
