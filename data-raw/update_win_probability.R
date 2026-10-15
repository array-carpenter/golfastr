## Daily: add win probability to finished events that don't have it yet and
## refresh the published skill ratings. Runs after update_season_data.R.
##
##   Rscript data-raw/update_win_probability.R [year ...] [--upload]

if (requireNamespace("pkgload", quietly = TRUE)) pkgload::load_all(quiet = TRUE) else library(golfastr)
suppressMessages(library(dplyr))
source("data-raw/wp/helpers.R")

args <- commandArgs(trailingOnly = TRUE)
upload <- "--upload" %in% args
targets <- as.integer(args[args != "--upload"])
this_year <- as.integer(format(Sys.Date(), "%Y"))
if (!length(targets)) targets <- this_year

years <- 2004:max(targets, this_year)
holes <- read_hole_history(years)
events <- event_table(holes, read_schedules(years))
params <- golfastr:::wp_params
sp <- params$skill_params

stroke <- events[!events$stableford, ]
rounds <- golfastr:::wp_round_values(holes[!holes$playoff & holes$tournament_id %in% stroke$tournament_id, ])
rated <- rate_events(rounds, stroke, sp[["half_life"]], sp[["prior_mean"]], sp[["prior_n"]])
ids <- list(events = load_event_ids(), players = load_player_ids())

for (year in targets) {
  h_year <- holes[holes$year == year, ]
  have <- if ("win_prob" %in% names(h_year)) {
    unique(h_year$tournament_id[!is.na(h_year$win_prob)])
  } else character()
  todo <- events$tournament_id[events$year == year & events$wp_eligible & !events$tournament_id %in% have]
  limit <- as.integer(Sys.getenv("WP_LIMIT", "0"))
  if (limit > 0) todo <- utils::head(todo, limit)
  message(year, ": ", length(todo), " events need win probability")
  if (!length(todo)) next

  tee <- suppressMessages(load_tee_times(year))
  have_ids <- nrow(tee) && nrow(ids$events) && nrow(ids$players)
  order <- if (have_ids) hole_order_table(tee, ids$events, ids$players)
  waves <- if (have_ids) wave_table(tee, ids$events, ids$players)
  results <- if ("win_prob" %in% names(h_year)) {
    h_year[!is.na(h_year$win_prob) & h_year$tournament_id %in% have,
           c("tournament_id", "player_id", "round", "hole", wp_columns)]
  }
  for (id in todo) {
    r <- rated$ratings[rated$ratings$tournament_id == id, ]
    ord <- if (!is.null(order)) order[order$tournament_id == id, ]
    if (!is.null(ord) && !nrow(ord)) ord <- NULL
    wv <- if (!is.null(waves)) waves[waves$tournament_id == id, ]
    if (!is.null(wv) && !nrow(wv)) wv <- NULL
    message("  ", events$tournament_name[events$tournament_id == id])
    results <- bind_rows(results, event_win_probability(
      h_year[h_year$tournament_id == id, ], events[events$tournament_id == id, ],
      setNames(r$rating, r$player_id), params, order = ord, wave = wv))
  }
  out <- attach_win_probability(h_year, results)
  save_and_upload(out, sprintf("holes_%d", year), "holes", upload)
}

files <- ratings_files(holes[!holes$playoff, ], stroke, rated, params)
for (name in names(files)) save_and_upload(files[[name]], name, "skill_ratings", upload)
message("done")
