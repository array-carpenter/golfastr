## Daily: add win probability to finished events that don't have it yet and
## refresh the published skill ratings. Runs after update_season_data.R.
##
##   Rscript data-raw/update_win_probability.R [year ...] [--all] [--upload]
##
## --all recomputes every event in the given years (used to publish the
## history); --since-days N recomputes events that started in the last N
## days (the weekly job, once ShotLink tee times give the true hole order);
## WP_CORES sets how many events run at once.

if (requireNamespace("pkgload", quietly = TRUE)) pkgload::load_all(quiet = TRUE) else library(golfastr)
suppressMessages(library(dplyr))
source("data-raw/wp/helpers.R")

args <- commandArgs(trailingOnly = TRUE)
upload <- "--upload" %in% args
redo <- "--all" %in% args
since <- which(args == "--since-days")
since_days <- if (length(since)) as.integer(args[since + 1]) else NA
if (length(since)) args <- args[-c(since, since + 1)]
targets <- as.integer(args[!args %in% c("--upload", "--all")])
cores <- as.integer(Sys.getenv("WP_CORES", "1"))
start_file <- "data-raw/wp/starting_strokes.csv"
starting <- if (file.exists(start_file)) utils::read.csv(start_file, colClasses = "character")
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
  h_year$.row <- seq_len(nrow(h_year))
  have <- if (!redo && "win_prob" %in% names(h_year)) {
    unique(h_year$tournament_id[!is.na(h_year$win_prob)])
  } else character()
  recent <- if (is.na(since_days)) character() else
    events$tournament_id[!is.na(events$start_date) & events$start_date >= Sys.Date() - since_days]
  have <- setdiff(have, recent)
  todo <- events$tournament_id[events$year == year & events$wp_eligible & !events$tournament_id %in% have]
  limit <- as.integer(Sys.getenv("WP_LIMIT", "0"))
  if (limit > 0) todo <- utils::head(todo, limit)
  message(year, ": ", length(todo), " events need win probability")
  if (!length(todo)) next

  tee <- suppressMessages(load_tee_times(year))
  have_ids <- nrow(tee) && nrow(ids$events) && nrow(ids$players)
  order <- if (have_ids) hole_order_table(tee, ids$events, ids$players)
  waves <- if (have_ids) wave_table(tee, ids$events, ids$players)
  if (redo) h_year <- h_year[setdiff(names(h_year), wp_columns)]
  h_year[h_year$tournament_id %in% todo, intersect(wp_columns, names(h_year))] <- NA
  results <- parallel::mclapply(todo, function(id) {
    r <- rated$ratings[rated$ratings$tournament_id == id, ]
    ord <- if (!is.null(order)) order[order$tournament_id == id, ]
    if (!is.null(ord) && !nrow(ord)) ord <- NULL
    wv <- if (!is.null(waves)) waves[waves$tournament_id == id, ]
    if (!is.null(wv) && !nrow(wv)) wv <- NULL
    st <- if (!is.null(starting)) starting[starting$tournament_id == id, ]
    st <- if (!is.null(st) && nrow(st)) setNames(as.numeric(st$start), st$player_id)
    message("  ", events$tournament_name[events$tournament_id == id])
    event_win_probability(h_year[h_year$tournament_id == id, ], events[events$tournament_id == id, ],
                          setNames(r$rating, r$player_id), params, order = ord, wave = wv, start = st)
  }, mc.cores = cores, mc.preschedule = FALSE)
  failed <- vapply(results, inherits, TRUE, "try-error")
  if (any(failed)) stop("win probability failed for ", paste(todo[failed], collapse = ", "))
  results <- bind_rows(results)
  out <- attach_win_probability(h_year, results)
  out$.row <- NULL
  save_and_upload(out, sprintf("holes_%d", year), "holes", upload)
}

files <- ratings_files(holes[!holes$playoff, ], stroke, rated, params)
for (name in names(files)) save_and_upload(files[[name]], name, "skill_ratings", upload)
message("done")
