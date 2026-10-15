## Raw (uncalibrated) win probability for every eligible event, in parallel.
## Resumable: finished events are skipped.
##
##   Rscript --vanilla data-raw/wp/run_all.R [cores]

devtools::load_all(quiet = TRUE)

cache <- "data-raw/wp/cache"
out_dir <- file.path(cache, "raw")
dir.create(out_dir, showWarnings = FALSE)
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args)) as.integer(args[1]) else 4L

holes <- readRDS(file.path(cache, "holes_all.rds"))
events <- readRDS(file.path(cache, "events.rds"))
ratings <- readRDS(file.path(cache, "skill_ratings.rds"))
fit <- readRDS(file.path(cache, "wp_fit.rds"))

todo <- events$tournament_id[events$wp_eligible & !events$starting_strokes]
todo <- todo[!file.exists(file.path(out_dir, paste0(todo, ".rds")))]
message(length(todo), " events to run on ", cores, " cores")
holes <- split(holes, holes$tournament_id)[todo]

res <- parallel::mclapply(todo, function(id) {
  ev <- events[events$tournament_id == id, ]
  r <- ratings[ratings$tournament_id == id, ]
  out <- tryCatch(
    wp_event(holes[[id]], stats::setNames(r$rating, r$player_id), fit,
             cuts = ev$cuts[[1]], n_rounds = ev$rounds, sims = 10000),
    error = function(e) e
  )
  if (inherits(out, "error")) return(paste(id, conditionMessage(out)))
  keep <- c("tournament_id", "player_id", "round", "hole", "g", "win_prob", "win_prob_before")
  saveRDS(out[keep], file.path(out_dir, paste0(id, ".rds")))
  NA_character_
}, mc.cores = cores, mc.preschedule = FALSE)

failed <- unlist(res)
failed <- failed[!is.na(failed)]
message(length(todo) - length(failed), " done, ", length(failed), " failed")
if (length(failed)) writeLines(failed, file.path(cache, "raw_failed.txt"))
