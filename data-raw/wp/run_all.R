## Raw (uncalibrated) win probability for every eligible event, in parallel.
## Resumable: finished events are skipped.
##
##   Rscript --vanilla data-raw/wp/run_all.R [cores]

devtools::load_all(quiet = TRUE)

cache <- "data-raw/wp/cache"
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args)) as.integer(args[1]) else 4L

events <- readRDS(file.path(cache, "events.rds"))
fit <- readRDS(file.path(cache, "wp_fit.rds"))
start <- utils::read.csv("data-raw/wp/starting_strokes.csv", colClasses = "character")
order_file <- file.path(cache, "hole_order.rds")
hole_order <- if (file.exists(order_file)) split(readRDS(order_file), readRDS(order_file)$tournament_id)
wave_file <- file.path(cache, "waves.rds")
waves <- if (file.exists(wave_file)) split(readRDS(wave_file), readRDS(wave_file)$tournament_id)
out_dir <- file.path(cache, Sys.getenv("WP_RUN", "raw"))
dir.create(out_dir, showWarnings = FALSE)

# One input file per event so each worker only loads its own event
in_dir <- file.path(cache, "event_inputs")
if (!dir.exists(in_dir)) {
  dir.create(in_dir)
  holes <- readRDS(file.path(cache, "holes_all.rds"))
  ratings <- readRDS(file.path(cache, "skill_ratings.rds"))
  cols <- c("tournament_id", "player_id", "player_name", "round", "hole", "par", "score", "cume_to_par")
  hs <- split(holes[cols], holes$tournament_id)
  rs <- split(ratings, ratings$tournament_id)
  for (id in names(hs)) saveRDS(list(holes = hs[[id]], ratings = rs[[id]]), file.path(in_dir, paste0(id, ".rds")))
  rm(holes, ratings, hs, rs)
  invisible(gc())
}

todo <- events$tournament_id[events$wp_eligible]
todo <- todo[!file.exists(file.path(out_dir, paste0(todo, ".rds")))]
message(length(todo), " events to run on ", cores, " cores")

res <- parallel::mclapply(todo, function(id) {
  ev <- events[events$tournament_id == id, ]
  input <- readRDS(file.path(in_dir, paste0(id, ".rds")))
  r <- input$ratings
  st <- start[start$tournament_id == id, ]
  out <- tryCatch(
    wp_event(input$holes, stats::setNames(r$rating, r$player_id), fit,
             cuts = ev$cuts[[1]], n_rounds = ev$rounds, sims = 10000,
             start = if (nrow(st)) stats::setNames(as.numeric(st$start), st$player_id),
             order = hole_order[[id]], wave = waves[[id]]),
    error = function(e) e
  )
  if (inherits(out, "error")) return(paste(id, conditionMessage(out)))
  out$hole_order_source <- if (is.null(hole_order[[id]])) "assumed" else
    ifelse(paste(out$player_id, out$round) %in% paste(hole_order[[id]]$player_id, hole_order[[id]]$round),
           "shotlink", "assumed")
  keep <- c("tournament_id", "player_id", "round", "hole", "g", "win_prob", "win_prob_before",
            "skill_rating", "hole_order_source")
  saveRDS(out[keep], file.path(out_dir, paste0(id, ".rds")))
  NA_character_
}, mc.cores = cores, mc.preschedule = FALSE)

failed <- unlist(res)
failed <- failed[!is.na(failed)]
message(length(todo) - length(failed), " done, ", length(failed), " failed")
if (length(failed)) writeLines(failed, file.path(cache, "raw_failed.txt"))
