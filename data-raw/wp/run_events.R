## Run the win probability engine on chosen events and print the story
##
##   Rscript --vanilla data-raw/wp/run_events.R 2016 "Masters" [2025 "PGA Championship" ...]

devtools::load_all(quiet = TRUE)
suppressMessages(library(dplyr))

cache <- "data-raw/wp/cache"
holes <- readRDS(file.path(cache, "holes_all.rds"))
events <- readRDS(file.path(cache, "events.rds"))
ratings <- readRDS(file.path(cache, "skill_ratings.rds"))
fit <- readRDS(file.path(cache, "wp_fit.rds"))

run_event <- function(id, sims = 10000) {
  ev <- events[events$tournament_id == id, ]
  h <- holes[holes$tournament_id == id, ]
  r <- ratings[ratings$tournament_id == id, ]
  t0 <- Sys.time()
  out <- wp_event(h, setNames(r$rating, r$player_id), fit, cuts = ev$cuts[[1]],
                  n_rounds = ev$rounds, sims = sims)
  attr(out, "seconds") <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  out
}

story <- function(out) {
  nm <- out[!duplicated(out$player_id), c("player_id", "player_name")]
  ticks <- out |>
    filter(!is.na(g)) |>
    group_by(g) |>
    slice_max(win_prob, n = 1, with_ties = FALSE) |>
    ungroup()
  show <- ticks[ticks$g %in% c(18, 36, 54, 63, 66, 68, 69, 70, 71, 72), ]
  print(as.data.frame(transmute(show, tick = g, leader = player_name, win_prob = round(win_prob, 3))))
  big <- out |> arrange(desc(abs(wpa))) |> head(8)
  print(as.data.frame(transmute(big, player_name, round, hole, to_par,
                                before = round(win_prob_before, 3), after = round(win_prob, 3))))
}

args <- commandArgs(trailingOnly = TRUE)
pairs <- split(args, rep(seq_len(length(args) / 2), each = 2))
for (p in pairs) {
  ev <- events[events$year == as.integer(p[1]) & grepl(p[2], events$tournament_name), ]
  for (id in ev$tournament_id) {
    out <- run_event(id)
    message("\n", ev$year[ev$tournament_id == id], " ", ev$tournament_name[ev$tournament_id == id],
            sprintf(" (%.0f s)", attr(out, "seconds")))
    story(out)
    saveRDS(out, file.path(cache, sprintf("wp_%s.rds", id)))
  }
}
