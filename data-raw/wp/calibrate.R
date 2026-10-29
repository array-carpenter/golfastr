## Check calibration of raw win probability and compare it with a benchmark
##
##   Rscript --vanilla data-raw/wp/calibrate.R
##
## Temperature scaling of raw win probability, fit separately for rounds 1-2
## and rounds 3+ on 2004-2019; each tick still sums to 1.
## Scored on 2020-2026 per tick: log-loss of the eventual winner and the
## Brier score summed over the field.

suppressMessages(library(dplyr))

cache <- "data-raw/wp/cache"
events <- readRDS(file.path(cache, "events.rds"))
files <- file.path(cache, "raw", paste0(events$tournament_id[events$wp_eligible], ".rds"))
files <- files[file.exists(files)]
raw <- bind_rows(lapply(files, readRDS))
message(length(files), " events, ", nrow(raw), " player-holes")

raw <- raw[!is.na(raw$g), ]
raw$year <- events$year[match(raw$tournament_id, events$tournament_id)]
raw$won <- as.integer(raw$player_id == events$winner_id[match(raw$tournament_id, events$tournament_id)])
raw$stage <- ifelse(raw$g <= 36, "early", "late")
raw$train <- raw$year <= 2019

# Temperature scaling: within each tick, p_i^t / sum(p_j^t). One t per
# stage, chosen to minimise the winner's log-loss on 2004-2019.
scale_tick <- function(p, t, tick) {
  q <- p^t
  q / stats::ave(q, tick, FUN = sum)
}
tick_id <- paste(raw$tournament_id, raw$g)
fit_t <- function(s) {
  d <- raw$train & raw$stage == s & raw$win_prob > 0
  loss <- function(t) {
    q <- scale_tick(raw$win_prob[d], t, tick_id[d])
    -sum(log(pmax(q[raw$won[d] == 1], 1e-9)))
  }
  stats::optimize(loss, c(0.5, 1.5))$minimum
}
calib <- c(early = fit_t("early"), late = fit_t("late"))
print(round(calib, 3))
raw$cal <- raw$win_prob
for (s in names(calib)) {
  d <- raw$stage == s & raw$win_prob > 0
  raw$cal[d] <- scale_tick(raw$win_prob[d], calib[[s]], tick_id[d])
}

# Benchmark: logistic on shots behind the leader and holes left
holes <- readRDS(file.path(cache, "holes_all.rds"))
key <- paste(holes$tournament_id, holes$player_id, holes$round, holes$hole)
raw$cume <- holes$cume_to_par[match(paste(raw$tournament_id, raw$player_id, raw$round, raw$hole), key)]
raw <- raw |>
  group_by(tournament_id, g) |>
  mutate(behind = cume - min(cume)) |>
  ungroup() |>
  mutate(left = pmax(18 * events$rounds[match(tournament_id, events$tournament_id)] - g, 0))
bench <- glm(won ~ behind * log1p(left), family = binomial,
             data = raw[raw$train & raw$left > 0, ])
raw$bench <- stats::predict(bench, raw, type = "response")
raw <- raw |>
  group_by(tournament_id, g) |>
  mutate(bench = bench / sum(bench)) |>
  ungroup()

score <- function(d, col) {
  d |>
    group_by(tournament_id, g, stage) |>
    summarise(logloss = -log(pmax(sum(.data[[col]] * won), 1e-6)),
              brier = sum((.data[[col]] - won)^2), .groups = "drop") |>
    group_by(stage) |>
    summarise(logloss = mean(logloss), brier = mean(brier), .groups = "drop") |>
    mutate(model = col)
}
test <- raw[!raw$train & raw$won %in% 0:1, ]
has_winner <- test |> group_by(tournament_id, g) |> summarise(w = sum(won), .groups = "drop") |> filter(w == 1)
test <- semi_join(test, has_winner, by = c("tournament_id", "g"))
print(bind_rows(score(test, "win_prob"), score(test, "cal"), score(test, "bench")) |> arrange(stage, logloss))

bands <- c(0, 0.01, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 0.99, 1)
rel <- function(col) test |>
  mutate(band = cut(.data[[col]], bands, include.lowest = TRUE)) |>
  group_by(band) |>
  summarise(n = n(), predicted = mean(.data[[col]]), actual = mean(won), .groups = "drop")
message("\nreliability, raw")
print(as.data.frame(rel("win_prob")), digits = 3)
message("\nreliability, calibrated")
print(as.data.frame(rel("cal")), digits = 3)

# Fitted temperatures are 0.99 (rounds 1-2) and 1.02 (rounds 3+): the raw
# model is already calibrated, so the package ships it unscaled.
