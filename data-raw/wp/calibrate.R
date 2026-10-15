## Calibrate raw win probability and compare it with a simple benchmark
##
##   Rscript --vanilla data-raw/wp/calibrate.R
##
## Isotonic regression of won ~ raw win probability, fit separately for
## rounds 1-2 and rounds 3+ on 2004-2019, then each tick rescaled to sum to 1.
## Scored on 2020-2026 per tick: log-loss of the eventual winner and the
## Brier score summed over the field.

suppressMessages(library(dplyr))

cache <- "data-raw/wp/cache"
events <- readRDS(file.path(cache, "events.rds"))
files <- list.files(file.path(cache, "raw"), full.names = TRUE)
raw <- bind_rows(lapply(files, readRDS))
message(length(files), " events, ", nrow(raw), " player-holes")

raw <- raw[!is.na(raw$g), ]
raw$year <- events$year[match(raw$tournament_id, events$tournament_id)]
raw$won <- as.integer(raw$player_id == events$winner_id[match(raw$tournament_id, events$tournament_id)])
raw$stage <- ifelse(raw$g <= 36, "early", "late")
raw$train <- raw$year <= 2019

fit_iso <- function(x, y) {
  o <- order(x)
  iso <- stats::isoreg(x[o], y[o])
  knots <- data.frame(x = iso$x, y = iso$yf)
  knots <- knots[!duplicated(knots$y, fromLast = FALSE) | !duplicated(knots$y, fromLast = TRUE), ]
  knots <- knots[!duplicated(knots$x), ]
  rbind(data.frame(x = 0, y = 0), knots, data.frame(x = 1, y = 1))
}
apply_iso <- function(knots, x) stats::approx(knots$x, knots$y, x, ties = "ordered", rule = 2)$y

calib <- lapply(c(early = "early", late = "late"), function(s) {
  d <- raw[raw$train & raw$stage == s, ]
  fit_iso(d$win_prob, d$won)
})
raw$cal <- ifelse(raw$stage == "early", apply_iso(calib$early, raw$win_prob),
                  apply_iso(calib$late, raw$win_prob))
raw <- raw |>
  group_by(tournament_id, g) |>
  mutate(cal = ifelse(sum(cal) > 0, cal / sum(cal), win_prob)) |>
  ungroup()

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

saveRDS(calib, file.path(cache, "wp_calibration.rds"))
