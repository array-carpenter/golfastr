## Tune the pre-event skill rating and publish every event's ratings
##
##   Rscript --vanilla data-raw/wp/fit_skill.R [--upload]
##
## Round values are strokes better than the field, adjusted for field
## strength (the field's mean rating) so a round against a weak field counts
## for less. Ratings and field strength are solved by a few passes.
## Tuned on 2006-2019 by squared error predicting each round against the field.

devtools::load_all(quiet = TRUE)
suppressMessages(library(dplyr))
source("data-raw/wp/helpers.R")

cache <- "data-raw/wp/cache"
holes <- readRDS(file.path(cache, "holes_all.rds"))
events <- readRDS(file.path(cache, "events.rds"))
events <- events[!events$stableford, ]
holes <- holes[holes$tournament_id %in% events$tournament_id, ]

rounds <- wp_round_values(holes)
rounds$year <- events$year[match(rounds$tournament_id, events$tournament_id)]
message(nrow(rounds), " player-rounds")

rate <- function(half_life, prior_mean, prior_n, passes = 3) {
  rate_events(rounds, events, half_life, prior_mean, prior_n, passes)
}

score <- function(ratings) {
  r <- left_join(rounds, ratings, by = c("tournament_id", "player_id"))
  r$pred <- r$rating - stats::ave(r$rating, r$tournament_id, r$round, FUN = mean)
  fit <- r[r$year %in% 2006:2019, ]
  test <- r[r$year >= 2020, ]
  c(train = mean((fit$sg - fit$pred)^2), test = mean((test$sg - test$pred)^2),
    base = mean(test$sg^2))
}

grid <- expand.grid(half_life = c(35, 50, 65), prior_n = c(2, 3, 5),
                    prior_mean = c(-1.5, -2, -2.5, -3))
res <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  s <- score(rate(g$half_life, g$prior_mean, g$prior_n, passes = 2)$ratings)
  cbind(g, t(s))
}))
res <- res[order(res$train), ]
print(head(res, 10), row.names = FALSE)

best <- res[1, ]
final <- rate(best$half_life, best$prior_mean, best$prior_n, passes = 3)
s <- score(final$ratings)
message(sprintf("chosen half_life %g, prior_n %g, prior_mean %g: train MSE %.3f, test MSE %.3f (no skill %.3f)",
                best$half_life, best$prior_n, best$prior_mean, s["train"], s["test"], s["base"]))

skill_ratings <- final$ratings
attr(skill_ratings, "params") <- unlist(best[c("half_life", "prior_n", "prior_mean")])
saveRDS(skill_ratings, file.path(cache, "skill_ratings.rds"))
saveRDS(final$strength, file.path(cache, "field_strength.rds"))
