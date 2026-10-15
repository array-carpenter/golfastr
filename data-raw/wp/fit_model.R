## Fit the hole score model used by the win probability engine
##
##   Rscript --vanilla data-raw/wp/fit_model.R
##
## 1. Round level (linear): a round against the field ~ a * centered rating
##    + w * this week's earlier rounds against the field.
## 2. Hole level (maximum likelihood): ordered-logit slope per par and the
##    spread of the round shock, integrating the shock over a Gauss-Hermite
##    grid. Fit on 2006-2019, checked on 2020-2026.

devtools::load_all(quiet = TRUE)
suppressMessages(library(dplyr))
set.seed(20261015)

cache <- "data-raw/wp/cache"
holes <- readRDS(file.path(cache, "holes_all.rds"))
events <- readRDS(file.path(cache, "events.rds"))
ratings <- readRDS(file.path(cache, "skill_ratings.rds"))
ok <- events$tournament_id[events$wp_eligible & !events$starting_strokes]
holes <- holes[holes$tournament_id %in% ok & !holes$playoff, ]
holes$to_par <- holes$score - holes$par

# Pooled hole distributions by par, the prior for every hole-round
par_prior <- lapply(c(`3` = 3, `4` = 4, `5` = 5), function(p) {
  wp_hole_pmf(holes$to_par[holes$par == p], prior = rep(0, length(wp_support)), prior_n = 0)
})

# --- round level -------------------------------------------------------------
rounds <- wp_round_values(holes) |>
  left_join(ratings[c("tournament_id", "player_id", "rating")], by = c("tournament_id", "player_id")) |>
  group_by(tournament_id, round) |>
  mutate(rating_c = rating - mean(rating)) |>
  ungroup() |>
  arrange(tournament_id, player_id, round) |>
  group_by(tournament_id, player_id) |>
  mutate(form = ifelse(round == 1, 0, (cumsum(sg) - sg) / (round - 1))) |>
  ungroup()
rounds$year <- events$year[match(rounds$tournament_id, events$tournament_id)]
train <- rounds$year %in% 2006:2019

fit_a <- lm(sg ~ 0 + rating_c, data = rounds[train & rounds$round == 1, ])
later <- rounds[train & rounds$round > 1, ]
later$form_c <- later$form - ave(later$form, later$tournament_id, later$round)
fit_w <- lm(sg ~ 0 + rating_c + form_c, data = later)
print(summary(fit_a)$coefficients)
print(summary(fit_w)$coefficients)
a <- unname(coef(fit_a)["rating_c"])
w <- unname(coef(fit_w)["form_c"])
message(sprintf("a = %.3f, w = %.3f, round residual sd = %.2f", a, w, sigma(fit_w)))

# --- hole level --------------------------------------------------------------
rounds$skill <- a * rounds$rating_c
rounds$skill[rounds$round > 1] <- rounds$skill[rounds$round > 1] + w *
  (rounds$form[rounds$round > 1] - ave(rounds$form[rounds$round > 1],
     rounds$tournament_id[rounds$round > 1], rounds$round[rounds$round > 1]))

hole_pmfs <- holes |>
  group_by(tournament_id, round, hole) |>
  summarise(par = first(par), pmf = list(wp_hole_pmf(to_par, par_prior[[as.character(first(par))]], 5)),
            .groups = "drop")
hole_key <- paste(hole_pmfs$tournament_id, hole_pmfs$round, hole_pmfs$hole)

sample_rounds <- function(rows, n) rows[sample(nrow(rows), min(n, nrow(rows))), ]
fit_set <- sample_rounds(rounds[train, ], 15000)
test_set <- sample_rounds(rounds[rounds$year >= 2020, ], 8000)

# Long table: one row per (player-round, hole) with that hole's pmf index
expand <- function(set) {
  x <- inner_join(holes[c("tournament_id", "player_id", "round", "hole", "par", "to_par")],
                  set[c("tournament_id", "player_id", "round", "skill")],
                  by = c("tournament_id", "player_id", "round"))
  x$pmf_id <- match(paste(x$tournament_id, x$round, x$hole), hole_key)
  x$pr_id <- as.integer(factor(paste(x$tournament_id, x$player_id, x$round)))
  x$bin <- pmin(pmax(x$to_par, min(wp_support)), max(wp_support)) - min(wp_support) + 1L
  x
}
fit_long <- expand(fit_set)
test_long <- expand(test_set)
cum_logit <- function(long) {
  cum <- t(vapply(hole_pmfs$pmf[long$pmf_id], function(p) cumsum(p)[-length(p)], numeric(length(wp_support) - 1)))
  stats::qlogis(pmin(pmax(cum, 1e-12), 1 - 1e-12))
}
fit_cl <- cum_logit(fit_long)
test_cl <- cum_logit(test_long)

# log P(observed bin) for every row at skill + shock, vectorized
row_logp <- function(long, cl, beta_by_par, shift) {
  b <- beta_by_par[as.character(long$par)]
  z <- cl + b * (long$skill + shift)
  cum <- cbind(0, stats::plogis(z), 1)
  p <- cum[cbind(seq_len(nrow(long)), long$bin + 1L)] - cum[cbind(seq_len(nrow(long)), long$bin)]
  log(pmax(p, 1e-12))
}

neg_loglik <- function(par, long, cl, k = 9) {
  beta <- c(`3` = par[1], `4` = par[2], `5` = par[3])
  sigma <- exp(par[4])
  g <- wp_shock_grid(k, sigma)
  per_node <- vapply(seq_len(k), function(j) {
    rowsum(row_logp(long, cl, beta, g$node[j]), long$pr_id)[, 1] + log(g$weight[j])
  }, numeric(max(long$pr_id)))
  m <- apply(per_node, 1, max)
  -sum(m + log(rowSums(exp(per_node - m))))
}

opt <- optim(c(0.1, 0.1, 0.1, log(2)), neg_loglik, long = fit_long, cl = fit_cl,
             method = "BFGS", control = list(maxit = 200))
beta <- c(`3` = opt$par[1], `4` = opt$par[2], `5` = opt$par[3])
sigma_round <- exp(opt$par[4])
print(round(c(beta, sigma_round = sigma_round), 4))

# Holdout: log-loss per hole against no skill and no shock
per_hole <- function(par) neg_loglik(par, test_long, test_cl) / nrow(test_long)
message(sprintf("holdout log-loss per hole: model %.4f, skill only %.4f, field only %.4f",
                per_hole(opt$par), per_hole(c(opt$par[1:3], log(1e-6))),
                per_hole(c(0, 0, 0, log(1e-6)))))

wp_fit <- list(a = a, w = w, beta = beta, sigma_round = sigma_round, par_prior = par_prior,
               prior_n_hole = 5, skill_params = attr(ratings, "params"))
saveRDS(wp_fit, file.path(cache, "wp_fit.rds"))
