dist <- function(lo, p) list(lo = as.integer(lo), p = p / sum(p))

test_that("exact win probability sums to one and splits ties evenly", {
  same <- dist(-2, c(0.25, 0.5, 0.25))
  wp <- wp_exact(list(same, same, same))
  expect_equal(sum(wp), 1)
  expect_equal(wp, rep(1 / 3, 3))
  expect_equal(wp_exact(list(wp_point(-5L), wp_point(-5L))), c(0.5, 0.5))
})

test_that("a big lead with little golf left is nearly certain", {
  wp <- wp_exact(list(dist(-20, c(0.2, 0.6, 0.2)), dist(0, c(0.2, 0.6, 0.2)),
                      dist(-1, c(0.2, 0.6, 0.2))))
  expect_gt(wp[1], 0.999)
})

test_that("exact results match brute-force enumeration", {
  a <- dist(-1, c(0.3, 0.4, 0.3))
  b <- dist(0, c(0.5, 0.5))
  c <- dist(-2, c(0.1, 0.2, 0.7))
  grid <- expand.grid(a = seq_along(a$p), b = seq_along(b$p), c = seq_along(c$p))
  brute <- c(0, 0, 0)
  for (r in seq_len(nrow(grid))) {
    tot <- c(a$lo + grid$a[r] - 1, b$lo + grid$b[r] - 1, c$lo + grid$c[r] - 1)
    pr <- a$p[grid$a[r]] * b$p[grid$b[r]] * c$p[grid$c[r]]
    lead <- tot == min(tot)
    brute <- brute + pr * lead / sum(lead)
  }
  expect_equal(wp_exact(list(a, b, c)), brute, tolerance = 1e-10)
})

test_that("simulation agrees with the exact answer when no cut is left", {
  d <- list(dist(-1, c(0.3, 0.4, 0.3)), dist(0, c(0.5, 0.5)), dist(-2, c(0.1, 0.2, 0.7)))
  set.seed(1)
  u <- array(stats::runif(3 * 40000), c(40000, 3, 1))
  sim <- wp_sim(c(0, 0, 0), list(d), integer(), u)
  expect_equal(sim, wp_exact(d), tolerance = 0.01)
})

test_that("players below the cut cannot win", {
  # player 3 always finishes the first stretch 10 back with a top-2 cut
  first <- list(wp_point(0L), wp_point(0L), wp_point(10L))
  second <- list(dist(0, c(0.5, 0.5)), dist(0, c(0.5, 0.5)), wp_point(-50L))
  u <- array(stats::runif(3 * 2000 * 2), c(2000, 3, 2))
  sim <- wp_sim(c(0, 0, 0), list(first, second), 2L, u)
  expect_equal(sim[3], 0)
  expect_equal(sum(sim), 1)
})

test_that("draws stay inside each distribution's support", {
  d <- dist(-3, c(0.2, 0.3, 0.5))
  x <- wp_draw(d, c(1e-12, 0.2, 0.2000001, 0.5, 0.999999999, 1))
  expect_true(all(x >= -3 & x <= -1))
  expect_equal(x[1:2], c(-3L, -3L))
})

test_that("suffix distributions shrink to a point as holes are played", {
  cl <- matrix(stats::qlogis(cumsum(c(0.01, 0.02, 0.2, 0.55, 0.17, 0.04, 0.007, 0.002, 0.001))[-9]),
               nrow = 3, ncol = 8, byrow = TRUE)
  suf <- wp_suffixes(cl, 0.1, 0)
  expect_length(suf, 4)
  expect_equal(sum(suf[[1]]$p), 1)
  expect_equal(suf[[4]], wp_point(0L))
  expect_equal(suf[[1]]$lo, -9L)
})

test_params <- function() {
  prior <- c(0.001, 0.02, 0.2, 0.6, 0.15, 0.025, 0.003, 0.0007, 0.0003)
  prior <- prior / sum(prior)
  list(a = 1, w = 0.1, beta = c(`3` = 0.08, `4` = 0.08, `5` = 0.08), sigma_round = 2.5,
       par_prior = list(`3` = prior, `4` = prior, `5` = prior), prior_n_hole = 5,
       skill_params = c(half_life = 35, prior_n = 5, prior_mean = -2.5))
}

fake_event <- function(n_players = 6, rounds = 4, seed = 1) {
  set.seed(seed)
  x <- expand.grid(hole = 1:18, round = seq_len(rounds), player_id = as.character(seq_len(n_players)),
                   stringsAsFactors = FALSE)
  x$par <- 4L
  x$score <- 4L + sample(c(-1L, 0L, 0L, 0L, 1L), nrow(x), replace = TRUE)
  x$tournament_id <- "T1"
  x
}

test_that("event win probability sums to one after every hole and ends with the winner", {
  ev <- fake_event()
  out <- wp_event(ev, ratings = c(`1` = 1, `2` = 0), params = test_params(), sims = 2000)
  expect_equal(nrow(out), nrow(ev))
  by_tick <- tapply(out$win_prob, out$g, sum)
  expect_true(all(abs(by_tick - 1) < 1e-8))
  totals <- tapply(ev$score, ev$player_id, sum)
  leaders <- names(totals)[totals == min(totals)]
  last <- out[out$g == max(out$g), ]
  expect_equal(sum(last$win_prob[last$player_id %in% leaders]), 1)
  expect_equal(out$wpa, out$win_prob - out$win_prob_before)
})

test_that("a missed cut drops a player to zero and the same seed repeats", {
  ev <- fake_event(n_players = 8)
  ev$score[ev$player_id == "8" & ev$round <= 2] <- 7L
  ev <- ev[!(ev$player_id == "8" & ev$round > 2), ]
  cuts <- data.frame(after_round = 2L, rule = 7L)
  a <- wp_event(ev, c(), test_params(), cuts = cuts, sims = 1000, seed = 3)
  b <- wp_event(ev, c(), test_params(), cuts = cuts, sims = 1000, seed = 3)
  expect_identical(a$win_prob, b$win_prob)
  p8 <- a[a$player_id == "8", ]
  expect_equal(p8$win_prob[p8$g == 36], 0)
  expect_true(all(abs(tapply(a$win_prob, a$g, sum) - 1) < 1e-8))
})

test_that("playoff holes settle the tie", {
  ev <- fake_event(n_players = 10, rounds = 4)
  ev$score[!ev$player_id %in% c("1", "2") & ev$round == 1] <- 6L
  t12 <- tapply(ev$score, ev$player_id, sum)
  ev$score[ev$player_id == "2" & ev$round == 4 & ev$hole == 18] <-
    ev$score[ev$player_id == "2" & ev$round == 4 & ev$hole == 18] + (t12[["1"]] - t12[["2"]])
  po <- data.frame(hole = 18L, round = 5L, player_id = c("1", "2"), par = 4L, score = c(4L, 5L),
                   tournament_id = "T1")
  out <- wp_event(rbind(ev, po), c(), test_params(), sims = 500)
  last <- out[out$round == 5, ]
  expect_equal(last$win_prob_before, c(0.5, 0.5))
  expect_equal(last$win_prob[last$player_id == "1"], 1)
  expect_equal(last$win_prob[last$player_id == "2"], 0)
})

test_that("add_win_probability keeps row order and adds the columns", {
  ev <- fake_event(n_players = 5, rounds = 4)
  ev <- ev[sample(nrow(ev)), ]
  out <- add_win_probability(ev, sims = 500, ratings = c(`1` = 2))
  expect_equal(out[names(ev)], tibble::as_tibble(ev), ignore_attr = TRUE)
  expect_true(all(c("win_prob", "win_prob_before", "wpa", "skill_rating") %in% names(out)))
  expect_true(all(out$win_prob >= 0 & out$win_prob <= 1))
  expect_equal(out$skill_rating[out$player_id == "1"][1], 2)
})

test_that("Stableford events are left as NA", {
  ev <- fake_event(n_players = 4, rounds = 1)
  ev$tournament_name <- "Barracuda Championship"
  expect_message(out <- add_win_probability(ev, ratings = c()), "not individual stroke play")
  expect_true(all(is.na(out$win_prob)))
})

test_that("cuts already made are read from the data", {
  ev <- fake_event(n_players = 12, rounds = 4)
  ev$score[ev$player_id %in% as.character(7:12) & ev$round <= 2] <- 6L
  ev <- ev[!(ev$player_id %in% as.character(7:12) & ev$round > 2), ]
  cuts <- wp_find_cuts(ev)
  expect_equal(cuts$after_round, 2L)
  expect_equal(cuts$rule, 6L)
})

test_that("a hole missing from the data counts as par and keeps ticks aligned", {
  ev <- fake_event(n_players = 6, rounds = 2)
  ev <- ev[!(ev$player_id == "2" & ev$round == 1 & ev$hole == 5), ]
  out <- wp_event(ev, c(), test_params(), sims = 500)
  p2 <- out[out$player_id == "2", ]
  expect_false(5 %in% p2$g)
  expect_equal(max(p2$g), 36)
  sums <- tapply(out$win_prob, out$g, sum)
  expect_true(all(abs(sums[names(sums) != "5"] - 1) < 1e-8))
})

test_that("starting on the 10th tee shifts the order of play", {
  ev <- fake_event(n_players = 4, rounds = 1)
  order <- data.frame(player_id = "1", round = 1L, hole = 1:18, seq = (1:18 - 10) %% 18 + 1)
  out <- wp_event(ev, c(), test_params(), order = order, sims = 500)
  p1 <- out[out$player_id == "1", ]
  expect_equal(p1$g[p1$hole == 10], 1)
  expect_equal(p1$g[p1$hole == 9], 18)
})

test_that("holes still to play in the harder wave lower a player's chances", {
  ev <- fake_event(n_players = 20, rounds = 1, seed = 4)
  pm <- as.character(11:20)
  # the afternoon wave plays the back nine harder
  back <- ev$player_id %in% pm & ev$hole >= 10
  ev$score[back] <- ev$score[back] + rep(c(1L, 0L, 1L), length.out = sum(back))
  # one player from each wave level par through nine
  ev$score[ev$player_id %in% c("1", "11") & ev$hole <= 9] <- 4L
  wave <- data.frame(player_id = as.character(1:20), round = 1L, wave = ifelse(1:20 <= 10, "AM", "PM"))
  without <- wp_event(ev, c(), test_params(), n_rounds = 1, sims = 500)
  with <- wp_event(ev, c(), test_params(), n_rounds = 1, wave = wave, sims = 500)
  at <- function(x, p) x$win_prob[x$player_id == p & x$g == 9]
  expect_equal(at(without, "1"), at(without, "11"))
  expect_lt(at(with, "11"), at(with, "1"))
})

test_that("live: players behind in their round stay in the field", {
  ev <- fake_event(n_players = 20, rounds = 2, seed = 7)
  # half the field is through 18 holes of round 2, the rest through 3
  ev <- ev[!(ev$round == 2 & as.integer(ev$player_id) > 10 & ev$hole > 3), ]
  out <- wp_event(ev, c(), test_params(), n_rounds = 4, live = TRUE, sims = 1000)
  expect_false(any(is.na(out$win_prob)))
  expect_equal(sum(attr(out, "current")), 1)
  expect_true(all(attr(out, "current")[as.character(11:20)] > 0))
  sums <- tapply(out$win_prob, out$g, sum)
  expect_true(all(abs(sums[as.integer(names(sums)) <= 21] - 1) < 1e-8))
})

test_that("live: players through the cut who have not teed off keep their chances", {
  ev <- fake_event(n_players = 12, rounds = 3, seed = 8)
  ev$score[as.integer(ev$player_id) > 6 & ev$round <= 2 & ev$hole <= 6] <- 6L
  ev <- ev[!(as.integer(ev$player_id) > 6 & ev$round == 3), ]
  ev <- ev[!(ev$player_id %in% c("5", "6") & ev$round == 3), ]
  out <- wp_event(ev, c(), test_params(), n_rounds = 4,
                  cuts = data.frame(after_round = 2L, rule = 6L), live = TRUE, sims = 1000)
  now <- attr(out, "current")
  expect_gt(now[["5"]], 0)
  expect_equal(unname(now[as.character(7:12)]), rep(0, 6))
  expect_equal(sum(now), 1)
})

test_that("a playoff hole played more than once is kept in order", {
  ev <- fake_event(n_players = 10, rounds = 4, seed = 9)
  ev$score[!ev$player_id %in% c("1", "2") & ev$round == 1] <- 6L
  t12 <- tapply(ev$score, ev$player_id, sum)
  last <- ev$player_id == "2" & ev$round == 4 & ev$hole == 18
  ev$score[last] <- ev$score[last] + (t12[["1"]] - t12[["2"]])
  po <- data.frame(hole = 18L, round = 5L, player_id = c("1", "2", "1", "2"), par = 4L,
                   score = c(4L, 4L, 4L, 5L), tournament_id = "T1")
  out <- wp_event(rbind(ev, po), c(), test_params(), n_rounds = 4, sims = 500)
  p <- out[out$round == 5, ]
  expect_equal(nrow(p), 4)
  expect_equal(p$win_prob, c(0.5, 0.5, 1, 0))
})

test_that("missing scores are skipped, not fatal", {
  ev <- fake_event(n_players = 5, rounds = 2)
  ev$score[1:3] <- NA
  out <- wp_event(ev, c(), test_params(), n_rounds = 2, sims = 500)
  expect_equal(nrow(out), nrow(ev) - 3)
})

test_that("add_win_probability returns current chances and spots a live event", {
  ev <- fake_event(n_players = 8, rounds = 2, seed = 11)
  ev <- ev[!(ev$round == 2 & ev$player_id %in% c("1", "2") & ev$hole > 9), ]
  expect_true(wp_in_progress(ev, rounds = 4))
  out <- add_win_probability(ev, sims = 500, ratings = c(), cut = NA)
  now <- attr(out, "current")
  expect_equal(nrow(now), 8)
  expect_equal(sum(now$win_prob), 1)
  done <- fake_event(n_players = 4, rounds = 4)
  expect_false(wp_in_progress(done, rounds = 4))
})
