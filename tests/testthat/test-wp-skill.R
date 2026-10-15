round_rows <- function(player, event, sg, round = seq_along(sg)) {
  tibble::tibble(tournament_id = event, player_id = player, round = as.integer(round), sg = sg)
}

events_tbl <- tibble::tibble(
  tournament_id = c("E1", "E2", "E3"),
  start_date = as.Date(c("2024-01-04", "2024-01-11", "2024-01-18"))
)

test_that("round values are strokes better than the field average", {
  holes <- expand.grid(hole = 1:18, player_id = c("a", "b"), stringsAsFactors = FALSE)
  holes$tournament_id <- "E1"
  holes$round <- 1L
  holes$score <- ifelse(holes$player_id == "a", 4L, 5L)
  rv <- wp_round_values(holes)
  expect_equal(rv$sg[rv$player_id == "a"], 9)
  expect_equal(rv$sg[rv$player_id == "b"], -9)
})

test_that("unfinished rounds are left out", {
  holes <- expand.grid(hole = 1:18, player_id = c("a", "b", "c"), stringsAsFactors = FALSE)
  holes$tournament_id <- "E1"
  holes$round <- 1L
  holes$score <- 4L
  holes <- holes[!(holes$player_id == "c" & holes$hole > 9), ]
  expect_setequal(wp_round_values(holes)$player_id, c("a", "b"))
})

test_that("a player with no history gets the prior", {
  r <- round_rows("a", "E2", c(1, 1))
  out <- wp_skill_ratings(r, events_tbl, half_life = 50, prior_mean = -1, prior_n = 10)
  expect_equal(out$rating[out$tournament_id == "E2"], -1)
  expect_equal(out$n_rounds[out$tournament_id == "E2"], 0)
})

test_that("only rounds before the event count", {
  r <- rbind(round_rows("a", "E1", c(2, 2)), round_rows("a", "E2", c(0, 0)),
             round_rows("a", "E3", c(-9, -9)))
  out <- wp_skill_ratings(r, events_tbl, half_life = 1e9, prior_mean = 0, prior_n = 0)
  expect_equal(out$rating[out$tournament_id == "E2"], 2)
  expect_equal(out$rating[out$tournament_id == "E3"], 1)
})

test_that("recent rounds weigh more and the prior pulls toward its mean", {
  r <- rbind(round_rows("a", "E1", c(0, 0, 0, 0)), round_rows("a", "E2", c(4, 4, 4, 4)),
             round_rows("a", "E3", 0))
  flat <- wp_skill_ratings(r, events_tbl, half_life = 1e9, prior_mean = 0, prior_n = 0)
  recent <- wp_skill_ratings(r, events_tbl, half_life = 2, prior_mean = 0, prior_n = 0)
  shrunk <- wp_skill_ratings(r, events_tbl, half_life = 1e9, prior_mean = 0, prior_n = 8)
  e3 <- function(x) x$rating[x$tournament_id == "E3"]
  expect_equal(e3(flat), 2)
  expect_gt(e3(recent), 3)
  expect_equal(e3(shrunk), 1)
})
