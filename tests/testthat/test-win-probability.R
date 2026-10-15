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
  u <- array(stats::runif(3 * 40000), c(3, 40000, 1))
  sim <- wp_sim(c(0, 0, 0), list(d), integer(), u)
  expect_equal(sim, wp_exact(d), tolerance = 0.01)
})

test_that("players below the cut cannot win", {
  # player 3 always finishes the first stretch 10 back with a top-2 cut
  first <- list(wp_point(0L), wp_point(0L), wp_point(10L))
  second <- list(dist(0, c(0.5, 0.5)), dist(0, c(0.5, 0.5)), wp_point(-50L))
  u <- array(stats::runif(3 * 2000 * 2), c(3, 2000, 2))
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
