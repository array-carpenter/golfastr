flat_prior <- function() {
  p <- c(0.002, 0.02, 0.18, 0.6, 0.16, 0.03, 0.005, 0.002, 0.001)
  p / sum(p)
}

test_that("hole pmfs sum to one and lean on the prior when the field is small", {
  pmf <- wp_hole_pmf(c(0, 0, 1, -1), prior = flat_prior(), prior_n = 4)
  expect_equal(sum(pmf), 1)
  expect_length(pmf, length(wp_support))
  expect_gt(pmf[wp_support == -2], 0)
  big <- wp_hole_pmf(rep(0, 1000), prior = flat_prior(), prior_n = 4)
  expect_gt(big[wp_support == 0], 0.99)
})

test_that("scores beyond the support are folded into the end bins", {
  pmf <- wp_hole_pmf(c(-4, 9), prior = flat_prior(), prior_n = 0)
  expect_equal(pmf[1], 0.5)
  expect_equal(pmf[length(pmf)], 0.5)
})

test_that("a skill shift moves mass toward lower scores", {
  pmf <- flat_prior()
  mean_score <- function(p) sum(p * wp_support)
  expect_equal(wp_shift_pmf(pmf, 0, beta = 0.1)[1, ], pmf)
  shifted <- wp_shift_pmf(pmf, c(-2, 0, 2), beta = 0.1)
  expect_equal(rowSums(shifted), rep(1, 3))
  means <- apply(shifted, 1, mean_score)
  expect_true(all(diff(means) < 0))
})

test_that("the shock grid has unit weight and the right spread", {
  g <- wp_shock_grid(9, sigma = 2.5)
  expect_equal(sum(g$weight), 1)
  expect_equal(sum(g$weight * g$node), 0, tolerance = 1e-10)
  expect_equal(sqrt(sum(g$weight * g$node^2)), 2.5, tolerance = 1e-6)
})

test_that("convolution adds supports and keeps total mass", {
  a <- list(lo = -1L, p = c(0.25, 0.5, 0.25))
  b <- list(lo = 0L, p = c(0.5, 0.5))
  ab <- wp_convolve(a, b)
  expect_equal(ab$lo, -1L)
  expect_equal(ab$p, c(0.125, 0.375, 0.375, 0.125))
  expect_equal(wp_convolve(a, wp_point(0L)), a)
})

test_that("the shock posterior favours good shocks after good holes", {
  g <- wp_shock_grid(9, sigma = 3)
  pmfs <- lapply(1:5, function(i) flat_prior())
  post_good <- wp_shock_posterior(rep(-1L, 5), pmfs, skill = 0, beta = 0.1, grid = g)
  post_bad <- wp_shock_posterior(rep(1L, 5), pmfs, skill = 0, beta = 0.1, grid = g)
  expect_equal(sum(post_good), 1)
  # a positive shock means playing better
  expect_gt(sum(post_good * g$node), 0)
  expect_lt(sum(post_bad * g$node), 0)
  expect_equal(wp_shock_posterior(integer(), list(), 0, 0.1, g), g$weight)
})
