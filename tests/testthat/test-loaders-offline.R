# Loader logic against fixture releases - no network needed.

test_that("load_leaderboard returns the full season without a tournament", {
  base <- make_release_dir(2004)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "off")
  on.exit(options(old), add = TRUE)

  season <- load_leaderboard(2004)
  expect_equal(nrow(season), 4)
  expect_setequal(unique(season$tournament_id), c("100", "200"))
})

test_that("load_leaderboard filters hosted data by event id", {
  base <- make_release_dir(2004)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "off")
  on.exit(options(old), add = TRUE)

  by_id <- load_leaderboard(2004, "100")
  expect_equal(nrow(by_id), 2)
  expect_equal(unique(by_id$tournament_name), "Fake Masters")
})

test_that("load_leaderboard matches hosted data by partial name", {
  base <- make_release_dir(2004)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "off")
  on.exit(options(old), add = TRUE)

  by_name <- load_leaderboard(2004, "sony")
  expect_equal(unique(by_name$tournament_id), "200")
})

test_that("load_holes serves hosted scorecards and applies top_n", {
  base <- make_release_dir(2004)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "off")
  on.exit(options(old), add = TRUE)

  full <- load_holes(2004, "Fake Masters")
  expect_equal(nrow(full), 4)

  top1 <- load_holes(2004, "100", top_n = 1)
  expect_equal(unique(top1$player_id), "p1")
})

test_that("load_holes requires a tournament", {
  expect_error(load_holes(2004), "tournament")
})
