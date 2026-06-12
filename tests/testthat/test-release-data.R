test_that("release_load returns NULL for missing assets", {
  skip_on_cran()
  skip_if_offline()

  result <- release_load("leaderboards", "leaderboards_1900.rds")
  expect_null(result)
})

test_that("release_load caches downloads for the session", {
  skip_on_cran()
  skip_if_offline()

  data <- release_load("leaderboards", "leaderboards_2026.rds")
  skip_if(is.null(data), "hosted 2026 data not available")

  expect_s3_class(data, "tbl_df")
  expect_true(all(c("player_id", "tournament_id", "tournament_name", "year")
                  %in% names(data)))

  # Second call should hit the session cache
  t <- system.time(release_load("leaderboards", "leaderboards_2026.rds"))
  expect_lt(t["elapsed"], 0.1)
})

test_that("load_leaderboard uses hosted data", {
  skip_on_cran()
  skip_if_offline()
  skip_if(is.null(release_load("leaderboards", "leaderboards_2026.rds")),
          "hosted 2026 data not available")

  lb <- load_leaderboard(2026, "Sony")

  expect_s3_class(lb, "data.frame")
  expect_gt(nrow(lb), 0)
  expect_true(all(grepl("Sony", lb$tournament_name)))
})

test_that("load_holes applies top_n to hosted data", {
  skip_on_cran()
  skip_if_offline()
  skip_if(is.null(release_load("holes", "holes_2026.rds")),
          "hosted 2026 data not available")

  holes <- load_holes(2026, "Sony", top_n = 5)

  expect_s3_class(holes, "data.frame")
  expect_gt(nrow(holes), 0)
  expect_lte(length(unique(holes$player_id)), 5)
  expect_true("position" %in% names(holes))
})
