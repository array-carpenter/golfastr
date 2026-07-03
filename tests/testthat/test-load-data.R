test_that("load_schedule works", {
  skip_on_cran()
  skip_if_offline()

  schedule <- load_schedule(2025)

  expect_s3_class(schedule, "data.frame")
  expect_gt(nrow(schedule), 40)  # Expect at least 40 tournaments
  expect_true("event_id" %in% names(schedule))
  expect_true("tournament_name" %in% names(schedule))
})

test_that("load_leaderboard works against the live API", {
  skip_on_cran()
  skip_if_offline()

  data <- load_leaderboard(2025, "401703504", live = TRUE)

  expect_s3_class(data, "data.frame")
  expect_gt(nrow(data), 0)
  expect_true("player_name" %in% names(data))
  expect_true("score_to_par" %in% names(data))
})

test_that("load_holes works against the live API", {
  skip_on_cran()
  skip_if_offline()

  data <- load_holes(2025, "401703504", top_n = 3, live = TRUE)

  expect_s3_class(data, "data.frame")
  expect_gt(nrow(data), 0)
  expect_true("tournament_name" %in% names(data))
  expect_true("hole" %in% names(data))
})

test_that("legacy loaders warn but keep working", {
  skip_on_cran()
  skip_if_offline()

  lifecycle::expect_deprecated(schedule <- load_pga_schedule(2025))
  expect_s3_class(schedule, "data.frame")
  expect_gt(nrow(schedule), 40)
})

test_that("load_pga_hbh still writes CSV files (deprecated path)", {
  skip_on_cran()
  skip_if_offline()

  if (dir.exists("test_output")) unlink("test_output", recursive = TRUE)

  suppressWarnings(
    data <- load_pga_hbh(2025, tournaments = "401703504",
                         top_n = 3, dir = "test_output")
  )

  expect_true(file.exists("test_output/pga_2025_holes.csv"))

  unlink("test_output", recursive = TRUE)
})
