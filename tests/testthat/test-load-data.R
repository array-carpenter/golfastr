test_that("load_pga_hbh works", {
  skip_on_cran()
  skip_if_offline()

  # Test loading single season
  data <- load_pga_hbh(2025, tournaments = "401703504")

  expect_s3_class(data, "data.frame")
  expect_gt(nrow(data), 0)
  expect_true("tournament_name" %in% names(data))
  expect_true("hole_num" %in% names(data))
})

test_that("load_pga_leaderboards works", {
  skip_on_cran()
  skip_if_offline()

  data <- load_pga_leaderboards(2025, tournaments = "401703504")

  expect_s3_class(data, "data.frame")
  expect_gt(nrow(data), 0)
  expect_true("full_name" %in% names(data))
  expect_true("score_display" %in% names(data))
})

test_that("load_pga_schedule works", {
  skip_on_cran()
  skip_if_offline()

  schedule <- load_pga_schedule(2025)

  expect_s3_class(schedule, "data.frame")
  expect_gt(nrow(schedule), 40)  # Expect at least 40 tournaments
  expect_true("event_id" %in% names(schedule))
  expect_true("tournament_name" %in% names(schedule))
})

test_that("CSV files are created", {
  skip_on_cran()
  skip_if_offline()

  # Clean up first
  if (dir.exists("test_output")) unlink("test_output", recursive = TRUE)

  # Load data with custom dir
  data <- load_pga_hbh(2025, tournaments = "401703504", dir = "test_output")

  # Check CSV was created
  expect_true(file.exists("test_output/pga_2025_holes.csv"))

  # Clean up
  unlink("test_output", recursive = TRUE)
})
