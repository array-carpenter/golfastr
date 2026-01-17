test_that("cache functions work", {
  # Clear cache to start fresh
  clear_cache(confirm = FALSE)

  # Cache directory should exist after clearing
  cache_dir <- if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("LOCALAPPDATA"), "golfastr")
  } else {
    file.path(Sys.getenv("HOME"), ".golfastr")
  }

  expect_true(dir.exists(cache_dir))
})

test_that("cache_info provides information", {
  # This should run without error (may produce messages)
  expect_no_error(cache_info())
})

test_that("caching speeds up subsequent calls", {
  skip_on_cran()
  skip_if_offline()

  # First call (no cache)
  start1 <- Sys.time()
  data1 <- load_pga_hbh(2025, tournaments = "401703504")
  time1 <- as.numeric(difftime(Sys.time(), start1, units = "secs"))

  # Second call (should use cache)
  start2 <- Sys.time()
  data2 <- load_pga_hbh(2025, tournaments = "401703504")
  time2 <- as.numeric(difftime(Sys.time(), start2, units = "secs"))

  # Cached call should be faster
  expect_lt(time2, time1)

  # Data should be identical
  expect_equal(nrow(data1), nrow(data2))
})
