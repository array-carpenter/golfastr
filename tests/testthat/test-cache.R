test_that("cache functions work", {
  skip_on_cran()

  # Test save and load cycle using a temp file
  cache_dir <- tools::R_user_dir("golfastr", "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  test_data <- data.frame(x = 1:3, y = letters[1:3])
  test_file <- "test_cache_data.rds"
  test_path <- file.path(cache_dir, test_file)

  # Save
  cache_save(test_data, test_file)
  expect_true(file.exists(test_path))


  # Load
  loaded <- cache_load(test_file)
  expect_equal(loaded, test_data)

  # Check exists
  expect_true(cache_exists(test_file))

  # Clean up test file
  unlink(test_path)
})

test_that("cache_info runs without error", {
  # This only reads, doesn't write - safe for CRAN
  expect_no_error(cache_info())
})

test_that("clear_cache handles non-existent directory", {
  # This tests the message path, doesn't create anything
  expect_message(clear_cache(confirm = FALSE), "Nothing to clear|already empty")
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
