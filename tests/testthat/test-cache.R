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

