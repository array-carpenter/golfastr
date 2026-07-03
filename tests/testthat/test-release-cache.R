test_that("cache mode off downloads fresh every call", {
  base <- make_release_dir(2001)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "off")
  on.exit(options(old), add = TRUE)

  d1 <- release_load("leaderboards", "leaderboards_2001.rds")
  expect_s3_class(d1, "tbl_df")
  expect_equal(nrow(d1), 4)

  # Remove the source: with caching off the next call must miss
  unlink(file.path(base, "leaderboards", "leaderboards_2001.rds"))
  expect_null(release_load("leaderboards", "leaderboards_2001.rds"))
})

test_that("memory cache serves repeat calls without re-downloading", {
  base <- make_release_dir(2002)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "memory")
  on.exit(options(old), add = TRUE)

  d1 <- release_load("leaderboards", "leaderboards_2002.rds")
  expect_s3_class(d1, "tbl_df")

  # Remove the source: the session cache should still answer
  unlink(file.path(base, "leaderboards", "leaderboards_2002.rds"))
  d2 <- release_load("leaderboards", "leaderboards_2002.rds")
  expect_identical(d1, d2)
})

test_that("filesystem cache persists and expires after 24 hours", {
  skip_on_cran()

  base <- make_release_dir(2003)
  cache_file <- file.path(get_cache_dir(), "releases", "leaderboards",
                          "leaderboards_2003.rds")
  unlink(cache_file)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "filesystem")
  on.exit({
    options(old)
    unlink(file.path(get_cache_dir(), "releases"), recursive = TRUE)
  }, add = TRUE)

  d1 <- release_load("leaderboards", "leaderboards_2003.rds")
  expect_true(file.exists(cache_file))

  # Remove the source: the on-disk cache should still answer
  unlink(file.path(base, "leaderboards", "leaderboards_2003.rds"))
  d2 <- release_load("leaderboards", "leaderboards_2003.rds")
  expect_equal(d1, d2)

  # Backdate the cached file past the 24h window: now it must miss
  Sys.setFileTime(cache_file, Sys.time() - 25 * 3600)
  expect_null(release_load("leaderboards", "leaderboards_2003.rds"))
})
