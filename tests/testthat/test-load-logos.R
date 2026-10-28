logos_release <- function() {
  base <- tempfile("releases")
  dir.create(file.path(base, "logos"), recursive = TRUE)
  saveRDS(tibble::tibble(
    logo_type = c("tour", "tournament", "course", "club"),
    name = c("PGA Tour", "Masters Tournament", "Augusta National Golf Club", "Augusta National Golf Club"),
    logo_url = paste0("https://example.com/", 1:4, ".png"),
    logo_source = c("wikimedia", "pgatour", "wikimedia", "wikimedia"),
    source_page = "https://example.com",
    tour = c("pga", "pga", NA, NA),
    pga_tournament_id = c(NA, "R2026014", NA, NA),
    espn_tournament_id = c(NA, "401811941", NA, NA),
    course_id = c(NA, NA, "014", NA),
    club_id = c(NA, NA, "augusta-national", "augusta-national"),
    primary_color = NA_character_, secondary_color = NA_character_,
    checked_at = Sys.Date()
  ), file.path(base, "logos", "logos.rds"))
  base
}

test_that("logos load and filter", {
  old <- options(golfastr.release_base = paste0("file://", logos_release()), golfastr.cache = "off")
  on.exit(options(old), add = TRUE)
  expect_equal(nrow(load_logos()), 4)
  expect_equal(load_logos("course")$course_id, "014")
  expect_equal(nrow(load_logos(name = "augusta")), 2)
  expect_equal(load_logos(c("tour", "club"), "pga")$logo_type, "tour")
  expect_true(all(names(load_logos()) %in% pga_field_descriptions("logos")$field))
})

test_that("a failed logo download gives an empty tibble and a message", {
  old <- options(golfastr.release_base = paste0("file://", tempfile()), golfastr.cache = "off")
  on.exit(options(old), add = TRUE)
  expect_message(out <- load_logos(), "could not be downloaded")
  expect_equal(nrow(out), 0)
})

test_that("hosted logos have a URL and a known type", {
  skip_on_cran()
  skip_if_offline()
  logos <- load_logos()
  skip_if(nrow(logos) == 0)
  expect_true(all(logos$logo_type %in% c("tour", "tournament", "course", "club")))
  expect_false(anyNA(logos$logo_url))
  expect_gt(nrow(load_logos("club", "Augusta National")), 0)
})
