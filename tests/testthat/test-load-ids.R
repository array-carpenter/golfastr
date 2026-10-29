ids_release <- function() {
  base <- tempfile("releases")
  dir.create(file.path(base, "ids"), recursive = TRUE)
  saveRDS(tibble::tibble(player_name = c("Scottie Scheffler", "Rory McIlroy"),
                         espn_player_id = c("9478", "3470"), pga_player_id = c("46046", "28237"),
                         first_season = 2012L, last_season = 2026L, events_matched = 100L),
          file.path(base, "ids", "player_ids.rds"))
  saveRDS(tibble::tibble(espn_tournament_id = c("1", "2"), pga_tournament_id = c("R2025016", "R2026016"),
                         year = c(2025L, 2026L), tournament_name = "The Sentry",
                         pga_tournament_name = "The Sentry", start_date = Sys.Date(),
                         field_overlap = 1),
          file.path(base, "ids", "event_ids.rds"))
  base
}

test_that("player and event ids load and filter", {
  old <- options(golfastr.release_base = paste0("file://", ids_release()), golfastr.cache = "off")
  on.exit(options(old), add = TRUE)
  expect_equal(nrow(load_player_ids()), 2)
  expect_equal(load_player_ids("scheff")$pga_player_id, "46046")
  expect_equal(load_event_ids(2026)$pga_tournament_id, "R2026016")
})

test_that("a failed download gives an empty tibble and a message", {
  old <- options(golfastr.release_base = paste0("file://", tempfile()), golfastr.cache = "off")
  on.exit(options(old), add = TRUE)
  expect_message(out <- load_player_ids(), "could not be downloaded")
  expect_equal(nrow(out), 0)
})
