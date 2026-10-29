db_release <- function(seasons = 2004:2005) {
  base <- tempfile("releases")
  for (tag in c("schedules", "holes", "ids", "skill_ratings")) dir.create(file.path(base, tag), recursive = TRUE)
  for (s in seasons) {
    saveRDS(tibble::tibble(event_id = paste0(s, c("01", "02")), tournament_name = c("A", "B"),
                           start_date = as.Date(sprintf("%d-01-0%d", s, 1:2)),
                           end_date = as.Date(sprintf("%d-01-0%d", s, 4:5))),
            file.path(base, "schedules", sprintf("schedule_%d.rds", s)))
    saveRDS(tibble::tibble(player_id = c(1L, 2L), player_name = c("One", "Two"), tournament_id = paste0(s, "01"),
                           round = 1L, hole = 1L, par = 4L, score = c(3L, 4L), year = s, win_prob = c(0.6, 0.4)),
            file.path(base, "holes", sprintf("holes_%d.rds", s)))
  }
  saveRDS(tibble::tibble(player_name = "One", espn_player_id = "1", pga_player_id = "P1",
                         first_season = 2012L, last_season = 2026L, events_matched = 9L),
          file.path(base, "ids", "player_ids.rds"))
  saveRDS(tibble::tibble(tournament_id = "200401", player_id = "1", player_name = "One", rating = 1.2, n_rounds = 40L),
          file.path(base, "skill_ratings", "skill_ratings.rds"))
  base
}

with_releases <- function(base, code) {
  old <- options(golfastr.release_base = paste0("file://", base), golfastr.cache = "off")
  on.exit(options(old))
  force(code)
}

test_that("update_db builds tables with a season column and text dates", {
  skip_if_not_installed("RSQLite")
  base <- db_release()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  with_releases(base, suppressMessages(update_db(tables = c("events", "holes", "player_ids"), db_connection = con)))
  ev <- DBI::dbReadTable(con, "events")
  expect_setequal(ev$season, 2004:2005)
  expect_true("tournament_id" %in% names(ev))
  expect_type(ev$start_date, "character")
  holes <- DBI::dbReadTable(con, "holes")
  expect_type(holes$player_id, "character")
  expect_equal(nrow(holes), 4)
  expect_equal(DBI::dbReadTable(con, "player_ids")$pga_player_id, "P1")
})

test_that("update_db adds new seasons and refreshes the latest one without duplicates", {
  skip_if_not_installed("RSQLite")
  base <- db_release(2004:2005)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  with_releases(base, suppressMessages(update_db(tables = "holes", db_connection = con)))
  saveRDS(tibble::tibble(player_id = 1L, player_name = "One", tournament_id = "200601", round = 1L, hole = 1L,
                         par = 4L, score = 4L, year = 2006L, win_prob = 1),
          file.path(base, "holes", "holes_2006.rds"))
  with_releases(base, suppressMessages(update_db(tables = "holes", db_connection = con)))
  counts <- DBI::dbGetQuery(con, "SELECT season, COUNT(*) AS n FROM holes GROUP BY season")
  expect_equal(counts$n[order(counts$season)], c(2, 2, 1))
  with_releases(base, suppressMessages(update_db(tables = "holes", db_connection = con, force_rebuild = TRUE)))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM holes")$n, 5)
})

test_that("update_db writes an SQLite file and rejects unknown tables", {
  skip_if_not_installed("RSQLite")
  base <- db_release(2004)
  dir <- tempfile("db")
  with_releases(base, suppressMessages(update_db(dbdir = dir, tables = "skill_ratings")))
  expect_true(file.exists(file.path(dir, "golfastr_db")))
  expect_error(update_db(tables = "nope"), "Unknown table")
})
