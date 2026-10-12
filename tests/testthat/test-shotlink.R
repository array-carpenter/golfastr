# ShotLink loaders against fixture releases - no network needed.

make_shotlink_dir <- function(year) {
  base <- tempfile("releases")
  for (tag in c("shots", "tee_times", "course_setup")) {
    dir.create(file.path(base, tag), recursive = TRUE)
  }

  shots <- tibble::tibble(
    tournament_id = c("R1", "R1", "R2"),
    tournament_name = c("Fake Players", "Fake Players", "Fake Sentry"),
    player_id = c("1", "2", "1"),
    player_name = c("Scottie Scheffler", "Rory McIlroy", "Scottie Scheffler"),
    round = 1L, hole = 1L, stroke = 1L,
    distance = c(300, 290, 310)
  )
  tee_times <- tibble::tibble(
    tournament_id = c("R1", "R1"),
    tournament_name = "Fake Players",
    round = 1L, group = 1L,
    player_id = c("1", "2"),
    player_name = c("Scottie Scheffler", "Rory McIlroy")
  )
  setup <- tibble::tibble(
    tournament_id = c("R1", "R2"),
    tournament_name = c("Fake Players", "Fake Sentry"),
    round = 1L, hole = 1L, par = 4L, yardage = c(423L, 520L)
  )

  saveRDS(shots, file.path(base, "shots", sprintf("shots_%d.rds", year)))
  saveRDS(tee_times, file.path(base, "tee_times", sprintf("tee_times_%d.rds", year)))
  saveRDS(setup, file.path(base, "course_setup", sprintf("course_setup_%d.rds", year)))
  base
}

with_shotlink <- function(code) {
  base <- make_shotlink_dir(2004)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "off")
  on.exit(options(old), add = TRUE)
  force(code)
}

test_that("load_shots returns the season and filters by tournament and player", {
  with_shotlink({
    expect_equal(nrow(load_shots(2004)), 3)
    expect_equal(nrow(load_shots(2004, "R1")), 2)
    expect_equal(nrow(load_shots(2004, "sentry")), 1)
    expect_equal(load_shots(2004, "players", player = "rory")$distance, 290)
  })
})

test_that("load_tee_times and load_course_setup read their releases", {
  with_shotlink({
    expect_equal(nrow(load_tee_times(2004, "Fake Players")), 2)
    expect_equal(load_course_setup(2004, "R2")$yardage, 520L)
  })
})

test_that("shotlink loaders give clear errors", {
  with_shotlink({
    expect_error(load_shots(2004, "Masters"), "No tournament")
    expect_error(load_shots(2004, player = "Tiger"), "No player")
    expect_error(load_shots(1999), "No shots data")
  })
})
