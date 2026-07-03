# Builds a fake release layout on disk so hosted-data paths can be tested
# offline. Point golfastr.release_base at the returned path with file://.
make_release_dir <- function(year) {
  base <- tempfile("releases")
  dir.create(file.path(base, "leaderboards"), recursive = TRUE)
  dir.create(file.path(base, "holes"), recursive = TRUE)

  lb <- tibble::tibble(
    position = c(1L, 2L, 1L, 2L),
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = c("Player One", "Player Two", "Player Three", "Player Four"),
    total_score = c(270L, 272L, 275L, 279L),
    score_to_par = c("-18", "-16", "-13", "-9"),
    tournament_id = c("100", "100", "200", "200"),
    tournament_name = c("Fake Masters", "Fake Masters",
                        "Fake Sony Open", "Fake Sony Open"),
    year = as.integer(year)
  )

  holes <- tibble::tibble(
    player_id = rep(c("p1", "p2"), each = 2),
    player_name = rep(c("Player One", "Player Two"), each = 2),
    position = rep(c(1L, 2L), each = 2),
    tournament_id = "100",
    tournament_name = "Fake Masters",
    round = 1L,
    hole = rep(1:2, 2),
    par = 4L,
    score = c(3L, 4L, 5L, 4L),
    score_type = c("BIRDIE", "PAR", "BOGEY", "PAR"),
    year = as.integer(year)
  )

  saveRDS(lb, file.path(base, "leaderboards",
                        sprintf("leaderboards_%d.rds", year)))
  saveRDS(holes, file.path(base, "holes", sprintf("holes_%d.rds", year)))
  base
}
