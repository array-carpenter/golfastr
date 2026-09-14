sample_leaderboard <- function() {
  tibble::tibble(
    position = c(1L, 2L, 3L, 1L, 2L),
    player_id = c("1", "2", "3", "1", "2"),
    player_name = c("Scottie Scheffler", "Rory McIlroy", "Xander Schauffele",
                    "Scottie Scheffler", "Rory McIlroy"),
    total_score = c(270L, 272L, 274L, 268L, 271L),
    score_to_par = c("-18", "-16", "-14", "-20", "-17"),
    tournament_id = c("100", "100", "100", "200", "200"),
    tournament_name = c("Fake Masters", "Fake Masters", "Fake Masters",
                        "Fake Open", "Fake Open"),
    year = c(2025L, 2025L, 2025L, 2025L, 2025L)
  )
}

test_that("analysis functions work on load_leaderboard-style columns", {
  lb <- sample_leaderboard()

  summary <- player_summary("Scheffler", data = lb)
  expect_equal(summary$player, "Scottie Scheffler")
  expect_equal(summary$tournaments, 2)
  expect_equal(summary$wins, 2)

  winners <- win_leaders(data = lb)
  expect_equal(as.character(winners$player[1]), "Scottie Scheffler")
  expect_equal(winners$wins[1], 2)

  snap <- leaderboard("Masters", 2025, data = lb)
  expect_equal(names(snap), c("Pos", "Player", "Score", "Total"))
  expect_equal(snap$Player[1], "Scottie Scheffler")
})

test_that("analysis functions still work on older saved-file columns", {
  old <- tibble::tibble(
    position = 1L,
    player_id = "1",
    display_name = "Scottie Scheffler",
    total_score = 270L,
    score_display = "-18",
    event_id = "100",
    tournament_name = "Fake Masters",
    year = 2025L
  )

  summary <- player_summary("Scheffler", data = old)
  expect_equal(summary$wins, 1)
  expect_equal(get_player("Scheffler", data = old)$display_name,
               "Scottie Scheffler")
})

test_that("analysis requires data or file_path", {
  expect_error(player_summary("Scheffler"), "data")
})

test_that("load_strokes_gained can filter by year using bundled data", {
  sg <- load_strokes_gained(use_bundled = TRUE, year = 2026)
  expect_true(all(sg$season == 2026))
  expect_gt(nrow(sg), 0)

  scheffler <- load_strokes_gained("Scheffler", use_bundled = TRUE, year = 2026)
  expect_true(any(grepl("Scheffler", scheffler$player_name)))
})
