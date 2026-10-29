hole_shots <- function(player = "1", hole = 1L, codes = c("OTB", "OFW", "OGR", "OGR"),
                       remaining = c(150, 10, 1, 0), types = rep("STROKE", length(codes)),
                       par = 4L, yardage = 420L) {
  n <- length(codes)
  tibble::tibble(
    tournament_id = "R1", player_id = player, player_name = paste("Player", player),
    round = 1L, course_id = "C1", hole = hole, par = par, yardage = yardage,
    hole_score = sum(types %in% c("STROKE", "PENALTY")), stroke = seq_len(n),
    stroke_type = types, from_code = codes, distance_remaining = remaining,
    final_stroke = seq_len(n) == n
  )
}

test_that("a simple par 4 is scored shot by shot", {
  sg <- calculate_strokes_gained(hole_shots())
  expect_equal(sg$start_lie, c("tee", "fairway", "green", "green"))
  expect_equal(sg$start_distance, c(420, 150, 10, 1))
  expect_equal(sg$sg_category, c("off_tee", "approach", "putt", "putt"))
  expect_equal(sg$strokes_used, rep(1L, 4))
  expect_equal(sg$expected_after[4], 0)
  # the shots add up to the baseline for the hole minus the score
  expect_equal(sum(sg$sg), sg$expected_before[1] - 4)
})

test_that("penalty strokes are charged to the shot that caused them", {
  shots <- hole_shots(
    codes = c("OTB", "OWA", "OTH", "OFW", "OGR"),
    remaining = c(160, 160, 155, 3, 0),
    types = c("STROKE", "PENALTY", "DROP", "STROKE", "STROKE")
  )
  sg <- calculate_strokes_gained(shots)
  expect_equal(nrow(sg), 3)
  expect_equal(sg$strokes_used, c(2L, 1L, 1L))
  expect_equal(sg$start_distance[2], 155)
  expect_equal(sg$sg[1], sg$expected_before[1] - sg$expected_after[1] - 2)
})

test_that("unfinished holes are dropped and missing columns are reported", {
  shots <- hole_shots()
  shots$final_stroke <- FALSE
  expect_equal(nrow(calculate_strokes_gained(shots)), 0)
  expect_error(calculate_strokes_gained(shots[, -1]), "tournament_id")
})

test_that("round totals equal strokes better than the field", {
  pars <- function(player, putts) do.call(rbind, lapply(1:18, function(h) {
    if (h <= putts) {
      hole_shots(player, h, codes = c("OTB", "OFW", "OGR"), remaining = c(150, 8, 0))
    } else {
      hole_shots(player, h)
    }
  }))
  shots <- rbind(pars("1", 0), pars("2", 3), pars("3", 9))
  rounds <- summarise_strokes_gained(calculate_strokes_gained(shots), by = "round")

  score <- c(72, 69, 63)
  expect_equal(rounds$sg_total, mean(score) - score, tolerance = 1e-8)
  expect_equal(sum(rounds$sg_total), 0, tolerance = 1e-8)
  expect_equal(rounds$sg_ott + rounds$sg_app + rounds$sg_arg + rounds$sg_putt,
               rounds$sg_total)
  expect_type(rounds$sg_total, "double")

  season <- summarise_strokes_gained(calculate_strokes_gained(shots), by = "season")
  expect_equal(season$player_id[1], "3")
  expect_equal(season$rounds, rep(1L, 3))
})

test_that("team events are left out of summaries", {
  holes <- do.call(rbind, lapply(1:18, function(h) hole_shots("1", h)))
  team <- holes
  team$tournament_id <- "R2"
  team$player_name <- "Team A/B"
  expect_message(
    out <- summarise_strokes_gained(calculate_strokes_gained(rbind(holes, team))),
    "team event"
  )
  expect_equal(unique(out$tournament_id), "R1")
})
