# Strokes gained from ShotLink strokes (prototype)
#
# Baseline: expected strokes to hole out by lie and distance, fit from every
# stroke in the data. A shot's strokes gained is the expected strokes before
# it, minus expected strokes after it, minus the strokes it cost (1, plus any
# penalty strokes before the next shot).

library(dplyr)

lie_group <- function(code) {
  dplyr::case_when(
    code == "OTB" ~ "tee",
    code == "OFW" ~ "fairway",
    code %in% c("ORO", "OIR") ~ "rough",
    code %in% c("OGS", "OST") ~ "sand",
    code == "OGR" ~ "green",
    TRUE ~ "recovery"
  )
}

# One row per stroke actually hit, with its start state and how many
# strokes were still needed to hole out from there.
shot_states <- function(shots) {
  shots |>
    arrange(tournament_id, player_id, round, hole, stroke) |>
    group_by(tournament_id, player_id, round, hole) |>
    filter(any(final_stroke)) |>
    mutate(
      counted = stroke_type %in% c("STROKE", "PENALTY"),
      used_before = cumsum(counted) - counted,
      to_hole_out = hole_score - used_before,
      prev_remaining = dplyr::lag(distance_remaining),
      start_dist = dplyr::coalesce(prev_remaining, as.numeric(yardage))
    ) |>
    filter(stroke_type == "STROKE") |>
    mutate(
      start_dist = ifelse(dplyr::row_number() == 1, as.numeric(yardage), start_dist),
      lie = lie_group(from_code),
      # strokes this shot cost: itself plus penalties before the next swing
      cost = dplyr::lead(used_before, default = dplyr::first(hole_score)) - used_before,
      next_lie = dplyr::lead(lie),
      next_dist = dplyr::lead(start_dist),
      holed = dplyr::row_number() == dplyr::n()
    ) |>
    ungroup() |>
    filter(!is.na(start_dist), start_dist > 0)
}

# Monotone expected-strokes curve per lie: loess on log distance, then
# isotonic so a longer shot never looks easier.
fit_baseline <- function(states) {
  grid <- exp(seq(log(0.1), log(700), length.out = 400))
  curves <- lapply(split(states, states$lie), function(d) {
    d <- d[d$to_hole_out <= 10, ]
    fit <- stats::loess(to_hole_out ~ log(start_dist), data = d, span = 0.3,
                        control = stats::loess.control(surface = "direct"))
    in_range <- grid >= min(d$start_dist) & grid <= max(d$start_dist)
    y <- stats::predict(fit, data.frame(start_dist = grid[in_range]))
    y <- stats::isoreg(grid[in_range], y)$yf
    list(x = grid[in_range], y = y)
  })
  function(lie, dist) {
    out <- rep(NA_real_, length(lie))
    for (l in names(curves)) {
      i <- which(lie == l)
      if (length(i)) out[i] <- stats::approx(curves[[l]]$x, curves[[l]]$y, dist[i], rule = 2)$y
    }
    out
  }
}

strokes_gained <- function(shots) {
  states <- shot_states(shots)
  expected <- fit_baseline(states)
  states |>
    mutate(
      e_start = expected(lie, start_dist),
      e_end = ifelse(holed, 0, expected(next_lie, next_dist)),
      sg = e_start - e_end - cost,
      category = dplyr::case_when(
        lie == "green" ~ "putt",
        lie == "tee" & par >= 4 ~ "off_tee",
        start_dist <= 30 ~ "around_green",
        TRUE ~ "approach"
      )
    ) |>
    structure(expected = expected)
}

if (sys.nframe() == 0) {
  scratch <- commandArgs(trailingOnly = TRUE)
  shots <- dplyr::bind_rows(lapply(scratch, readRDS))
  sg <- strokes_gained(shots)
  expected <- attr(sg, "expected")

  cat("\nBaseline vs Broadie's PGA Tour table\n")
  ref <- tibble::tribble(
    ~lie,      ~dist,   ~broadie,
    "green",   3 / 3,   1.04,
    "green",   10 / 3,  1.61,
    "green",   30 / 3,  1.98,
    "green",   60 / 3,  2.21,
    "fairway", 100,     2.80,
    "fairway", 150,     2.98,
    "fairway", 200,     3.19,
    "rough",   150,     3.23,
    "sand",    20,      2.53,
    "tee",     400,     3.99,
    "tee",     450,     4.08
  )
  ref$ours <- round(expected(ref$lie, ref$dist), 2)
  print(ref)

  rounds <- sg |>
    group_by(tournament_id, round, player_id, player_name) |>
    summarise(sg_total = sum(sg), score = sum(cost), holes = n_distinct(hole), .groups = "drop")
  by_cat <- sg |>
    group_by(tournament_id, round, player_id, category) |>
    summarise(sg = sum(sg), .groups = "drop") |>
    tidyr::pivot_wider(names_from = category, values_from = sg, values_fill = 0)
  rounds <- rounds |>
    left_join(by_cat, by = c("tournament_id", "round", "player_id")) |>
    filter(holes == 18) |>
    group_by(tournament_id, round) |>
    mutate(field_avg = mean(score), vs_field = field_avg - score,
           sg_field = sg_total - mean(sg_total)) |>
    ungroup()

  cat("\nCategories add up to the total:",
      isTRUE(all.equal(rounds$sg_total,
                       rounds$off_tee + rounds$approach + rounds$around_green + rounds$putt)), "\n")
  cat("Round SG (field-adjusted) vs strokes better than field average: r =",
      round(cor(rounds$sg_field, rounds$vs_field), 4),
      " max gap =", round(max(abs(rounds$sg_field - rounds$vs_field)), 3), "\n")

  cat("\nTop 10 by total SG per round\n")
  rounds |>
    group_by(tournament_id, player_name) |>
    summarise(rounds = n(), across(c(off_tee, approach, around_green, putt, sg_field), mean),
              .groups = "drop") |>
    arrange(desc(sg_field)) |>
    mutate(across(where(is.numeric) & !rounds, ~ round(.x, 2))) |>
    head(10) |>
    print()
}
