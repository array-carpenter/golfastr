## Shared by the win probability build scripts and the daily update.

release_url <- function(tag, file) {
  sprintf("https://github.com/array-carpenter/golfastr/releases/download/%s/%s", tag, file)
}

read_release <- function(tag, file, local_dir = NULL) {
  if (!is.null(local_dir)) {
    f <- file.path(local_dir, file)
    if (file.exists(f)) return(readRDS(f))
  }
  tryCatch(readRDS(url(release_url(tag, file))), error = function(e) NULL)
}

# All hole files with a playoff flag. Playoff holes come through as an extra
# round with a handful of players; a round is regulation when at least a
# quarter of the opening field (and at least 10 players) played it.
read_hole_history <- function(years, local_dir = NULL) {
  holes <- dplyr::bind_rows(lapply(years, function(y) read_release("holes", sprintf("holes_%d.rds", y), local_dir)))
  holes <- holes[holes$hole <= 18, ]
  per_round <- holes |>
    dplyr::group_by(tournament_id, round) |>
    dplyr::summarise(n = dplyr::n_distinct(player_id), .groups = "drop") |>
    dplyr::group_by(tournament_id) |>
    dplyr::mutate(regulation = n >= pmax(10, 0.25 * n[round == 1])) |>
    dplyr::ungroup()
  reg <- per_round[per_round$regulation, c("tournament_id", "round")]
  holes$playoff <- !paste(holes$tournament_id, holes$round) %in% paste(reg$tournament_id, reg$round)
  holes
}

read_schedules <- function(years) {
  s <- dplyr::bind_rows(lapply(years, function(y) read_release("schedules", sprintf("schedule_%d.rds", y))))
  dplyr::transmute(s, tournament_id = as.character(event_id), start_date = as.Date(start_date))
}

# A cut after round r: at least 5 players who finished round r did not start
# round r + 1, and they all sat at or below the worst place that went on
# (a couple of withdrawals above the line are allowed). The rule ("top N and
# ties") lies between the place of the last player through and the number
# through; a standard N for the era is used when one fits.
find_cuts <- function(rt, n_rounds, year) {
  out <- list()
  for (r in seq_len(n_rounds - 1)) {
    done <- rt[rt$round == r, ]
    nxt <- unique(rt$player_id[rt$round == r + 1])
    if (!nrow(done)) next
    done$place <- rank(done$cum, ties.method = "min")
    on <- done$player_id %in% nxt
    if (sum(!on) < 5) next
    place <- max(done$place[on])
    if (sum(!on & done$place <= place) > 3) next
    made <- sum(on)
    std <- if (year >= 2019) c(65, 70, 50, 60, 44, 78, 40, 36) else c(70, 65, 50, 60, 44, 78, 40, 36)
    std <- std[std >= place & std <= made]
    rule <- if (length(std)) std[1] else made
    out[[length(out) + 1]] <- data.frame(after_round = r, place = place, made = made, rule = rule)
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(after_round = integer(), place = integer(), made = integer(), rule = integer())
}

event_table <- function(holes, dates) {
  round_totals <- holes[!holes$playoff, ] |>
    dplyr::group_by(tournament_id, player_id, round) |>
    dplyr::summarise(holes = dplyr::n(), strokes = sum(score), .groups = "drop") |>
    dplyr::filter(holes == 18) |>
    dplyr::arrange(tournament_id, player_id, round) |>
    dplyr::group_by(tournament_id, player_id) |>
    dplyr::mutate(cum = cumsum(strokes), complete = round == seq_along(round)) |>
    dplyr::ungroup() |>
    dplyr::filter(complete)

  events <- holes |>
    dplyr::group_by(tournament_id) |>
    dplyr::summarise(year = dplyr::first(year), tournament_name = dplyr::first(tournament_name),
                     players = dplyr::n_distinct(player_id), rounds = max(round[!playoff]),
                     playoff = any(playoff),
                     winner_id = dplyr::first(player_id[position == 1]),
                     stableford = any(!par %in% 3:6) || grepl("Barracuda", dplyr::first(tournament_name)),
                     .groups = "drop") |>
    dplyr::left_join(dates, by = "tournament_id")

  event_year <- stats::setNames(events$year, events$tournament_id)
  cuts <- lapply(split(round_totals, round_totals$tournament_id), function(rt) {
    find_cuts(rt, max(rt$round), event_year[[rt$tournament_id[1]]])
  })
  empty <- data.frame(after_round = integer(), place = integer(), made = integer(), rule = integer())
  events$cuts <- cuts[events$tournament_id]
  events$cuts[vapply(events$cuts, is.null, TRUE)] <- list(empty)

  team <- grepl("Zurich", events$tournament_name) & events$year >= 2017
  other <- grepl("Match Play|Presidents Cup|Ryder Cup|Qualifying|Q-School", events$tournament_name)
  events$starting_strokes <- grepl("TOUR Championship", events$tournament_name, ignore.case = TRUE) &
    events$year %in% 2019:2024
  events$wp_eligible <- !events$stableford & !team & !other & !is.na(events$winner_id) &
    events$players >= 10 & events$rounds >= 3
  events
}

# Pre-event ratings with round values adjusted for field strength (the
# field's mean rating), solved by a few passes.
rate_events <- function(rounds, events, half_life, prior_mean, prior_n, passes = 3) {
  r <- rounds
  r$sg_adj <- r$sg
  for (p in seq_len(passes)) {
    x <- r
    x$sg <- x$sg_adj
    ratings <- golfastr:::wp_skill_ratings(x, events, half_life, prior_mean, prior_n)
    strength <- tapply(ratings$rating, ratings$tournament_id, mean)
    r$sg_adj <- r$sg + strength[r$tournament_id]
  }
  list(ratings = ratings, strength = strength)
}
