## Stack every season's hole file and describe each event for the win
## probability model: rounds played, cuts, winner and whether the event is
## plain individual stroke play.
##
##   Rscript --vanilla data-raw/wp/prepare_events.R
##
## Writes data-raw/wp/cache/holes_all.rds and data-raw/wp/cache/events.rds

suppressMessages(library(dplyr))

local_dir <- path.expand(Sys.getenv("HOLE_HISTORY_OUT", "~/Code/golf-wp-planning/data/hosted_holes"))
cache <- "data-raw/wp/cache"
dir.create(cache, recursive = TRUE, showWarnings = FALSE)

read_season <- function(year) {
  f <- file.path(local_dir, sprintf("holes_%d.rds", year))
  if (file.exists(f)) return(readRDS(f))
  readRDS(url(sprintf(
    "https://github.com/array-carpenter/golfastr/releases/download/holes/holes_%d.rds", year)))
}

holes <- bind_rows(lapply(2004:2026, read_season))
holes <- holes[holes$hole <= 18, ]

# Playoff holes come through as an extra round with a handful of players.
# A round is regulation when at least a quarter of the opening field (and
# at least 10 players) played it.
per_round <- holes |>
  group_by(tournament_id, round) |>
  summarise(n = n_distinct(player_id), .groups = "drop") |>
  group_by(tournament_id) |>
  mutate(regulation = n >= pmax(10, 0.25 * n[round == 1])) |>
  ungroup()
reg <- per_round[per_round$regulation, c("tournament_id", "round")]
holes$playoff <- !paste(holes$tournament_id, holes$round) %in% paste(reg$tournament_id, reg$round)
message(nrow(holes), " hole rows")

dates <- arrow::read_parquet("~/Code/golf-wp-planning/data/events.parquet") |>
  transmute(tournament_id = as.character(event_id), start_date = as.Date(substr(event_date, 1, 10)))

# Strokes after each round for players with all 18 holes of it
round_totals <- holes[!holes$playoff, ] |>
  group_by(tournament_id, player_id, round) |>
  summarise(holes = n(), strokes = sum(score), .groups = "drop") |>
  filter(holes == 18) |>
  arrange(tournament_id, player_id, round) |>
  group_by(tournament_id, player_id) |>
  mutate(cum = cumsum(strokes), complete = round == seq_along(round)) |>
  ungroup() |>
  filter(complete)

# A cut after round r: at least 5 players who finished round r did not start
# round r + 1, and they all sat at or below the worst place that went on
# (a couple of withdrawals above the line are allowed).
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
    early <- sum(!on & done$place <= place)
    if (early > 3) next
    # The rule ("top N and ties") lies between the place of the last player
    # through and the number through; use a standard N when one fits.
    made <- sum(on)
    std <- if (year >= 2019) c(65, 70, 50, 60, 44, 78, 40, 36) else c(70, 65, 50, 60, 44, 78, 40, 36)
    std <- std[std >= place & std <= made]
    rule <- if (length(std)) std[1] else made
    out[[length(out) + 1]] <- data.frame(after_round = r, place = place, made = made, rule = rule)
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(after_round = integer(), place = integer(), made = integer(), rule = integer())
}

events <- holes |>
  group_by(tournament_id) |>
  summarise(year = first(year), tournament_name = first(tournament_name),
            players = n_distinct(player_id), rounds = max(round[!playoff]),
            playoff = any(playoff),
            winner_id = first(player_id[position == 1]),
            stableford = any(!par %in% 3:6) || grepl("Barracuda", first(tournament_name)),
            .groups = "drop") |>
  left_join(dates, by = "tournament_id")

event_year <- setNames(events$year, events$tournament_id)
cuts <- lapply(split(round_totals, round_totals$tournament_id), function(rt) {
  find_cuts(rt, max(rt$round), event_year[[rt$tournament_id[1]]])
})
events$cuts <- cuts[events$tournament_id]
events$cuts[vapply(events$cuts, is.null, TRUE)] <- list(data.frame(after_round = integer(), place = integer(), made = integer(), rule = integer()))

team <- grepl("Zurich", events$tournament_name) & events$year >= 2017
match_play <- grepl("Match Play|Presidents Cup|Ryder Cup|Qualifying|Q-School", events$tournament_name)
events$starting_strokes <- grepl("TOUR Championship", events$tournament_name, ignore.case = TRUE) &
  events$year %in% 2019:2024
events$wp_eligible <- !events$stableford & !team & !match_play & !is.na(events$winner_id) &
  events$players >= 10 & events$rounds >= 3

message(nrow(events), " events, ", sum(events$wp_eligible), " eligible for win probability")
print(table(events$year, events$wp_eligible))
cut_desc <- vapply(events$cuts, function(c) if (nrow(c)) paste0("R", c$after_round, ":", c$rule, collapse = " ") else "none", "")
print(table(cut_desc[events$wp_eligible]))
print(events[!events$wp_eligible, c("year", "tournament_name", "players", "rounds")], n = 60)

saveRDS(holes, file.path(cache, "holes_all.rds"))
saveRDS(events, file.path(cache, "events.rds"))
