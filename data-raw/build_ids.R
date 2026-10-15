## ESPN <-> PGA Tour ID crosswalk, and true hole order from ShotLink tee times
##
##   Rscript --vanilla data-raw/build_ids.R [--upload]
##
## Events: an ESPN event and a ShotLink tournament match when they start
## within 4 days of each other and share most of their field (by normalized
## player name), which also separates opposite-field events in the same week.
## Players: inside matched events, identical normalized names vote for an
## (ESPN id, PGA Tour id) pair. A pair needs 2 or more events, and an id that
## pairs with more than one partner goes to data-raw/id_conflicts.csv instead.

suppressMessages(library(dplyr))

cache <- "data-raw/wp/cache"
shotlink_out <- "data-raw/shotlink-cache/out"
holes <- readRDS(file.path(cache, "holes_all.rds"))
events <- readRDS(file.path(cache, "events.rds"))

norm_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("\\b(jr|sr|ii|iii|iv)\\b\\.?", "", x)
  x <- gsub("[^a-z ]", "", x)
  gsub("\\s+", " ", trimws(x))
}

tee_files <- list.files(shotlink_out, "^tee_times_\\d{4}\\.rds$", full.names = TRUE)
tee <- bind_rows(lapply(tee_files, readRDS))
message(length(tee_files), " ShotLink seasons, ", length(unique(tee$tournament_id)), " tournaments")

pga_events <- tee |>
  group_by(tournament_id) |>
  summarise(pga_name = first(tournament_name),
            start = as.Date(min(tee_time[round == 1], na.rm = TRUE)), .groups = "drop")
pga_fields <- split(norm_name(tee$player_name), tee$tournament_id)
espn_fields <- split(norm_name(holes$player_name), holes$tournament_id)

cand <- cross_join(
  events |> transmute(espn_id = tournament_id, espn_name = tournament_name, year, espn_start = start_date),
  pga_events |> transmute(pga_id = tournament_id, pga_name, pga_start = start)
) |>
  filter(abs(as.numeric(pga_start - espn_start)) <= 4)
cand$overlap <- mapply(function(e, p) {
  a <- unique(espn_fields[[e]])
  b <- unique(pga_fields[[p]])
  length(intersect(a, b)) / min(length(a), length(b))
}, cand$espn_id, cand$pga_id)
event_ids <- cand |>
  filter(overlap >= 0.6) |>
  group_by(espn_id) |>
  slice_max(overlap, n = 1, with_ties = FALSE) |>
  group_by(pga_id) |>
  slice_max(overlap, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(espn_tournament_id = espn_id, pga_tournament_id = pga_id, year,
            tournament_name = espn_name, pga_tournament_name = pga_name,
            start_date = espn_start, field_overlap = round(overlap, 3))
message(nrow(event_ids), " events matched")

# Player votes inside matched events
espn_people <- holes |>
  distinct(tournament_id, player_id, player_name) |>
  mutate(key = norm_name(player_name))
pga_people <- tee |>
  distinct(tournament_id, player_id, player_name) |>
  mutate(key = norm_name(player_name))
votes <- event_ids |>
  select(espn_tournament_id, pga_tournament_id) |>
  inner_join(espn_people, by = c("espn_tournament_id" = "tournament_id"), relationship = "many-to-many") |>
  inner_join(pga_people, by = c("pga_tournament_id" = "tournament_id", "key"),
             suffix = c("_espn", "_pga"), relationship = "many-to-many") |>
  count(espn_id = player_id_espn, pga_id = player_id_pga, name = "events")

pairs <- votes |> filter(events >= 2)
dup_espn <- pairs$espn_id[duplicated(pairs$espn_id)]
dup_pga <- pairs$pga_id[duplicated(pairs$pga_id)]
conflict <- pairs$espn_id %in% dup_espn | pairs$pga_id %in% dup_pga
utils::write.csv(pairs[conflict, ], "data-raw/id_conflicts.csv", row.names = FALSE)

player_names <- espn_people |> group_by(player_id) |> summarise(player_name = last(player_name))
seasons <- holes |>
  filter(tournament_id %in% event_ids$espn_tournament_id) |>
  group_by(player_id) |>
  summarise(first_season = min(year), last_season = max(year), .groups = "drop")
player_ids <- pairs[!conflict, ] |>
  transmute(espn_player_id = espn_id, pga_player_id = pga_id, events_matched = events) |>
  left_join(player_names, by = c("espn_player_id" = "player_id")) |>
  left_join(seasons, by = c("espn_player_id" = "player_id")) |>
  select(player_name, espn_player_id, pga_player_id, first_season, last_season, events_matched) |>
  arrange(player_name)
message(nrow(player_ids), " players matched, ", sum(conflict), " conflicting pairs set aside")

# True play order: players starting on the 10th tee play 10-18 then 1-9
source("data-raw/wp/helpers.R")
hole_order <- hole_order_table(tee, event_ids, player_ids)
order_tbl <- distinct(hole_order, tournament_id, player_id, round, start_tee)
saveRDS(hole_order, file.path(cache, "hole_order.rds"))
saveRDS(wave_table(tee, event_ids, player_ids), file.path(cache, "waves.rds"))
message(nrow(order_tbl), " player-rounds with a known starting tee, ",
        round(100 * mean(order_tbl$start_tee != 1), 1), "% off the 10th")

saveRDS(player_ids, file.path(cache, "player_ids.rds"))
saveRDS(event_ids, file.path(cache, "event_ids.rds"))

if ("--upload" %in% commandArgs(trailingOnly = TRUE)) {
  out <- file.path(tempdir(), "ids")
  dir.create(out, showWarnings = FALSE)
  for (x in list(list(player_ids, "player_ids"), list(event_ids, "event_ids"))) {
    base <- file.path(out, x[[2]])
    saveRDS(x[[1]], paste0(base, ".rds"))
    utils::write.csv(x[[1]], paste0(base, ".csv"), row.names = FALSE)
    arrow::write_parquet(x[[1]], paste0(base, ".parquet"))
  }
  for (f in list.files(out, full.names = TRUE)) {
    piggyback::pb_upload(f, repo = "array-carpenter/golfastr", tag = "ids", overwrite = TRUE)
  }
}
