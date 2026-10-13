# Compare ShotLink-based strokes gained with the PGA Tour's official season averages.
#
#   Rscript --vanilla scripts/strokes_gained_validate.R 2026

source("scripts/strokes_gained_prototype.R")

season <- as.integer(commandArgs(trailingOnly = TRUE)[1])
base <- "https://github.com/array-carpenter/golfastr/releases/download"
shots <- readRDS(url(sprintf("%s/shots/shots_%d.rds", base, season)))
official <- readRDS(url(sprintf("%s/strokes_gained/strokes_gained.rds", base)))
official <- official[official$season == season, ]

sg <- strokes_gained(shots)

# Field adjustment: measure each category against the field playing the same
# course in the same round, so the four categories add up to the total.
complete <- sg |>
  group_by(tournament_id, round, player_id) |>
  summarise(holes = n_distinct(hole), .groups = "drop") |>
  filter(holes == 18)

rounds <- sg |>
  group_by(tournament_id, round, course_id, player_id, player_name, category) |>
  summarise(sg = sum(sg), .groups = "drop") |>
  tidyr::pivot_wider(names_from = category, values_from = sg, values_fill = 0) |>
  semi_join(complete, by = c("tournament_id", "round", "player_id")) |>
  group_by(tournament_id, round, course_id) |>
  mutate(across(c(off_tee, approach, around_green, putt), ~ .x - mean(.x))) |>
  ungroup() |>
  mutate(total = off_tee + approach + around_green + putt)

ours <- rounds |>
  group_by(player_id, player_name) |>
  summarise(rounds = n(), sg_ott = mean(off_tee), sg_app = mean(approach),
            sg_arg = mean(around_green), sg_putt = mean(putt), sg_total = mean(total),
            .groups = "drop")

cmp <- inner_join(ours, official, by = "player_id", suffix = c("", "_pga"))
cat(season, ": ", nrow(ours), " players with ShotLink rounds, ", nrow(cmp),
    " also in the official table\n\n", sep = "")

stats_tbl <- lapply(c("sg_ott", "sg_app", "sg_arg", "sg_putt", "sg_total"), function(v) {
  a <- cmp[[v]]; b <- cmp[[paste0(v, "_pga")]]
  data.frame(category = v, r = round(cor(a, b), 3), mean_abs_diff = round(mean(abs(a - b)), 3),
             slope = round(unname(coef(lm(b ~ a))[2]), 2))
})
print(do.call(rbind, stats_tbl), row.names = FALSE)

cat("\nTop 10 by our total, with the official number\n")
cmp |>
  arrange(desc(sg_total)) |>
  transmute(player_name, rounds, rounds_pga, sg_total = round(sg_total, 2),
            sg_total_pga, sg_app = round(sg_app, 2), sg_app_pga, sg_putt = round(sg_putt, 2),
            sg_putt_pga) |>
  head(10) |>
  print()

saveRDS(list(rounds = rounds, players = ours, comparison = cmp),
        sprintf("scripts/sg_validation_%d.rds", season))
