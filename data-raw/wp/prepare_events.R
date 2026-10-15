## Stack every season's hole file and describe each event for the win
## probability model: rounds played, cuts, winner and whether the event is
## plain individual stroke play.
##
##   Rscript --vanilla data-raw/wp/prepare_events.R
##
## Writes data-raw/wp/cache/holes_all.rds and data-raw/wp/cache/events.rds

suppressMessages(library(dplyr))

source("data-raw/wp/helpers.R")
local_dir <- path.expand(Sys.getenv("HOLE_HISTORY_OUT", "~/Code/golf-wp-planning/data/hosted_holes"))
cache <- "data-raw/wp/cache"
dir.create(cache, recursive = TRUE, showWarnings = FALSE)

years <- 2004:as.integer(format(Sys.Date(), "%Y"))
holes <- read_hole_history(years, local_dir)
message(nrow(holes), " hole rows")
events <- event_table(holes, read_schedules(years))

message(nrow(events), " events, ", sum(events$wp_eligible), " eligible for win probability")
print(table(events$year, events$wp_eligible))
cut_desc <- vapply(events$cuts, function(c) if (nrow(c)) paste0("R", c$after_round, ":", c$rule, collapse = " ") else "none", "")
print(table(cut_desc[events$wp_eligible]))
print(events[!events$wp_eligible, c("year", "tournament_name", "players", "rounds")], n = 60)

saveRDS(holes, file.path(cache, "holes_all.rds"))
saveRDS(events, file.path(cache, "events.rds"))
