## Schedules for past seasons from ESPN's annual scoreboard responses
`%||%` <- function(a, b) if (is.null(a)) b else a
##
##   Rscript --vanilla data-raw/build_schedule_history.R 2004 ... [--upload]

args <- commandArgs(trailingOnly = TRUE)
upload <- "--upload" %in% args
years <- as.integer(args[args != "--upload"])
raw_dir <- path.expand(Sys.getenv("SCOREBOARD_DIR", "~/Code/golf-wp-planning/data/raw"))
out_dir <- file.path(tempdir(), "schedules")
dir.create(out_dir, showWarnings = FALSE)

for (year in years) {
  sb <- jsonlite::fromJSON(file.path(raw_dir, sprintf("scoreboard_%d.json", year)), simplifyVector = FALSE)
  field <- function(name) vapply(sb$events, function(e) as.character(e[[name]] %||% NA_character_), "")
  schedule <- tibble::tibble(
    event_id = field("id"),
    tournament_name = field("name"),
    start_date = as.Date(substr(field("date"), 1, 10)),
    end_date = as.Date(substr(field("endDate"), 1, 10))
  )
  schedule <- schedule[!is.na(schedule$event_id), ]
  schedule <- schedule[order(schedule$start_date), ]
  message(year, ": ", nrow(schedule), " events")
  base <- file.path(out_dir, sprintf("schedule_%d", year))
  saveRDS(schedule, paste0(base, ".rds"))
  utils::write.csv(schedule, paste0(base, ".csv"), row.names = FALSE)
  arrow::write_parquet(schedule, paste0(base, ".parquet"))
  if (upload) {
    for (ext in c(".rds", ".csv", ".parquet")) {
      piggyback::pb_upload(paste0(base, ext), repo = "array-carpenter/golfastr", tag = "schedules", overwrite = TRUE)
    }
  }
}
