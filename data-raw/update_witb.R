## What's in the bag tables for load_witb() and load_witb_switches()
##
##   Rscript --vanilla data-raw/update_witb.R [--upload]
##
## Reads the export from the witbdb repo (scripts/export_golfastr.py), which
## parses GolfWRX and PGA Tour Club Tracker bag posts into one row per club.
## WITBDB_EXPORT overrides the export folder.

export_dir <- Sys.getenv("WITBDB_EXPORT", "~/Code/witbdb/data/export")

as_flag <- function(x) as.logical(as.integer(x))

clubs <- tibble::as_tibble(arrow::read_parquet(file.path(export_dir, "witb_clubs.parquet")))
clubs$as_of_date <- as.Date(clubs$as_of_date)
clubs$club_order <- as.integer(clubs$club_order)
clubs$is_prototype <- as_flag(clubs$is_prototype)
num <- c("loft_stated", "loft_actual", "lie", "bounce", "shaft_weight_g", "tipping_in",
         "length_in", "putter_length_in", "putter_loft", "putter_lie")
clubs[num] <- lapply(clubs[num], as.numeric)

switches <- tibble::as_tibble(arrow::read_parquet(file.path(export_dir, "witb_switches.parquet")))
switches$last_seen_old <- as.Date(switches$last_seen_old)
switches$first_seen_new <- as.Date(switches$first_seen_new)
switches$gap_days <- as.integer(switches$gap_days)
switches$uncertain <- as_flag(switches$uncertain)
switches$brand_change <- as_flag(switches$brand_change)

message(nrow(clubs), " clubs in ", length(unique(clubs$bag_id)), " bags, ",
        format(min(clubs$as_of_date)), " to ", format(max(clubs$as_of_date)),
        "; ", nrow(switches), " switches")

if ("--upload" %in% commandArgs(trailingOnly = TRUE)) {
  out <- file.path(tempdir(), "witb")
  dir.create(out, showWarnings = FALSE)
  for (name in c("witb_clubs", "witb_switches")) {
    data <- get(sub("witb_", "", name))
    base <- file.path(out, name)
    saveRDS(data, paste0(base, ".rds"))
    utils::write.csv(data, paste0(base, ".csv"), row.names = FALSE)
    arrow::write_parquet(data, paste0(base, ".parquet"))
  }
  repo <- "array-carpenter/golfastr"
  if (!"witb" %in% piggyback::pb_releases(repo)$tag_name) {
    piggyback::pb_release_create(repo, "witb", name = "What's in the bag",
                                 body = "Player equipment by bag and club, and equipment switches (load_witb()).")
    # a new tag takes about a minute to show up for uploads and downloads
    Sys.sleep(60)
  }
  options(piggyback_cache_duration = 0)
  for (f in list.files(out, full.names = TRUE)) {
    piggyback::pb_upload(f, repo = repo, tag = "witb", overwrite = TRUE)
  }
}
