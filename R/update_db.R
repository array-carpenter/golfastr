#' Build or Update a Local golfastr Database
#'
#' Writes golfastr's hosted data into a database on your computer, so it can
#' be queried with SQL or dplyr. By default this is an SQLite file
#' (\code{golfastr_db} in \code{dbdir}); pass \code{db_connection} to use any
#' other DBI database, such as DuckDB or Postgres.
#'
#' The first call downloads everything requested. Later calls add seasons
#' that are missing and refresh the latest season, so running it once a week
#' keeps the database current. Lookup tables (ratings, IDs, strokes gained)
#' are replaced in full each time.
#'
#' Every season-based table has a \code{season} column. Tables built from
#' ESPN data (\code{holes}, \code{events}, \code{leaderboards},
#' \code{skill_ratings}) use ESPN IDs in
#' \code{tournament_id} and \code{player_id}. ShotLink tables (\code{shots},
#' \code{tee_times}, \code{course_setup}) name theirs
#' \code{pga_tournament_id} and \code{pga_player_id}, and also carry
#' \code{espn_tournament_id} and \code{espn_player_id} where the ID
#' crosswalk has a match, so the two sources join directly. Dates are stored
#' as \code{"YYYY-MM-DD"} text and times as ISO 8601 text in UTC.
#'
#' @param dbdir Folder for the SQLite file. Defaults to
#'   \code{getOption("golfastr.dbdirectory")}, or the working directory.
#' @param dbname File name of the SQLite database.
#' @param tables Tables to build. The default leaves out the ShotLink
#'   tables, which are large (\code{shots} is about a million rows per
#'   season); add \code{"shots"}, \code{"tee_times"} and
#'   \code{"course_setup"} to include them.
#' @param force_rebuild If \code{TRUE}, drop the requested tables and build
#'   them again from scratch.
#' @param db_connection Optional DBI connection to write to instead of the
#'   SQLite file. It is left open.
#' @return The DBI connection, invisibly. When golfastr opened the SQLite
#'   file itself it is closed and \code{NULL} is returned; reopen it with
#'   \code{DBI::dbConnect(RSQLite::SQLite(), file.path(dbdir, dbname))}.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   dir <- tempdir()
#'   update_db(dbdir = dir, tables = c("events", "player_ids"))
#'   con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, "golfastr_db"))
#'   DBI::dbGetQuery(con, "SELECT season, COUNT(*) AS events FROM events GROUP BY season")
#'   DBI::dbDisconnect(con)
#' }
#' }
update_db <- function(dbdir = getOption("golfastr.dbdirectory", default = "."),
                      dbname = "golfastr_db",
                      tables = c("events", "holes", "leaderboards", "strokes_gained",
                                 "skill_ratings", "player_ids", "event_ids"),
                      force_rebuild = FALSE,
                      db_connection = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    cli::cli_abort("{.fn update_db} needs the {.pkg DBI} package.")
  }
  unknown <- setdiff(tables, names(db_sources))
  if (length(unknown)) {
    cli::cli_abort(c("Unknown table{?s}: {.val {unknown}}.",
                     "i" = "Available: {.val {names(db_sources)}}."))
  }
  own <- is.null(db_connection)
  if (own) {
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
      cli::cli_abort("{.fn update_db} needs {.pkg RSQLite}, or pass {.arg db_connection}.")
    }
    dir.create(dbdir, recursive = TRUE, showWarnings = FALSE)
    db_connection <- DBI::dbConnect(RSQLite::SQLite(), file.path(dbdir, dbname))
    on.exit(DBI::dbDisconnect(db_connection), add = TRUE)
  }
  old <- options(golfastr.cache = "off")
  on.exit(options(old), add = TRUE)

  ids <- NULL
  if (any(vapply(db_sources[tables], function(s) isTRUE(s$shotlink), TRUE))) {
    ids <- list(events = db_fetch("ids", "event_ids.rds"), players = db_fetch("ids", "player_ids.rds"))
  }
  this_year <- as.integer(format(Sys.Date(), "%Y"))

  for (tbl in tables) {
    src <- db_sources[[tbl]]
    exists <- DBI::dbExistsTable(db_connection, tbl)
    if (force_rebuild && exists) {
      DBI::dbRemoveTable(db_connection, tbl)
      exists <- FALSE
    }
    if (is.null(src$first)) {
      data <- db_fetch(src$tag, src$file)
      if (is.null(data)) {
        cli::cli_warn("Could not download {.val {tbl}}; left unchanged.")
        next
      }
      data <- db_standardise(data, tbl)
      DBI::dbWriteTable(db_connection, tbl, as.data.frame(data), overwrite = TRUE)
      cli::cli_inform("{.val {tbl}}: {nrow(data)} rows")
      next
    }

    have <- if (exists) DBI::dbGetQuery(db_connection, paste("SELECT DISTINCT season FROM", tbl))$season else integer()
    todo <- setdiff(src$first:this_year, have)
    if (length(have)) todo <- sort(unique(c(todo, max(have))))
    added <- 0L
    for (season in todo) {
      data <- db_fetch(src$tag, sprintf(src$file, season))
      if (is.null(data) || !nrow(data)) next
      data <- db_standardise(data, tbl, season, ids)
      if (season %in% have) {
        DBI::dbExecute(db_connection, paste("DELETE FROM", tbl, "WHERE season = ?"), params = list(season))
      }
      if (DBI::dbExistsTable(db_connection, tbl)) {
        DBI::dbWriteTable(db_connection, tbl, as.data.frame(data), append = TRUE)
      } else {
        DBI::dbWriteTable(db_connection, tbl, as.data.frame(data))
      }
      added <- added + 1L
    }
    cli::cli_inform("{.val {tbl}}: {added} season{?s} written")
  }

  if (own) invisible(NULL) else invisible(db_connection)
}

db_sources <- list(
  events = list(tag = "schedules", file = "schedule_%d.rds", first = 2004L),
  holes = list(tag = "holes", file = "holes_%d.rds", first = 2004L),
  leaderboards = list(tag = "leaderboards", file = "leaderboards_%d.rds", first = 2004L),
  shots = list(tag = "shots", file = "shots_%d.rds", first = 2012L, shotlink = TRUE),
  tee_times = list(tag = "tee_times", file = "tee_times_%d.rds", first = 2012L, shotlink = TRUE),
  course_setup = list(tag = "course_setup", file = "course_setup_%d.rds", first = 2012L, shotlink = TRUE),
  strokes_gained = list(tag = "strokes_gained", file = "strokes_gained.rds"),
  skill_ratings = list(tag = "skill_ratings", file = "skill_ratings.rds"),
  player_ids = list(tag = "ids", file = "player_ids.rds"),
  event_ids = list(tag = "ids", file = "event_ids.rds")
)

#' @noRd
db_fetch <- function(tag, file) {
  suppressMessages(suppressWarnings(release_load(tag, file)))
}

# One shape for every table: a season column, ID columns as text, ShotLink
# IDs labelled as PGA Tour IDs (with ESPN IDs added where known), and dates
# as text.
#' @noRd
db_standardise <- function(data, tbl, season = NULL, ids = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (tbl == "events") {
    names(data)[names(data) == "event_id"] <- "tournament_id"
  }
  if (!is.null(season)) {
    data$season <- as.integer(season)
  } else if (!"season" %in% names(data) && "year" %in% names(data)) {
    data$season <- as.integer(data$year)
  }
  if (isTRUE(db_sources[[tbl]]$shotlink)) {
    names(data)[names(data) == "tournament_id"] <- "pga_tournament_id"
    names(data)[names(data) == "player_id"] <- "pga_player_id"
    if (!is.null(ids$events) && nrow(ids$events)) {
      data$espn_tournament_id <- ids$events$espn_tournament_id[
        match(data$pga_tournament_id, ids$events$pga_tournament_id)]
    }
    if (!is.null(ids$players) && nrow(ids$players) && "pga_player_id" %in% names(data)) {
      data$espn_player_id <- ids$players$espn_player_id[
        match(data$pga_player_id, ids$players$pga_player_id)]
    }
  }
  for (col in grep("(^|_)id$", names(data), value = TRUE)) {
    data[[col]] <- as.character(data[[col]])
  }
  for (col in names(data)) {
    x <- data[[col]]
    if (inherits(x, "Date")) data[[col]] <- format(x, "%Y-%m-%d")
    if (inherits(x, "POSIXt")) data[[col]] <- format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  data
}
