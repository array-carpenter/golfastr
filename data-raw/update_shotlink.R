## Update ShotLink Data (shots, tee times, course setup)
##
## Pulls every stroke from the PGA Tour GraphQL API (shotDetailsV3), plus
## pairings and tee times (teeTimesV2), and uploads one file per season to the
## "shots", "tee_times" and "course_setup" GitHub releases.
##
## A full season is ~20,000 requests. Requests are spaced out (2s by default,
## longer after a 429 or 5xx) and every response is cached, so a stopped run
## picks up where it left off. Run it in the background:
##
##   nohup Rscript --vanilla data-raw/update_shotlink.R 2025 2024 --upload \
##     > data-raw/shotlink.log 2>&1 &
##
## SHOTLINK_EVENTS=R2025011 limits a run to one or more tournament ids.
##
## Older seasons have strokes, lies and distances but no coordinates or radar.

library(httr2)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
seasons <- as.integer(args[grepl("^\\d{4}$", args)])
upload <- "--upload" %in% args
delay <- as.numeric(Sys.getenv("SHOTLINK_DELAY", "2"))
cache_dir <- Sys.getenv("SHOTLINK_CACHE", "data-raw/shotlink-cache")
only_events <- strsplit(Sys.getenv("SHOTLINK_EVENTS"), ",")[[1]]
out_dir <- file.path(cache_dir, "out")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (length(seasons) == 0) stop("Pass one or more seasons, e.g. 2025 2024")

api_key <- "da2-gsrx5bibzbb4njvhl7t37wqyl4"

# --- requests ---------------------------------------------------------------

pga_query <- function(query) {
  resp <- request("https://orchestrator.pgatour.com/graphql") |>
    req_headers("Content-Type" = "application/json", "x-api-key" = api_key) |>
    req_user_agent("golfastr data refresh (github.com/array-carpenter/golfastr)") |>
    req_body_json(list(query = query)) |>
    req_timeout(60) |>
    req_retry(
      max_tries = 8,
      retry_on_failure = TRUE,
      is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
      backoff = function(i) min(600, 30 * 2^(i - 1))
    ) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
  Sys.sleep(delay)
  if (resp_status(resp) != 200) {
    stop("HTTP ", resp_status(resp))
  }
  resp_body_string(resp)
}

# Fetch once, then always read from disk. A response with GraphQL errors is
# cached too, so a missing round is not re-requested on every run.
cached <- function(path, query) {
  if (file.exists(path)) {
    hit <- tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(hit)) return(hit)
    # a run stopped mid-write leaves a truncated file
    file.remove(path)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  body <- pga_query(query)
  tmp <- paste0(path, ".tmp")
  con <- gzfile(tmp, "w")
  writeLines(body, con)
  close(con)
  file.rename(tmp, path)
  fromJSON(path, simplifyVector = FALSE)
}

# --- queries ----------------------------------------------------------------

schedule_query <- function(season) sprintf(
  '{ schedule(tourCode: "R", year: "%d") { completed { tournaments {
     id tournamentName startDate courseName city state country purse } } } }',
  season
)

tee_times_query <- function(tid) sprintf(
  '{ teeTimesV2(id: "%s") { timezone courses { id courseName hostCourse }
     rounds { roundInt groups { groupNumber teeTime startTee backNine courseId
       players { id displayName } } } } }',
  tid
)

shots_query <- function(tid, pid, round) sprintf(
  '{ shotDetailsV3(tournamentId: "%s", playerId: "%s", round: %d, includeRadar: true) {
     holes { holeNumber par yardage score
       pinGreen { leftToRightCoords { x y } }
       strokes { strokeNumber strokeType distance distanceRemaining
         fromLocation toLocation fromLocationCode toLocationCode playByPlay finalStroke
         overview { leftToRightCoords { fromCoords { x y } toCoords { x y } } }
         radarData { clubSpeed ballSpeed smashFactor launchSpin spinAxis
           verticalLaunchAngle horizontalLaunchAngle apexHeight apexRange apexSide
           actualFlightTime } } } } }',
  tid, pid, as.integer(round)
)

# --- parsing helpers --------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
coord <- function(x) {
  v <- num(x)
  if (is.na(v) || v < 0) NA_real_ else v
}
radar <- function(x) {
  v <- num(x)
  if (is.na(v) || v == 0) NA_real_ else v
}

# "279 yds", "8 ft 3 in", "11 in" -> yards
to_yards <- function(x) {
  x <- chr(x)
  if (is.na(x)) return(NA_real_)
  yds <- regmatches(x, regexpr("[0-9.]+(?= yds?)", x, perl = TRUE))
  ft <- regmatches(x, regexpr("[0-9.]+(?= ft)", x, perl = TRUE))
  inch <- regmatches(x, regexpr("[0-9.]+(?= in)", x, perl = TRUE))
  if (length(yds)) return(as.numeric(yds))
  if (length(ft) || length(inch)) {
    total_in <- (if (length(ft)) as.numeric(ft) * 12 else 0) +
      (if (length(inch)) as.numeric(inch) else 0)
    return(total_in / 36)
  }
  NA_real_
}

col <- function(rows, f, type) vapply(rows, f, type, USE.NAMES = FALSE)

parse_tee_times <- function(tt, ev, season) {
  t <- tt$data$teeTimesV2
  courses <- t$courses
  course_name <- setNames(vapply(courses, function(c) chr(c$courseName), ""),
                          vapply(courses, function(c) chr(c$id), ""))
  rows <- unlist(lapply(t$rounds, function(rd) unlist(lapply(rd$groups, function(g)
    lapply(g$players, function(p) list(rd = rd, g = g, p = p))), recursive = FALSE)),
    recursive = FALSE)
  if (length(rows) == 0) return(NULL)
  course_id <- col(rows, function(x) chr(x$g$courseId), "")
  data.frame(
    season = season, tournament_id = ev$id, tournament_name = chr(ev$tournamentName),
    round = col(rows, function(x) as.integer(x$rd$roundInt), 0L),
    group = col(rows, function(x) as.integer(x$g$groupNumber %||% NA), 0L),
    tee_time = as.POSIXct(col(rows, function(x) num(x$g$teeTime), 0) / 1000,
                          origin = "1970-01-01", tz = "UTC"),
    timezone = chr(t$timezone),
    start_tee = col(rows, function(x) as.integer(x$g$startTee %||% NA), 0L),
    course_id = course_id, course_name = unname(course_name[course_id]),
    player_id = col(rows, function(x) chr(x$p$id), ""),
    player_name = col(rows, function(x) chr(x$p$displayName), ""),
    stringsAsFactors = FALSE
  )
}

parse_shots <- function(res, season, ev, pr) {
  holes <- res$data$shotDetailsV3$holes
  rows <- unlist(lapply(holes, function(h) lapply(h$strokes, function(s) list(h = h, s = s))),
                 recursive = FALSE)
  if (length(rows) == 0) return(NULL)
  lr <- function(x) x$s$overview$leftToRightCoords
  rd <- function(x) x$s$radarData
  data.frame(
    season = season, tournament_id = ev$id, tournament_name = chr(ev$tournamentName),
    player_id = pr$pid, player_name = pr$name, round = pr$round, course_id = pr$course_id,
    hole = col(rows, function(x) as.integer(x$h$holeNumber), 0L),
    par = col(rows, function(x) as.integer(x$h$par %||% NA), 0L),
    yardage = col(rows, function(x) as.integer(x$h$yardage %||% NA), 0L),
    hole_score = col(rows, function(x) suppressWarnings(as.integer(chr(x$h$score))), 0L),
    stroke = col(rows, function(x) as.integer(x$s$strokeNumber), 0L),
    stroke_type = col(rows, function(x) chr(x$s$strokeType), ""),
    distance = col(rows, function(x) to_yards(x$s$distance), 0),
    distance_remaining = col(rows, function(x) to_yards(x$s$distanceRemaining), 0),
    from_location = col(rows, function(x) chr(x$s$fromLocation), ""),
    to_location = col(rows, function(x) chr(x$s$toLocation), ""),
    from_code = col(rows, function(x) chr(x$s$fromLocationCode), ""),
    to_code = col(rows, function(x) chr(x$s$toLocationCode), ""),
    final_stroke = col(rows, function(x) isTRUE(x$s$finalStroke), TRUE),
    play_by_play = col(rows, function(x) chr(x$s$playByPlay), ""),
    from_x = col(rows, function(x) coord(lr(x)$fromCoords$x), 0),
    from_y = col(rows, function(x) coord(lr(x)$fromCoords$y), 0),
    to_x = col(rows, function(x) coord(lr(x)$toCoords$x), 0),
    to_y = col(rows, function(x) coord(lr(x)$toCoords$y), 0),
    club_speed = col(rows, function(x) radar(rd(x)$clubSpeed), 0),
    ball_speed = col(rows, function(x) radar(rd(x)$ballSpeed), 0),
    smash_factor = col(rows, function(x) radar(rd(x)$smashFactor), 0),
    spin_rate = col(rows, function(x) radar(rd(x)$launchSpin), 0),
    spin_axis = col(rows, function(x) num(rd(x)$spinAxis), 0),
    launch_angle = col(rows, function(x) radar(rd(x)$verticalLaunchAngle), 0),
    launch_direction = col(rows, function(x) num(rd(x)$horizontalLaunchAngle), 0),
    apex_height = col(rows, function(x) radar(rd(x)$apexHeight), 0),
    apex_range = col(rows, function(x) radar(rd(x)$apexRange), 0),
    apex_side = col(rows, function(x) num(rd(x)$apexSide), 0),
    flight_time = col(rows, function(x) radar(rd(x)$actualFlightTime), 0),
    stringsAsFactors = FALSE
  )
}

parse_setup <- function(res, season, ev, pr) {
  holes <- res$data$shotDetailsV3$holes
  if (length(holes) == 0) return(NULL)
  data.frame(
    season = season, tournament_id = ev$id, tournament_name = chr(ev$tournamentName),
    round = pr$round, course_id = pr$course_id,
    hole = col(holes, function(h) as.integer(h$holeNumber), 0L),
    par = col(holes, function(h) as.integer(h$par %||% NA), 0L),
    yardage = col(holes, function(h) as.integer(h$yardage %||% NA), 0L),
    pin_x = col(holes, function(h) coord(h$pinGreen$leftToRightCoords$x), 0),
    pin_y = col(holes, function(h) coord(h$pinGreen$leftToRightCoords$y), 0),
    stringsAsFactors = FALSE
  )
}

# --- per season -------------------------------------------------------------

run_season <- function(season) {
  sdir <- file.path(cache_dir, season)
  sched <- cached(file.path(sdir, "schedule.json.gz"), schedule_query(season))
  events <- unlist(lapply(sched$data$schedule$completed, `[[`, "tournaments"),
                   recursive = FALSE)
  if (length(only_events)) events <- Filter(function(ev) ev$id %in% only_events, events)
  message(season, ": ", length(events), " completed events")

  tee_times <- list()
  shots <- list()
  setup <- list()

  for (ev in events) {
    tid <- ev$id
    tt <- cached(file.path(sdir, tid, "tee_times.json.gz"), tee_times_query(tid))
    tee <- parse_tee_times(tt, ev, season)
    if (is.null(tee)) {
      message("  ", tid, " ", ev$tournamentName, ": no tee times, skipped")
      next
    }
    tee_times[[tid]] <- tee
    player_rounds <- unique(tee[c("player_id", "player_name", "round", "course_id")])
    paths <- file.path(sdir, tid, "shots",
                       sprintf("%s_%d.json.gz", player_rounds$player_id, player_rounds$round))
    message("  ", tid, " ", ev$tournamentName, ": ", nrow(player_rounds),
            " player-rounds, ", sum(!file.exists(paths)), " to fetch")

    for (i in seq_len(nrow(player_rounds))) {
      pr <- list(pid = player_rounds$player_id[i], name = player_rounds$player_name[i],
                 round = player_rounds$round[i], course_id = player_rounds$course_id[i])
      res <- tryCatch(cached(paths[i], shots_query(tid, pr$pid, pr$round)),
                      error = function(e) {
                        message("    ", pr$pid, " R", pr$round, ": ", conditionMessage(e))
                        NULL
                      })
      key <- paste(tid, pr$pid, pr$round)
      shots[[key]] <- parse_shots(res, season, ev, pr)
      setup[[key]] <- parse_setup(res, season, ev, pr)
    }
  }

  setup <- dplyr::bind_rows(setup)
  setup <- setup[!duplicated(setup[c("tournament_id", "round", "course_id", "hole")]), ]
  out <- list(
    tee_times = tibble::as_tibble(dplyr::bind_rows(tee_times)),
    shots = tibble::as_tibble(dplyr::bind_rows(shots)),
    course_setup = tibble::as_tibble(setup)
  )
  message(season, ": ", nrow(out$shots), " strokes, ", nrow(out$tee_times),
          " tee time rows, ", nrow(out$course_setup), " hole setups")
  out
}

write_release <- function(x, tag, season) {
  base <- file.path(out_dir, sprintf("%s_%d", tag, season))
  saveRDS(x, paste0(base, ".rds"))
  utils::write.csv(x, paste0(base, ".csv"), row.names = FALSE)
  if (requireNamespace("arrow", quietly = TRUE)) arrow::write_parquet(x, paste0(base, ".parquet"))
  if (upload) {
    if (!tag %in% piggyback::pb_releases("array-carpenter/golfastr")$tag_name) {
      piggyback::pb_release_create("array-carpenter/golfastr", tag)
    }
    for (ext in c(".rds", ".csv", ".parquet")) {
      path <- paste0(base, ext)
      if (!file.exists(path)) next
      message("Uploading ", basename(path), " to release '", tag, "'")
      # a new release can take a few seconds to show up in the API
      for (attempt in 1:5) {
        piggyback::.pb_cache_clear()
        ok <- tryCatch({
          piggyback::pb_upload(path, repo = "array-carpenter/golfastr", tag = tag, overwrite = TRUE)
          TRUE
        }, error = function(e) {
          message("  upload attempt ", attempt, " failed: ", conditionMessage(e))
          FALSE
        })
        if (ok) break
        Sys.sleep(15 * attempt)
      }
    }
  }
}

for (season in seasons) {
  data <- run_season(season)
  for (tag in names(data)) write_release(data[[tag]], tag, season)
}
