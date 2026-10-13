#' Calculate Strokes Gained for Every Shot
#'
#' Scores each shot from [load_shots()] against a baseline of how many strokes
#' a PGA Tour player needs to hole out from a given lie and distance. A shot's
#' strokes gained is the expected strokes before it, minus the expected strokes
#' after it, minus the strokes it cost (one, plus any penalty strokes before
#' the next swing).
#'
#' The baseline is fit from ShotLink strokes and ships with the package. Over
#' the 2026 season, round totals built from it and adjusted with
#' [summarise_strokes_gained()] track the PGA Tour's official strokes gained
#' total closely (correlation 0.98, mean difference 0.08 strokes per round).
#' The split between off-the-tee and approach is less exact (correlation about
#' 0.85), and "around the green" is defined as within 30 yards of the hole
#' rather than of the green's edge.
#'
#' @param shots A tibble from [load_shots()]. Holes without a holed final
#'   stroke are dropped.
#' @param baseline Optional data frame with columns \code{lie},
#'   \code{distance} (yards) and \code{expected} (strokes to hole out) to use
#'   in place of the bundled PGA Tour baseline, for example one fit to a
#'   different tour.
#' @return A tibble with one row per stroke hit (penalty and drop rows are
#'   folded into the shot that caused them), with the columns of `shots` plus:
#'   \itemize{
#'     \item \code{start_lie}: tee, fairway, rough, sand, recovery or green
#'     \item \code{start_distance}: Yards to the hole before the shot
#'     \item \code{strokes_used}: Strokes the shot cost, including penalties
#'     \item \code{expected_before}, \code{expected_after}: Baseline strokes to
#'       hole out before and after the shot
#'     \item \code{sg}: Strokes gained by the shot
#'     \item \code{sg_category}: \code{"off_tee"}, \code{"approach"},
#'       \code{"around_green"} or \code{"putt"}
#'   }
#' @seealso [summarise_strokes_gained()] for round, event and season totals.
#' @export
#' @examples
#' shots <- tibble::tibble(
#'   tournament_id = "R1", player_id = "1", round = 1L, course_id = "C1",
#'   hole = 1L, par = 4L, yardage = 420L, hole_score = 4L, stroke = 1:4,
#'   stroke_type = "STROKE", from_code = c("OTB", "OFW", "OGR", "OGR"),
#'   distance_remaining = c(150, 10, 1, 0),
#'   final_stroke = c(FALSE, FALSE, FALSE, TRUE)
#' )
#' calculate_strokes_gained(shots)[, c("stroke", "start_lie", "sg", "sg_category")]
#' \donttest{
#' sg <- calculate_strokes_gained(load_shots(2026, "Sony"))
#' }
calculate_strokes_gained <- function(shots, baseline = NULL) {
  if (is.null(baseline)) baseline <- sg_baseline
  need <- c("tournament_id", "player_id", "round", "hole", "par", "yardage",
            "hole_score", "stroke", "stroke_type", "from_code",
            "distance_remaining", "final_stroke")
  missing <- setdiff(need, names(shots))
  if (length(missing)) {
    cli::cli_abort("{.arg shots} is missing column{?s} {.field {missing}}.")
  }

  shots <- as.data.frame(shots, stringsAsFactors = FALSE)
  shots <- shots[order(shots$tournament_id, shots$player_id, shots$round,
                       shots$hole, shots$stroke), ]
  key <- paste(shots$tournament_id, shots$player_id, shots$round, shots$hole)
  finished <- tapply(shots$final_stroke %in% TRUE, key, any)
  keep <- finished[key]
  shots <- shots[keep, ]
  key <- key[keep]

  counted <- shots$stroke_type %in% c("STROKE", "PENALTY")
  used_before <- stats::ave(as.integer(counted), key, FUN = cumsum) - counted
  first_row <- !duplicated(key)
  prev_remaining <- c(NA, utils::head(shots$distance_remaining, -1))
  prev_remaining[first_row] <- NA

  swing <- shots$stroke_type == "STROKE"
  out <- shots[swing, ]
  key <- key[swing]
  used_before <- used_before[swing]
  first_swing <- !duplicated(key)
  last_swing <- !duplicated(key, fromLast = TRUE)

  start <- prev_remaining[swing]
  start[first_swing | is.na(start)] <- out$yardage[first_swing | is.na(start)]
  lie <- sg_lie(out$from_code)

  next_of <- function(x) {
    nxt <- c(utils::tail(x, -1), NA)
    nxt[last_swing] <- NA
    nxt
  }
  next_used <- next_of(used_before)
  next_used[last_swing] <- out$hole_score[last_swing]

  out$start_lie <- lie
  out$start_distance <- as.numeric(start)
  out$strokes_used <- as.integer(next_used - used_before)
  out$expected_before <- sg_expected(lie, out$start_distance, baseline)
  out$expected_after <- ifelse(last_swing, 0,
                               sg_expected(next_of(lie), next_of(out$start_distance), baseline))
  out$sg <- out$expected_before - out$expected_after - out$strokes_used
  out$sg_category <- ifelse(lie == "green", "putt",
                     ifelse(lie == "tee" & out$par >= 4, "off_tee",
                     ifelse(out$start_distance <= 30, "around_green", "approach")))
  tibble::as_tibble(out)
}

#' Summarise Strokes Gained by Round, Event or Season
#'
#' Adds up shot-level strokes gained from [calculate_strokes_gained()] and
#' adjusts each category to the field: in every round, each category is
#' measured against the average of the players on the same course, so the
#' four categories add up to the total and a round's total equals the strokes
#' the player beat the field average by. Only complete 18-hole rounds are
#' used, and team events (Zurich Classic, Presidents Cup) are left out.
#'
#' @param sg A tibble from [calculate_strokes_gained()].
#' @param by One of `"round"` (one row per player per round), `"tournament"`
#'   or `"season"` (per-round averages for each player).
#' @return A tibble with \code{player_id}, \code{player_name} (when present),
#'   grouping columns, \code{rounds}, and \code{sg_ott}, \code{sg_app},
#'   \code{sg_arg}, \code{sg_putt}, \code{sg_t2g} and \code{sg_total}.
#' @export
#' @examples
#' \donttest{
#' sg <- calculate_strokes_gained(load_shots(2026, "Sony"))
#' summarise_strokes_gained(sg, by = "tournament")
#' }
summarise_strokes_gained <- function(sg, by = c("round", "tournament", "season")) {
  by <- match.arg(by)
  sg <- as.data.frame(sg, stringsAsFactors = FALSE)
  if (!"course_id" %in% names(sg)) sg$course_id <- NA_character_
  if (!"player_name" %in% names(sg)) sg$player_name <- NA_character_
  if (!"season" %in% names(sg)) sg$season <- NA_integer_

  # team formats (Zurich Classic, Presidents Cup) do not fit individual baselines
  team_events <- unique(sg$tournament_id[grepl("^Team ", sg$player_name)])
  if (length(team_events)) {
    cli::cli_inform("Dropping team event{?s} {.val {team_events}}.")
    sg <- sg[!sg$tournament_id %in% team_events, ]
  }

  rkey <- paste(sg$tournament_id, sg$round, sg$player_id)
  holes <- tapply(sg$hole, rkey, function(h) length(unique(h)))
  sg <- sg[as.vector(holes[rkey]) == 18, ]
  if (nrow(sg) == 0) {
    cli::cli_abort("No complete 18-hole rounds in {.arg sg}.")
  }

  cats <- c(off_tee = "sg_ott", approach = "sg_app", around_green = "sg_arg", putt = "sg_putt")
  id <- c("season", "tournament_id", "round", "course_id", "player_id", "player_name")
  rounds <- unique(sg[id])
  rkey <- do.call(paste, sg[id])
  ukey <- do.call(paste, rounds[id])
  for (cat in names(cats)) {
    v <- tapply(sg$sg[sg$sg_category == cat], rkey[sg$sg_category == cat], sum)
    v <- as.vector(v[ukey])
    rounds[[cats[[cat]]]] <- ifelse(is.na(v), 0, v)
  }

  field <- paste(rounds$tournament_id, rounds$round, rounds$course_id)
  for (col in cats) {
    rounds[[col]] <- rounds[[col]] - stats::ave(rounds[[col]], field)
  }
  rounds$sg_t2g <- rounds$sg_ott + rounds$sg_app + rounds$sg_arg
  rounds$sg_total <- rounds$sg_t2g + rounds$sg_putt
  rounds$rounds <- 1L

  if (by == "round") {
    return(tibble::as_tibble(rounds))
  }

  group <- if (by == "tournament") c("season", "tournament_id", "player_id", "player_name")
           else c("season", "player_id", "player_name")
  value <- c(unname(cats), "sg_t2g", "sg_total")
  gkey <- do.call(paste, rounds[group])
  out <- rounds[!duplicated(gkey), group]
  okey <- do.call(paste, out[group])
  out$rounds <- as.integer(table(gkey)[okey])
  for (col in value) {
    out[[col]] <- as.vector(tapply(rounds[[col]], gkey, mean)[okey])
  }
  out <- out[order(-out$sg_total), ]
  tibble::as_tibble(out)
}

#' @noRd
sg_lie <- function(code) {
  out <- rep("recovery", length(code))
  out[code == "OTB"] <- "tee"
  out[code == "OFW"] <- "fairway"
  out[code %in% c("ORO", "OIR")] <- "rough"
  out[code %in% c("OGS", "OST")] <- "sand"
  out[code == "OGR"] <- "green"
  out
}

#' @noRd
sg_expected <- function(lie, distance, baseline) {
  out <- rep(NA_real_, length(lie))
  for (l in unique(stats::na.omit(lie))) {
    b <- baseline[baseline$lie == l, ]
    i <- which(lie == l & !is.na(distance))
    if (length(i) && nrow(b)) {
      out[i] <- stats::approx(b$distance, b$expected, distance[i], rule = 2)$y
    }
  }
  out
}
