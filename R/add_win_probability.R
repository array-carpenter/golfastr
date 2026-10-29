#' Add Win Probability to Hole-by-Hole Data
#'
#' Estimates each player's chance of winning after every hole, and the win
#' probability added (WPA) by each hole, for PGA Tour stroke play events.
#'
#' Hole files from [load_holes()] already carry these columns for completed
#' events from 2004 on. Use this function for an event in progress or for
#' your own hole data.
#'
#' For an event in progress, players who are behind in their round (or
#' have not teed off) stay in the field at their latest score, and each
#' player's chance right now is returned as \code{attr(x, "current")}.
#'
#' The model plays out the rest of the event from each hole. Each remaining
#' hole is drawn from how the field played that hole that day, shifted for
#' the player's skill rating (strokes per round better than the field, from
#' every earlier round since 2004) and for how the player has done so far
#' this week and this round. Cuts are applied and ties for first are
#' settled as a playoff (split evenly). Once no cut is left the answer is
#' exact; before that it is simulated.
#'
#' Fit on 2004-2019 and checked on 2020-2026, the probabilities are
#' calibrated (for example, players given 10-20% won 15% of the time, and
#' those given 70-90% won 80%). The log-loss of the eventual winner per hole
#' is 3.31 in rounds 1-2 and 1.62 in rounds 3-4, against 3.79 and 1.79 for a
#' logistic model on strokes behind the leader and holes left.
#'
#' Holes are assumed to be played 1 to 18. Players who start on the 10th tee
#' are therefore placed in the wrong order within a round; their totals at
#' the end of each round are unaffected. Playoff holes (rounds after
#' \code{rounds}) split the chances evenly among the players still in until
#' the playoff is decided. Modified Stableford and team events get `NA`.
#'
#' @param holes Hole-by-hole data with \code{tournament_id},
#'   \code{player_id}, \code{round}, \code{hole}, \code{par} and
#'   \code{score}, such as the output of [load_holes()]. Several events can
#'   be passed at once.
#' @param sims Number of simulations used while a cut is still to come.
#'   After the last cut the calculation is exact.
#' @param seed Random seed, so results are repeatable.
#' @param ratings Optional skill ratings: a named numeric vector (names are
#'   ESPN player IDs) or a data frame with \code{player_id} and
#'   \code{rating}. By default the published pre-event ratings are used, or
#'   each player's latest rating for events not yet in them.
#' @param cut Cut rule ("top \code{cut} and ties" after round 2) assumed for
#'   an event whose cut has not happened yet. Use \code{NA} for events with
#'   no cut. Cuts already made are read from the data.
#' @param rounds Number of regulation rounds in the event: 3 for an event
#'   shortened to 54 holes, 5 for the old 90-hole Bob Hope Classic.
#' @param start Optional starting strokes as a named numeric vector (names
#'   are ESPN player IDs), as at the 2019-2024 Tour Championship.
#' @return \code{holes} with these columns added, and the current chances
#'   in \code{attr(, "current")}:
#'   \itemize{
#'     \item \code{win_prob}: Chance the player wins, after this hole
#'     \item \code{win_prob_before}: Chance before this hole
#'     \item \code{wpa}: \code{win_prob - win_prob_before}
#'     \item \code{skill_rating}: Pre-event rating, strokes per round better
#'       than an average tour player
#'   }
#' @export
#' @examples
#' \donttest{
#' holes <- load_holes(2025, "Sony")
#' holes <- add_win_probability(holes, sims = 2000)
#' }
add_win_probability <- function(holes, sims = 10000, seed = 1, ratings = NULL,
                                cut = 65, rounds = 4, start = NULL) {
  required <- c("tournament_id", "player_id", "round", "hole", "par", "score")
  missing <- setdiff(required, names(holes))
  if (length(missing)) {
    cli::cli_abort("{.arg holes} is missing column{?s} {.field {missing}}.")
  }
  holes <- tibble::as_tibble(holes)
  holes$.row <- seq_len(nrow(holes))
  holes$player_id <- as.character(holes$player_id)
  holes$tournament_id <- as.character(holes$tournament_id)
  params <- wp_params
  current <- list()

  pieces <- lapply(split(holes, holes$tournament_id), function(h) {
    id <- h$tournament_id[1]
    name <- if ("tournament_name" %in% names(h)) h$tournament_name[1] else id
    year <- if ("year" %in% names(h)) h$year[1] else NA
    team <- grepl("Zurich", name) && (is.na(year) || year >= 2017)
    if (any(!h$par %in% 3:6, na.rm = TRUE) || team || grepl("Barracuda|Match Play", name)) {
      cli::cli_inform("{name}: not individual stroke play, win probability left as NA.")
      return(wp_empty_columns(h))
    }
    r <- wp_event_ratings(id, unique(h$player_id), ratings)
    regulation_rows <- h$round <= rounds & !is.na(h$score)
    cuts <- wp_find_cuts(h[regulation_rows, ])
    latest <- max(h$round[regulation_rows])
    if (!nrow(cuts) && latest <= 2 && !is.na(cut) && length(unique(h$player_id)) > cut) {
      cuts <- data.frame(after_round = 2L, rule = as.integer(cut))
    }
    out <- wp_event(h, r, params, cuts = cuts, n_rounds = rounds, start = start,
                    live = wp_in_progress(h, rounds), sims = sims, seed = seed)
    now <- attr(out, "current")
    current[[id]] <<- tibble::tibble(tournament_id = id, player_id = names(now),
                                     player_name = if (!"player_name" %in% names(h)) NA_character_ else
                                       h$player_name[match(names(now), h$player_id)],
                                     win_prob = unname(now))
    h$win_prob <- out$win_prob[match(h$.row, out$.row)]
    h$win_prob_before <- out$win_prob_before[match(h$.row, out$.row)]
    h$wpa <- h$win_prob - h$win_prob_before
    h$skill_rating <- unname(r[h$player_id])
    h$skill_rating[is.na(h$skill_rating)] <- params$skill_params[["prior_mean"]]
    h
  })
  out <- dplyr::bind_rows(pieces)
  out <- out[order(out$.row), ]
  out$.row <- NULL
  now <- dplyr::bind_rows(current)
  if (nrow(now)) attr(out, "current") <- now[order(now$tournament_id, -now$win_prob), ]
  out
}

# Finished when there are playoff holes, or when everyone who started the
# last round has played all of it.
#' @noRd
wp_in_progress <- function(h, rounds) {
  h <- h[!is.na(h$score), ]
  if (any(h$round > rounds)) return(FALSE)
  last <- h[h$round == rounds, ]
  if (!nrow(last)) return(TRUE)
  any(tapply(last$hole, last$player_id, length) < 18)
}

#' @noRd
wp_empty_columns <- function(h) {
  h$win_prob <- NA_real_
  h$win_prob_before <- NA_real_
  h$wpa <- NA_real_
  h$skill_rating <- NA_real_
  h
}

#' @noRd
wp_event_ratings <- function(id, players, ratings) {
  if (is.data.frame(ratings)) ratings <- stats::setNames(ratings$rating, as.character(ratings$player_id))
  if (!is.null(ratings)) return(ratings[players])
  by_event <- release_load("skill_ratings", "skill_ratings.rds")
  if (!is.null(by_event) && id %in% by_event$tournament_id) {
    r <- by_event[by_event$tournament_id == id, ]
    return(stats::setNames(r$rating, r$player_id)[players])
  }
  latest <- release_load("skill_ratings", "skill_ratings_latest.rds")
  if (is.null(latest)) {
    cli::cli_inform("Skill ratings could not be downloaded; every player gets the same rating.")
    return(stats::setNames(rep(NA_real_, length(players)), players))
  }
  stats::setNames(latest$rating, latest$player_id)[players]
}
