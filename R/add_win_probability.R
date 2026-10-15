#' Add Win Probability to Hole-by-Hole Data
#'
#' Estimates each player's chance of winning after every hole, and the win
#' probability added (WPA) by each hole, for PGA Tour stroke play events.
#'
#' Hole files from [load_holes()] already carry these columns for completed
#' events from 2004 on. Use this function for an event in progress or for
#' your own hole data.
#'
#' The model plays out the rest of the event from each hole. Each remaining
#' hole is drawn from how the field played that hole that day, shifted for
#' the player's skill rating (strokes per round better than the field, from
#' every earlier round since 2004) and for how the player has done so far
#' this week and this round. Cuts are applied, ties for first are settled as a playoff
#' (split evenly), and the result is calibrated against 2004-2019 outcomes.
#'
#' Holes are assumed to be played 1 to 18. Players who start on the 10th tee
#' are therefore placed in the wrong order within a round; their totals at
#' the end of each round are unaffected. Modified Stableford and team events
#' get `NA`.
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
#' @param rounds Number of rounds in the event.
#' @return \code{holes} with these columns added:
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
                                cut = 65, rounds = 4) {
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

  pieces <- lapply(split(holes, holes$tournament_id), function(h) {
    id <- h$tournament_id[1]
    name <- if ("tournament_name" %in% names(h)) h$tournament_name[1] else id
    if (any(!h$par %in% 3:6) || grepl("Barracuda|Zurich|Match Play", name)) {
      cli::cli_inform("{name}: not individual stroke play, win probability left as NA.")
      return(wp_empty_columns(h))
    }
    r <- wp_event_ratings(id, unique(h$player_id), ratings)
    cuts <- wp_find_cuts(h[!wp_playoff_rows(h), ])
    regulation <- max(h$round[!wp_playoff_rows(h)])
    if (!nrow(cuts) && regulation <= 2 && !is.na(cut) && length(unique(h$player_id)) > cut) {
      cuts <- data.frame(after_round = 2L, rule = as.integer(cut))
    }
    out <- wp_event(h, r, params, cuts = cuts, n_rounds = max(rounds, regulation),
                    sims = sims, seed = seed)
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
  out
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
