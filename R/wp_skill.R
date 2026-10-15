#' @noRd
wp_round_values <- function(holes) {
  holes <- holes[!is.na(holes$score), ]
  key <- paste(holes$tournament_id, holes$player_id, holes$round, sep = "\r")
  n <- tapply(holes$hole, key, function(h) length(unique(h)))
  strokes <- tapply(holes$score, key, sum)
  full <- names(n)[n == 18]
  parts <- do.call(rbind, strsplit(full, "\r", fixed = TRUE))
  out <- tibble::tibble(
    tournament_id = parts[, 1],
    player_id = parts[, 2],
    round = as.integer(parts[, 3]),
    strokes = as.numeric(strokes[full])
  )
  field <- stats::ave(out$strokes, out$tournament_id, out$round, FUN = mean)
  out$sg <- field - out$strokes
  out[order(out$tournament_id, out$player_id, out$round), ]
}

# Rating before each event: exponentially weighted mean of earlier round
# values (weight halves every `half_life` rounds), shrunk toward
# `prior_mean` as if `prior_n` rounds at that level had been played.
#' @noRd
wp_skill_ratings <- function(rounds, events, half_life, prior_mean, prior_n) {
  rounds$start_date <- events$start_date[match(rounds$tournament_id, events$tournament_id)]
  rounds <- rounds[order(rounds$player_id, rounds$start_date, rounds$tournament_id, rounds$round), ]
  d <- 0.5^(1 / half_life)
  decay <- function(x) as.numeric(stats::filter(x, d, method = "recursive"))
  s <- stats::ave(rounds$sg, rounds$player_id, FUN = decay)
  w <- stats::ave(rep(1, nrow(rounds)), rounds$player_id, FUN = decay)
  idx <- seq_len(nrow(rounds))
  first <- !duplicated(rounds[c("player_id", "tournament_id")])
  i <- idx[first]
  has_prev <- i > 1 & c(FALSE, rounds$player_id[-1] == rounds$player_id[-nrow(rounds)])[i]
  prev_s <- ifelse(has_prev, s[pmax(i - 1, 1)], 0)
  prev_w <- ifelse(has_prev, w[pmax(i - 1, 1)], 0)
  n_prev <- i - stats::ave(idx, rounds$player_id, FUN = min)[i]
  tibble::tibble(
    tournament_id = rounds$tournament_id[i],
    player_id = rounds$player_id[i],
    rating = (prev_s + prior_n * prior_mean) / (prev_w + prior_n),
    n_rounds = as.integer(n_prev)
  )
}
