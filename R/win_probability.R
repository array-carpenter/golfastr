# Distribution of strokes to par over the holes still to play in a round,
# for every number of holes already played (element k + 1 = after k holes),
# at one skill level. `cl` holds each hole's cumulative logits in play order.
#' @noRd
wp_suffixes <- function(cl, beta, s) {
  n <- nrow(cl)
  out <- vector("list", n + 1L)
  out[[n + 1L]] <- wp_point(0L)
  if (!n) return(out)
  z <- stats::plogis(cl + beta * s)
  cum <- cbind(0, z, 1)
  pmf <- cum[, -1, drop = FALSE] - cum[, -ncol(cum), drop = FALSE]
  pmf[pmf < 0] <- 0
  # every suffix product at once in the Fourier domain
  width <- ncol(pmf)
  size <- 2L^ceiling(log2((width - 1L) * n + 1L))
  a <- matrix(0, size, n)
  a[seq_len(width), ] <- t(pmf)
  f <- stats::mvfft(a)
  suffix <- f
  if (n > 1) for (h in (n - 1L):1) suffix[, h] <- suffix[, h] * suffix[, h + 1L]
  back <- Re(stats::mvfft(suffix, inverse = TRUE)) / size
  lo <- min(wp_support)
  for (h in seq_len(n)) {
    len <- (width - 1L) * (n - h + 1L) + 1L
    p <- pmax(back[seq_len(len), h], 0)
    out[[h]] <- list(lo = as.integer(lo * (n - h + 1L)), p = p / sum(p))
  }
  out
}

#' @noRd
wp_mix <- function(dists, weights) {
  p <- Reduce(`+`, Map(function(d, w) d$p * w, dists, weights))
  list(lo = dists[[1]]$lo, p = p)
}

# Exact chance each player finishes lowest. `dists` are each player's final
# totals (list(lo, p)); ties are split evenly, which is the playoff. Uses
# E[1 / (1 + K)] = integral over x in [0, 1] of prod_j (P(T_j > t) + P(T_j = t) x)
# with a Gauss-Legendre rule that is exact for these polynomials.
#' @noRd
wp_exact <- function(dists) {
  n <- length(dists)
  if (n == 1L) return(1)
  lo <- min(vapply(dists, `[[`, 1L, "lo"))
  hi <- max(vapply(dists, function(d) d$lo + length(d$p) - 1L, 1L))
  m <- hi - lo + 1L
  p <- matrix(0, n, m)
  for (i in seq_len(n)) {
    idx <- dists[[i]]$lo - lo + seq_along(dists[[i]]$p)
    p[i, idx] <- dists[[i]]$p
  }
  p <- p / rowSums(p)
  above <- 1 - p %*% upper.tri(diag(m), diag = TRUE)
  above[above < 0] <- 0
  gl <- wp_gauss_legendre(ceiling((n + 1) / 2))
  win <- numeric(n)
  for (t in seq_len(m)) {
    pt <- p[, t]
    if (!any(pt > 0)) next
    f <- outer(above[, t], rep(1, length(gl$x))) + outer(pt, gl$x)
    zero <- f <= 0
    logf <- log(ifelse(zero, 1, f))
    sum_log <- colSums(logf)
    n_zero <- colSums(zero)
    others <- ifelse(zero,
                     ifelse(rep(n_zero, each = n) == 1L, exp(rep(sum_log, each = n)), 0),
                     ifelse(rep(n_zero, each = n) == 0L, exp(rep(sum_log, each = n) - logf), 0))
    win <- win + pt * as.vector(others %*% gl$w)
  }
  win / sum(win)
}

#' @noRd
wp_gauss_legendre <- function(k) {
  k <- max(k, 2L)
  j <- matrix(0, k, k)
  b <- seq_len(k - 1) / sqrt(4 * seq_len(k - 1)^2 - 1)
  j[cbind(1:(k - 1), 2:k)] <- b
  j[cbind(2:k, 1:(k - 1))] <- b
  e <- eigen(j, symmetric = TRUE)
  list(x = (e$values + 1) / 2, w = e$vectors[1, ]^2)
}

#' @noRd
wp_draw <- function(d, u) {
  cdf <- cumsum(d$p) / sum(d$p)
  cdf[length(cdf)] <- 1
  d$lo + findInterval(u, cdf, left.open = TRUE)
}

# Monte Carlo when a cut is still to come. `current` is each player's score
# so far, `segments` a list (one per stretch of rounds ending at a cut or the
# finish) of per-player distributions, `rules` the cut after each stretch
# but the last ("top N and ties"), `u` uniforms [sim, player, segment].
#' @noRd
wp_sim <- function(current, segments, rules, u) {
  n <- length(current)
  sims <- dim(u)[1]
  total <- matrix(current, sims, n, byrow = TRUE)
  alive <- matrix(TRUE, sims, n)
  sim_id <- rep(seq_len(sims), times = n)
  for (s in seq_along(segments)) {
    for (i in seq_len(n)) total[, i] <- total[, i] + wp_draw(segments[[s]][[i]], u[, i, s])
    if (s <= length(rules)) {
      masked <- total
      masked[!alive] <- Inf
      sorted <- matrix(masked[order(sim_id, masked)], n)
      alive <- alive & masked <= sorted[min(rules[s], n), ]
    }
  }
  total[!alive] <- Inf
  best <- total[order(sim_id, total)][seq.int(1L, n * sims, by = n)]
  lead <- total == best
  colSums(lead / rowSums(lead)) / sims
}

# Rounds with only a handful of players are playoff holes.
#' @noRd
wp_playoff_rows <- function(holes) {
  n <- tapply(holes$player_id, holes$round, function(x) length(unique(x)))
  first <- if ("1" %in% names(n)) n[["1"]] else max(n)
  playoff_rounds <- as.integer(names(n)[n < 0.25 * first & as.integer(names(n)) > 1])
  holes$round %in% playoff_rounds
}

# Win probability after every hole of one event. With `live = TRUE` the
# event is still going: a player who has not caught up to a tick stays in
# the field at his latest score instead of being treated as withdrawn.
#' @noRd
wp_event <- function(holes, ratings, params, cuts = NULL, n_rounds = NULL,
                     order = NULL, start = NULL, wave = NULL, live = FALSE,
                     sims = 10000, seed = 1) {
  holes <- as.data.frame(holes, stringsAsFactors = FALSE)
  if (is.null(holes$.row)) holes$.row <- seq_len(nrow(holes))
  holes <- holes[!is.na(holes$score) & !is.na(holes$par), ]
  holes$player_id <- as.character(holes$player_id)
  holes$to_par <- holes$score - holes$par
  playoff <- if (is.null(n_rounds)) wp_playoff_rows(holes) else holes$round > n_rounds
  po <- holes[playoff, ]
  holes <- holes[!playoff, ]
  # a player's rounds must run 1, 2, ... without gaps (the data has a few
  # players missing a whole round)
  rounds_played <- tapply(holes$round, holes$player_id, function(r) {
    u <- sort(unique(r))
    identical(u, seq_along(u))
  })
  holes <- holes[holes$player_id %in% names(rounds_played)[rounds_played], ]
  po <- po[po$player_id %in% names(rounds_played)[rounds_played], ]
  if (is.null(n_rounds)) n_rounds <- max(4L, max(holes$round))
  if (is.null(cuts)) cuts <- data.frame(after_round = integer(), rule = integer())
  cuts <- cuts[cuts$after_round < n_rounds, , drop = FALSE]
  cuts <- cuts[order(cuts$after_round), , drop = FALSE]

  players <- sort(unique(holes$player_id))
  n <- length(players)
  pid <- match(holes$player_id, players)
  rating <- if (length(ratings)) unname(ratings[players]) else rep(NA_real_, n)
  names(rating) <- players
  rating[is.na(rating)] <- params$skill_params[["prior_mean"]]
  a <- rep_len(params$a, 2)
  centered <- rating - mean(rating)
  start_strokes <- if (is.null(start)) rep(0, n) else {
    s <- start[players]
    s[is.na(s)] <- 0
    s
  }
  beta <- params$beta
  grid <- wp_shock_grid(9L, params$sigma_round)

  # each hole's cumulative logits by round; unplayed holes borrow the same
  # hole's other rounds, then the pooled distribution for its par
  par_of <- tapply(holes$par, holes$hole, function(p) as.integer(names(which.max(table(p)))))
  hole_par <- rep(4L, 18)
  hole_par[as.integer(names(par_of))] <- pmin(pmax(par_of, 3L), 5L)
  cl_of <- function(x) {
    cm <- cumsum(x)[-length(x)]
    stats::qlogis(pmin(pmax(cm, 1e-12), 1 - 1e-12))
  }
  # fourth index: the whole field, then the morning and afternoon waves. A
  # wave's distribution leans on the field's for that hole and round.
  wave_of <- rep(1L, nrow(holes))
  if (!is.null(wave)) {
    w <- wave$wave[match(paste(holes$player_id, holes$round), paste(wave$player_id, wave$round))]
    wave_of[!is.na(w)] <- ifelse(w[!is.na(w)] == "AM", 2L, 3L)
  }
  holes$wave_i <- wave_of
  prior_n_wave <- if (is.null(params$prior_n_wave)) 20 else params$prior_n_wave
  hole_cl <- array(NA_real_, c(n_rounds, 18, length(wp_support) - 1L, 3L))
  hole_beta <- matrix(beta[as.character(hole_par)], n_rounds, 18, byrow = TRUE)
  for (h in 1:18) {
    prior <- params$par_prior[[as.character(hole_par[h])]]
    all_rounds <- holes$to_par[holes$hole == h]
    for (r in seq_len(n_rounds)) {
      here <- holes$hole == h & holes$round == r
      x <- holes$to_par[here]
      pmf <- if (length(x)) wp_hole_pmf(x, prior, params$prior_n_hole) else
        wp_hole_pmf(all_rounds, prior, params$prior_n_hole)
      hole_cl[r, h, , 1L] <- cl_of(pmf)
      for (v in 2:3) {
        xv <- holes$to_par[here & wave_of == v]
        hole_cl[r, h, , v] <- if (length(xv)) cl_of(wp_hole_pmf(xv, pmf, prior_n_wave)) else
          hole_cl[r, h, , 1L]
      }
    }
  }

  # play order per player-round
  seq_of <- holes$hole
  if (!is.null(order)) {
    key <- paste(order$player_id, order$round, order$hole)
    s <- order$seq[match(paste(holes$player_id, holes$round, holes$hole), key)]
    seq_of[!is.na(s)] <- s[!is.na(s)]
  }
  holes$seq <- seq_of
  holes <- holes[order(pid, holes$round, holes$seq), ]
  pid <- match(holes$player_id, players)
  holes$g <- 18L * (holes$round - 1L) + as.integer(holes$seq)
  progress <- tapply(holes$g, factor(pid, levels = seq_len(n)), max)
  progress[is.na(progress)] <- 0L
  progress <- as.integer(progress)
  # score through each tick; a hole missing from the data counts as par
  cum_to_par <- lapply(seq_len(n), function(i) {
    by_tick <- numeric(progress[i])
    by_tick[holes$g[pid == i]] <- holes$to_par[pid == i]
    c(0, cumsum(by_tick))
  })
  played_by <- lapply(seq_len(n), function(i) {
    x <- holes[pid == i, c("round", "hole", "seq", "to_par")]
    split(x, x$round)
  })
  wave_idx <- matrix(1L, n, n_rounds)
  wave_idx[cbind(pid, holes$round)] <- holes$wave_i
  # all 18 holes in the order the player plays them that round
  full_order <- function(i, r) {
    x <- played_by[[i]][[as.character(r)]]
    if (is.null(x) || !nrow(x)) return(1:18)
    offset <- (x$hole[1] - x$seq[1]) %% 18
    order((1:18 - offset - 1) %% 18 + 1)
  }

  levels_r <- factor(holes$round, levels = seq_len(n_rounds))
  rt <- tapply(holes$to_par, list(factor(pid, levels = seq_len(n)), levels_r), sum)
  full <- tapply(holes$hole, list(factor(pid, levels = seq_len(n)), levels_r), length) == 18
  rt[is.na(full) | !full] <- NA
  round_sg <- -sweep(rt, 2, colMeans(rt, na.rm = TRUE))

  # Skill for the rest of the event from the start of each round: rating plus
  # this week's earlier rounds, centered on the players still in the field
  skill <- matrix(0, n, n_rounds)
  for (r in seq_len(n_rounds)) {
    form <- if (r > 1) rowMeans(round_sg[, seq_len(r - 1), drop = FALSE], na.rm = TRUE) else rep(0, n)
    form[is.na(form)] <- 0
    in_field <- if (live) progress >= 18L * (r - 1L) else progress >= 18L * (r - 1L) + 1L
    if (!any(in_field)) in_field <- rep(TRUE, n)
    skill[, r] <- a[if (r == 1) 1 else 2] * centered + params$w * (form - mean(form[in_field]))
  }

  cache <- new.env()
  dists_from <- function(i, r) {
    key <- paste(i, r)
    if (!is.null(cache[[key]])) return(cache[[key]])
    ord <- full_order(i, r)
    cl_r <- matrix(hole_cl[r, ord, , wave_idx[i, r]], 18)
    suffix <- lapply(grid$node, function(z) wp_suffixes(cl_r, hole_beta[r, ord], skill[i, r] + z))
    later_rounds <- seq_len(n_rounds)[seq_len(n_rounds) > r]
    later <- lapply(later_rounds, function(rr) {
      cl_rr <- matrix(hole_cl[rr, , , wave_idx[i, rr]], 18)
      wp_mix(lapply(grid$node, function(z) wp_suffixes(cl_rr, hole_beta[rr, ], skill[i, r] + z)[[1]]),
             grid$weight)
    })
    names(later) <- as.character(later_rounds)
    out <- list(suffix = suffix, later = later, later_total = Reduce(wp_convolve, later, wp_point(0L)))
    assign(key, out, envir = cache)
    out
  }

  set.seed(seed)
  ends <- sort(unique(c(cuts$after_round, n_rounds)))
  u <- array(stats::runif(n * sims * length(ends)), c(sims, n, length(ends)))

  total_ticks <- max(progress)
  wp <- matrix(0, n, total_ticks + 1L)
  for (g in 0:total_ticks) {
    state <- if (live) pmin(g, progress) else rep(g, n)
    active <- if (live) rep(TRUE, n) else progress >= g
    pending <- cuts[0, , drop = FALSE]
    for (j in seq_len(nrow(cuts))) {
      cut_round <- cuts$after_round[j]
      went_on <- progress > 18L * cut_round
      finished <- state >= 18L * cut_round
      if (g < 18L * cut_round || (!any(went_on) && !all(finished[active]))) {
        pending <- rbind(pending, cuts[j, , drop = FALSE])
        next
      }
      tot <- vapply(seq_len(n), function(i) {
        if (progress[i] >= 18L * cut_round) cum_to_par[[i]][18L * cut_round + 1L] else Inf
      }, 0) + start_strokes
      line <- if (any(went_on)) max(tot[went_on]) else sort(tot[active])[min(cuts$rule[j], sum(active))]
      active <- active & (went_on | tot <= line)
    }
    ids <- which(active)
    if (!length(ids)) next

    rest_now <- lapply(ids, function(i) {
      gi <- state[i]
      r <- if (gi == 0) 1L else as.integer((gi - 1L) %/% 18L + 1L)
      k <- if (gi == 0) 0L else as.integer(gi - 18L * (r - 1L))
      d <- dists_from(i, r)
      if (k == 18L) return(list(now = wp_point(0L), from = r))
      x <- played_by[[i]][[as.character(r)]]
      if (!is.null(x)) x <- x[x$seq <= k, ]
      if (is.null(x) || !nrow(x)) {
        post <- grid$weight
      } else {
        pmfs <- lapply(x$hole, function(h) diff(c(0, stats::plogis(hole_cl[r, h, , wave_idx[i, r]]), 1)))
        post <- wp_shock_posterior(x$to_par, pmfs, skill[i, r], hole_beta[r, x$hole], grid)
      }
      list(now = wp_mix(lapply(d$suffix, function(sf) sf[[k + 1L]]), post), from = r)
    })
    current <- vapply(ids, function(i) cum_to_par[[i]][state[i] + 1L], 0) + start_strokes[ids]

    if (!nrow(pending)) {
      dists <- Map(function(i, rn, cur) {
        later <- dists_from(i, rn$from)$later_total
        d <- wp_convolve(rn$now, later)
        d$lo <- d$lo + as.integer(round(cur))
        d
      }, ids, rest_now, current)
      wp[ids, g + 1L] <- wp_exact(dists)
    } else {
      seg_ends <- sort(unique(c(pending$after_round, n_rounds)))
      segments <- lapply(seq_along(seg_ends), function(s) {
        Map(function(i, rn) {
          from <- if (s == 1) rn$from + 1L else seg_ends[s - 1] + 1L
          rounds_in <- seq_len(n_rounds)[seq_len(n_rounds) >= from & seq_len(n_rounds) <= seg_ends[s]]
          later <- dists_from(i, rn$from)$later
          d <- if (s == 1) rn$now else wp_point(0L)
          for (rr in rounds_in) d <- wp_convolve(d, later[[as.character(rr)]])
          d
        }, ids, rest_now)
      })
      slices <- match(seg_ends, ends)
      wp[ids, g + 1L] <- wp_sim(current, segments, pending$rule, u[, ids, slices, drop = FALSE])
    }
  }

  holes$win_prob <- wp[cbind(match(holes$player_id, players), holes$g + 1L)]
  holes$win_prob_before <- wp[cbind(match(holes$player_id, players), holes$g)]
  out <- holes
  final <- wp[, total_ticks + 1L]

  if (nrow(po)) {
    po <- wp_playoff(po, players, final)
    keep <- intersect(names(out), names(po))
    out <- rbind(out[keep], po[keep])
  }
  out$wpa <- out$win_prob - out$win_prob_before
  out$skill_rating <- unname(rating[match(out$player_id, players)])
  attr(out, "current") <- stats::setNames(final, players)
  out
}

# Playoff holes in the order they were played. Step k is each player's k-th
# playoff hole; a player still in after step k is one who played step k + 1,
# which covers sudden death and aggregate formats alike. The last step goes
# to the leader (position 1 when known, else the fewest playoff strokes).
#' @noRd
wp_playoff <- function(po, players, final) {
  po <- po[order(po$.row), ]
  po$step <- stats::ave(seq_len(nrow(po)), po$player_id, FUN = seq_along)
  po$g <- NA_integer_
  po$seq <- po$hole
  po$win_prob_before <- NA_real_
  po$win_prob <- NA_real_
  share <- final
  alive <- players[final > 0]
  strokes <- stats::setNames(numeric(length(players)), players)
  last <- max(po$step)
  for (k in seq_len(last)) {
    rows <- which(po$step == k & po$player_id %in% alive)
    if (!length(rows)) next
    strokes[po$player_id[rows]] <- strokes[po$player_id[rows]] + po$score[rows]
    before <- share[match(po$player_id[rows], players)]
    if (k < last && any(po$step == k + 1L)) {
      alive <- intersect(alive, po$player_id[po$step == k + 1L])
    } else {
      leader <- if (!is.null(po$position)) po$player_id[rows][po$position[rows] %in% 1] else character()
      if (!length(leader)) {
        best <- min(strokes[po$player_id[rows]])
        leader <- po$player_id[rows][strokes[po$player_id[rows]] == best]
      }
      alive <- unique(leader)
    }
    share[] <- 0
    share[match(alive, players)] <- 1 / length(alive)
    po$win_prob_before[rows] <- before
    po$win_prob[rows] <- share[match(po$player_id[rows], players)]
  }
  po
}

# Cuts already made in the data: after round r, at least 5 players who
# finished it did not go on. The rule ("top N and ties") is taken as the
# number who went on.
#' @noRd
wp_find_cuts <- function(holes) {
  n_holes <- tapply(holes$hole, list(holes$player_id, holes$round), length)
  strokes <- tapply(holes$score, list(holes$player_id, holes$round), sum)
  out <- data.frame(after_round = integer(), rule = integer())
  rounds <- sort(as.integer(colnames(n_holes)))
  for (r in rounds[-length(rounds)]) {
    done <- rowSums(n_holes[, as.character(seq_len(r)), drop = FALSE] == 18, na.rm = TRUE) == r
    went_on <- !is.na(n_holes[, as.character(r + 1L)])
    if (sum(done & !went_on) < 5 || !any(went_on)) next
    total <- rowSums(strokes[, as.character(seq_len(r)), drop = FALSE])
    line <- max(total[done & went_on], na.rm = TRUE)
    out <- rbind(out, data.frame(after_round = r, rule = sum(done & total <= line, na.rm = TRUE)))
  }
  out
}
