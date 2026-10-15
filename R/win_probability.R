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

# Win probability after every hole of one event.
#' @noRd
wp_event <- function(holes, ratings, params, cuts = NULL, n_rounds = NULL,
                     order = NULL, start = NULL, wave = NULL, sims = 10000, seed = 1) {
  holes <- as.data.frame(holes, stringsAsFactors = FALSE)
  holes$player_id <- as.character(holes$player_id)
  holes$to_par <- holes$score - holes$par
  playoff <- wp_playoff_rows(holes)
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
  # third index: the whole field, then the morning and afternoon waves. A
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

  round_sg <- matrix(NA_real_, n, n_rounds)
  rt <- tapply(holes$to_par, list(factor(pid, levels = seq_len(n)), factor(holes$round, levels = seq_len(n_rounds))), sum)
  full <- tapply(holes$hole, list(factor(pid, levels = seq_len(n)), factor(holes$round, levels = seq_len(n_rounds))), length) == 18
  rt[is.na(full) | !full] <- NA
  round_sg <- -sweep(rt, 2, colMeans(rt, na.rm = TRUE))

  set.seed(seed)
  ends <- sort(unique(c(cuts$after_round, n_rounds)))
  u <- array(stats::runif(n * sims * length(ends)), c(sims, n, length(ends)))

  total_ticks <- max(progress)
  wp <- matrix(0, n, total_ticks + 1L)
  cache_round <- 0L
  for (g in 0:total_ticks) {
    r <- if (g == 0) 1L else as.integer((g - 1L) %/% 18L + 1L)
    k <- if (g == 0) 0L else as.integer(g - 18L * (r - 1L))
    active <- progress >= g
    done_cuts <- cuts$after_round[cuts$after_round < r | (cuts$after_round == r & k == 18L)]
    for (c in done_cuts) {
      if (g >= 18L * c) {
        went_on <- progress > 18L * c
        if (any(went_on)) {
          active <- active & went_on
        } else {
          tot <- vapply(seq_len(n), function(i) cum_to_par[[i]][min(g, progress[i]) + 1L], 0) + start_strokes
          line <- sort(tot[active])[min(cuts$rule[cuts$after_round == c], sum(active))]
          active <- active & tot <= line
        }
      }
    }
    ids <- which(active)
    if (!length(ids)) next

    if (r != cache_round) {
      form <- if (r > 1) rowMeans(round_sg[, seq_len(r - 1), drop = FALSE], na.rm = TRUE) else rep(0, n)
      form[is.na(form)] <- 0
      skill <- a[if (r == 1) 1 else 2] * centered + params$w * (form - mean(form[active]))
      suffix <- vector("list", n)
      later <- vector("list", n)
      for (i in seq_len(n)) {
        ord <- full_order(i, r)
        cl_r <- matrix(hole_cl[r, ord, , wave_idx[i, r]], 18)
        suffix[[i]] <- lapply(grid$node, function(z) wp_suffixes(cl_r, hole_beta[r, ord], skill[i] + z))
        later[[i]] <- lapply(seq_len(n_rounds)[seq_len(n_rounds) > r], function(rr) {
          cl_rr <- matrix(hole_cl[rr, , , wave_idx[i, rr]], 18)
          wp_mix(lapply(grid$node, function(z) wp_suffixes(cl_rr, hole_beta[rr, ], skill[i] + z)[[1]]),
                 grid$weight)
        })
        if (length(later[[i]])) names(later[[i]]) <- as.character(seq_len(n_rounds)[seq_len(n_rounds) > r])
      }
      later_total <- lapply(later, function(l) Reduce(wp_convolve, l, wp_point(0L)))
      cache_round <- r
    }

    current <- vapply(ids, function(i) cum_to_par[[i]][g + 1L], 0) + start_strokes[ids]
    rest_now <- lapply(ids, function(i) {
      if (k == 18L) return(wp_point(0L))
      x <- played_by[[i]][[as.character(r)]]
      x <- x[x$seq <= k, ]
      pmfs <- lapply(x$hole, function(h) diff(c(0, stats::plogis(hole_cl[r, h, , wave_idx[i, r]]), 1)))
      post <- wp_shock_posterior(x$to_par, pmfs, skill[i], hole_beta[r, x$hole], grid)
      wp_mix(lapply(suffix[[i]], function(sf) sf[[k + 1L]]), post)
    })

    pending <- cuts[cuts$after_round > r | (cuts$after_round == r & k < 18L), , drop = FALSE]
    if (!nrow(pending)) {
      dists <- Map(function(i, now, cur) {
        d <- wp_convolve(now, later_total[[i]])
        d$lo <- d$lo + as.integer(round(cur))
        d
      }, ids, rest_now, current)
      wp[ids, g + 1L] <- wp_exact(dists)
    } else {
      seg_ends <- sort(unique(c(pending$after_round, n_rounds)))
      segments <- lapply(seq_along(seg_ends), function(s) {
        from <- if (s == 1) r + 1L else seg_ends[s - 1] + 1L
        rounds_in <- seq_len(n_rounds)[seq_len(n_rounds) >= from & seq_len(n_rounds) <= seg_ends[s]]
        Map(function(i, now) {
          d <- if (s == 1) now else wp_point(0L)
          for (rr in rounds_in) d <- wp_convolve(d, later[[i]][[as.character(rr)]])
          d
        }, ids, rest_now)
      })
      slices <- match(seg_ends, ends)
      wp[ids, g + 1L] <- wp_sim(current, segments, pending$rule[order(pending$after_round)],
                                u[, ids, slices, drop = FALSE])
    }
  }

  holes$win_prob <- wp[cbind(match(holes$player_id, players), holes$g + 1L)]
  holes$win_prob_before <- wp[cbind(match(holes$player_id, players), holes$g)]
  out <- holes

  if (nrow(po)) {
    final <- wp[, total_ticks + 1L]
    po <- po[order(po$round, po$hole), ]
    po$win_prob_before <- NA_real_
    po$win_prob <- NA_real_
    po$g <- NA_integer_
    po$seq <- po$hole
    alive <- players[final > 0]
    share <- final
    for (step in unique(paste(po$round, po$hole))) {
      rows <- which(paste(po$round, po$hole) == step & po$player_id %in% alive)
      if (!length(rows)) next
      before <- share[match(po$player_id[rows], players)]
      best <- min(po$score[rows])
      alive <- po$player_id[rows][po$score[rows] == best]
      share[] <- 0
      share[match(alive, players)] <- 1 / length(alive)
      po$win_prob_before[rows] <- before
      po$win_prob[rows] <- share[match(po$player_id[rows], players)]
    }
    keep <- intersect(names(out), names(po))
    out <- rbind(out[keep], po[keep])
  }
  out$wpa <- out$win_prob - out$win_prob_before
  out$skill_rating <- unname(rating[match(out$player_id, players)])
  out
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
