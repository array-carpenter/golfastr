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
  lo <- min(wp_support)
  for (h in n:1) {
    out[[h]] <- wp_convolve(list(lo = lo, p = pmf[h, ]), out[[h + 1L]])
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
# but the last ("top N and ties"), `u` uniforms [player, sim, segment].
#' @noRd
wp_sim <- function(current, segments, rules, u) {
  n <- length(current)
  sims <- dim(u)[2]
  total <- matrix(current, n, sims)
  alive <- matrix(TRUE, n, sims)
  for (s in seq_along(segments)) {
    for (i in seq_len(n)) total[i, ] <- total[i, ] + wp_draw(segments[[s]][[i]], u[i, , s])
    if (s <= length(rules)) {
      masked <- ifelse(alive, total, Inf)
      line <- apply(masked, 2, function(x) sort(x, partial = min(rules[s], n))[min(rules[s], n)])
      alive <- alive & masked <= rep(line, each = n)
    }
  }
  masked <- ifelse(alive, total, Inf)
  best <- apply(masked, 2, min)
  lead <- masked == rep(best, each = n)
  rowSums(lead / rep(colSums(lead), each = n)) / sims
}
