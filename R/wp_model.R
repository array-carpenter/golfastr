# Hole scores relative to par; anything outside is folded into the end bins.
wp_support <- -3:5

#' @noRd
wp_hole_pmf <- function(to_par, prior, prior_n) {
  to_par <- pmin(pmax(to_par[!is.na(to_par)], min(wp_support)), max(wp_support))
  counts <- tabulate(to_par - min(wp_support) + 1L, nbins = length(wp_support))
  (counts + prior_n * prior) / (length(to_par) + prior_n)
}

# Ordered-logit shift: a player `s` strokes per round better than the field
# moves every cumulative probability up by beta * s on the logit scale.
# Returns one row per value of s.
#' @noRd
wp_shift_pmf <- function(pmf, s, beta) {
  cum <- cumsum(pmf)[-length(pmf)]
  cum <- pmin(pmax(cum, 1e-12), 1 - 1e-12)
  shifted <- stats::plogis(outer(beta * s, stats::qlogis(cum), "+"))
  shifted <- cbind(shifted, 1)
  out <- shifted - cbind(0, shifted[, -ncol(shifted), drop = FALSE])
  out[out < 0] <- 0
  out / rowSums(out)
}

# Gauss-Hermite rule for a normal round shock (Golub-Welsch).
#' @noRd
wp_shock_grid <- function(k, sigma) {
  j <- matrix(0, k, k)
  off <- sqrt(seq_len(k - 1))
  j[cbind(1:(k - 1), 2:k)] <- off
  j[cbind(2:k, 1:(k - 1))] <- off
  e <- eigen(j, symmetric = TRUE)
  ord <- order(e$values)
  list(node = sigma * e$values[ord], weight = e$vectors[1, ord]^2)
}

#' @noRd
wp_point <- function(x) list(lo = as.integer(x), p = 1)

#' @noRd
wp_convolve <- function(a, b) {
  if (length(a$p) < length(b$p)) {
    tmp <- a
    a <- b
    b <- tmp
  }
  n <- length(a$p)
  out <- numeric(n + length(b$p) - 1L)
  for (i in seq_along(b$p)) {
    idx <- i:(i + n - 1L)
    out[idx] <- out[idx] + b$p[i] * a$p
  }
  list(lo = as.integer(a$lo + b$lo), p = out)
}

# Weights on the shock grid after seeing this round's holes so far.
#' @noRd
wp_shock_posterior <- function(played, pmfs, skill, beta, grid) {
  if (!length(played)) return(grid$weight)
  beta <- rep_len(beta, length(played))
  loglik <- numeric(length(grid$node))
  for (h in seq_along(played)) {
    p <- wp_shift_pmf(pmfs[[h]], skill + grid$node, beta[h])
    bin <- min(max(played[h], min(wp_support)), max(wp_support)) - min(wp_support) + 1L
    loglik <- loglik + log(p[, bin])
  }
  w <- log(grid$weight) + loglik
  w <- exp(w - max(w))
  w / sum(w)
}
