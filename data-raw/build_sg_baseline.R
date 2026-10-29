## Build the strokes gained baseline bundled in R/sysdata.rda
##
## Expected strokes to hole out by lie and distance, fit from every ShotLink
## stroke in the given seasons. Run from the package root:
##
##   Rscript --vanilla data-raw/build_sg_baseline.R 2026 2025 2024

devtools::load_all(quiet = TRUE)

seasons <- as.integer(commandArgs(trailingOnly = TRUE))
base <- "https://github.com/array-carpenter/golfastr/releases/download/shots"
shots <- do.call(rbind, lapply(seasons, function(s) {
  message("reading shots_", s)
  readRDS(url(sprintf("%s/shots_%d.rds", base, s)))
}))

# A flat placeholder baseline is enough to get each shot's start state.
flat <- expand.grid(lie = c("tee", "fairway", "rough", "sand", "recovery", "green"),
                    distance = c(1, 700), expected = 0, stringsAsFactors = FALSE)
states <- calculate_strokes_gained(shots, baseline = flat)

# Strokes still needed from each swing = this swing's cost plus every later one.
key <- paste(states$tournament_id, states$player_id, states$round, states$hole)
states$to_hole_out <- stats::ave(states$strokes_used, key, FUN = function(x) rev(cumsum(rev(x))))
states <- states[states$start_distance > 0 & states$to_hole_out <= 10, ]

grid <- exp(seq(log(0.1), log(700), length.out = 400))
fit_lie <- function(d) {
  # average within narrow log-distance bins, smooth, then force the curve to
  # rise with distance
  bin <- round(log(d$start_distance) * 40) / 40
  agg <- data.frame(
    distance = tapply(d$start_distance, bin, mean),
    expected = tapply(d$to_hole_out, bin, mean),
    n = as.vector(table(bin))
  )
  fit <- stats::loess(expected ~ log(distance), data = agg, weights = n, span = 0.3,
                      control = stats::loess.control(surface = "direct"))
  x <- grid[grid >= min(agg$distance) & grid <= max(agg$distance)]
  y <- stats::isoreg(x, stats::predict(fit, data.frame(distance = x)))$yf
  data.frame(lie = d$start_lie[1], distance = x, expected = y, shots = nrow(d))
}

sg_baseline <- do.call(rbind, lapply(split(states, states$start_lie), fit_lie))
rownames(sg_baseline) <- NULL
attr(sg_baseline, "seasons") <- seasons
message(nrow(states), " strokes from ", length(seasons), " season(s); ",
        nrow(sg_baseline), " baseline points")

usethis::use_data(sg_baseline, internal = TRUE, overwrite = TRUE)
