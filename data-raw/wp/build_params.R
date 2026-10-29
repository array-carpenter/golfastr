## Bundle the fitted win probability model into R/sysdata.rda next to the
## strokes gained baseline.
##
##   Rscript --vanilla data-raw/wp/build_params.R

cache <- "data-raw/wp/cache"
env <- new.env()
load("R/sysdata.rda", envir = env)
sg_baseline <- env$sg_baseline

fit <- readRDS(file.path(cache, "wp_fit.rds"))
wp_params <- list(
  a = fit$a, w = fit$w, beta = fit$beta, sigma_round = fit$sigma_round,
  par_prior = fit$par_prior, prior_n_hole = fit$prior_n_hole,
  skill_params = fit$skill_params
)
usethis::use_data(sg_baseline, wp_params, internal = TRUE, overwrite = TRUE)
