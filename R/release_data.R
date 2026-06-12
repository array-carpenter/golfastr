#' Load a Dataset from GitHub Releases
#'
#' Internal helper that downloads pre-built season data files from the
#' package's GitHub releases. Downloads are cached for the session so
#' repeated calls don't re-fetch the same file.
#'
#' @param tag Release tag (e.g., "leaderboards").
#' @param filename Asset filename (e.g., "leaderboards_2026.rds").
#' @return A tibble, or NULL if the download fails (e.g., no internet
#'   connection or the asset doesn't exist).
#' @keywords internal
release_load <- function(tag, filename) {
  key <- paste(tag, filename, sep = "/")
  cached <- get0(key, envir = .release_cache, inherits = FALSE)
  if (!is.null(cached)) {
    return(cached)
  }

  release_url <- sprintf(
    "https://github.com/array-carpenter/golfastr/releases/download/%s/%s",
    tag, filename
  )

  data <- tryCatch(
    {
      con <- url(release_url)
      on.exit(close(con))
      tibble::as_tibble(readRDS(con))
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (!is.null(data)) {
    assign(key, data, envir = .release_cache)
  }
  data
}

.release_cache <- new.env(parent = emptyenv())
