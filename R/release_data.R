#' Load a Dataset from GitHub Releases
#'
#' Internal helper that downloads pre-built season data files from the
#' package's GitHub releases. Caching behavior is controlled by
#' `options(golfastr.cache = ...)`:
#'
#' * `"memory"` (default): downloads are kept for the current session.
#' * `"filesystem"`: downloads are stored under the package cache directory
#'   (see [cache_info()]) and reused for 24 hours across sessions.
#' * `"off"`: every call downloads fresh.
#'
#' @param tag Release tag (e.g., "leaderboards").
#' @param filename Asset filename (e.g., "leaderboards_2026.rds").
#' @return A tibble, or NULL if the download fails (e.g., no internet
#'   connection or the asset doesn't exist).
#' @keywords internal
release_load <- function(tag, filename) {
  cache_mode <- getOption("golfastr.cache", "memory")
  key <- paste(tag, filename, sep = "/")

  if (identical(cache_mode, "memory")) {
    cached <- get0(key, envir = .release_cache, inherits = FALSE)
    if (!is.null(cached)) {
      return(cached)
    }
  } else if (identical(cache_mode, "filesystem")) {
    cache_file <- file.path(get_cache_dir(), "releases", tag, filename)
    if (file.exists(cache_file)) {
      age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
      if (age < 24) {
        return(tibble::as_tibble(readRDS(cache_file)))
      }
    }
  }

  base_url <- getOption(
    "golfastr.release_base",
    "https://github.com/array-carpenter/golfastr/releases/download"
  )
  release_url <- paste(base_url, tag, filename, sep = "/")

  data <- tryCatch(
    {
      con <- gzcon(url(release_url, open = "rb"))
      on.exit(close(con))
      tibble::as_tibble(readRDS(con))
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (!is.null(data)) {
    if (identical(cache_mode, "filesystem")) {
      cache_file <- file.path(get_cache_dir(), "releases", tag, filename)
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(data, cache_file)
    } else if (identical(cache_mode, "memory")) {
      assign(key, data, envir = .release_cache)
    }
  }
  data
}

.release_cache <- new.env(parent = emptyenv())
