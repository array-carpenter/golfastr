#' Get Cache Directory Path
#'
#' Returns the path to the golfastr cache directory using the
#' standard R user directory per CRAN policy.
#'
#' @return Character string with the cache directory path.
#' @keywords internal
get_cache_dir <- function() {
  tools::R_user_dir("golfastr", "cache")
}


#' Clear golfastr Cache
#'
#' Removes all cached data files from the local cache directory.
#'
#' @param confirm Logical. If TRUE (default), prompts for confirmation
#'   before clearing. Set to FALSE to skip confirmation.
#' @return Invisible NULL. Called for side effects.
#' @export
#' @examples
#' \donttest{
#' clear_cache()
#' clear_cache(confirm = FALSE)
#' }
clear_cache <- function(confirm = TRUE) {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist. Nothing to clear.")
    return(invisible(NULL))
  }

  files <- list.files(cache_dir, full.names = TRUE)

  if (length(files) == 0) {
    message("Cache is already empty.")
    return(invisible(NULL))
  }

  if (confirm) {
    message(sprintf("This will delete %d cached file(s) from:\n%s",
                    length(files), cache_dir))
    response <- readline("Are you sure? (y/n): ")
    if (!tolower(response) %in% c("y", "yes")) {
      message("Cancelled.")
      return(invisible(NULL))
    }
  }

  unlink(files)
  message(sprintf("Cleared %d file(s) from cache.", length(files)))
  invisible(NULL)
}


#' Display Cache Information
#'
#' Shows information about the current cache including
#' location, number of files, and total size.
#'
#' @return Invisible NULL. Called for side effects (prints to console).
#' @export
#' @examples
#' cache_info()
cache_info <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory: ", cache_dir)
    message("Status: Not yet created (will be created on first use)")
    return(invisible(NULL))
  }

  files <- list.files(cache_dir, full.names = TRUE)
  n_files <- length(files)

  if (n_files == 0) {
    total_size <- 0
  } else {
    total_size <- sum(file.info(files)$size, na.rm = TRUE)
  }

  size_str <- if (total_size < 1024) {
    sprintf("%d bytes", total_size)
  } else if (total_size < 1024^2) {
    sprintf("%.1f KB", total_size / 1024)
  } else {
    sprintf("%.1f MB", total_size / 1024^2)
  }

  message("golfastr Cache Information")
  message("-------------------------")
  message("Location: ", cache_dir)
  message("Files: ", n_files)
  message("Total size: ", size_str)

  if (n_files > 0) {
    message("\nCached files:")
    for (f in basename(files)) {
      message("  - ", f)
    }
  }

  invisible(NULL)
}


#' Save Data to Cache
#'
#' Internal function to save data to the local cache.
#'
#' @param data Data to cache.
#' @param filename Name for the cached file.
#' @return Invisible path to the cached file.
#' @keywords internal
cache_save <- function(data, filename) {
  cache_dir <- get_cache_dir()
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  path <- file.path(cache_dir, filename)
  saveRDS(data, path)
  invisible(path)
}


#' Load Data from Cache
#'
#' Internal function to load data from the local cache.
#'
#' @param filename Name of the cached file.
#' @return The cached data, or NULL if not found.
#' @keywords internal
cache_load <- function(filename) {
  cache_dir <- get_cache_dir()
  path <- file.path(cache_dir, filename)
  if (file.exists(path)) {
    readRDS(path)
  } else {
    NULL
  }
}


#' Check if Data is Cached
#'
#' Internal function to check if data exists in cache.
#'
#' @param filename Name of the cached file.
#' @return Logical indicating if file exists in cache.
#' @keywords internal
cache_exists <- function(filename) {
  cache_dir <- get_cache_dir()
  path <- file.path(cache_dir, filename)
  file.exists(path)
}
