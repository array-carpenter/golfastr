#' Load PGA Tour Schedule
#'
#' Deprecated. Use [load_schedule()] instead, which supports all tours and
#' shares its output format with the rest of the package.
#'
#' @param year Numeric. The year to load (e.g., 2025).
#' @param tour Character. Tour type, default "pga".
#' @return A tibble with tournament schedule data.
#' @export
load_pga_schedule <- function(year, tour = "pga") {
  deprecate_warn("0.2.0", "load_pga_schedule()", "load_schedule()")
  load_schedule(year = as.integer(year), tour = tour)
}


#' Load PGA Leaderboards
#'
#' Deprecated. Use [load_leaderboard()] instead, which serves completed
#' tournaments from pre-built hosted files rather than one live request
#' per event.
#'
#' @param years Numeric vector. Year(s) to load (e.g., 2025 or 2023:2025).
#' @param tournaments Character vector. Optional tournament event IDs or names
#'   to filter. If NULL, loads all tournaments.
#' @param tour Character. Tour type, default "pga".
#' @param dir Character. Optional directory to save CSV files.
#' @return A tibble with leaderboard data.
#' @export
load_pga_leaderboards <- function(years,
                                   tournaments = NULL,
                                   tour = "pga",
                                   dir = NULL) {
  deprecate_warn("0.2.0", "load_pga_leaderboards()", "load_leaderboard()")
  all_data <- list()

  for (year in years) {
    schedule <- load_schedule(year = as.integer(year), tour = tour)

    if (!is.null(tournaments)) {
      # Filter to specified tournaments
      schedule <- schedule[schedule$event_id %in% tournaments |
                           schedule$tournament_name %in% tournaments, ]
    }

    if (nrow(schedule) == 0) {
      next
    }

    for (i in seq_len(nrow(schedule))) {
      event_id <- schedule$event_id[i]
      tryCatch({
        lb <- fetch_leaderboard_fast(event_id, as.integer(year), tour)
        if (!is.null(lb) && nrow(lb) > 0) {
          lb$year <- year
          all_data[[length(all_data) + 1]] <- lb
        }
      }, error = function(e) {
        cli::cli_inform("Failed to load event {event_id}: {conditionMessage(e)}")
      })
    }
  }

  if (length(all_data) == 0) {
    return(tibble::tibble())
  }

  result <- dplyr::bind_rows(all_data)

  # Save to directory if specified
  if (!is.null(dir)) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    for (year in unique(result$year)) {
      year_data <- result[result$year == year, ]
      filepath <- file.path(dir, sprintf("pga_%d_leaderboard.csv", year))
      utils::write.csv(year_data, filepath, row.names = FALSE)
    }
  }

  result
}


#' Load PGA Hole-by-Hole Data
#'
#' Deprecated. Use [load_holes()] instead, which serves full-field
#' scorecards for completed tournaments from pre-built hosted files.
#'
#' @param years Numeric vector. Year(s) to load.
#' @param tournaments Character vector. Optional tournament event IDs or names.
#'   If NULL, loads all tournaments.
#' @param top_n Numeric. Number of top finishers to include scorecards for.
#'   Default is 10. Set to NULL for all players.
#' @param tour Character. Tour type, default "pga".
#' @param dir Character. Optional directory to save CSV files.
#' @return A tibble with hole-by-hole scoring data.
#' @export
load_pga_hbh <- function(years,
                         tournaments = NULL,
                         top_n = 10,
                         tour = "pga",
                         dir = NULL) {
  deprecate_warn("0.2.0", "load_pga_hbh()", "load_holes()")
  all_data <- list()

  for (year in years) {
    schedule <- load_schedule(year = as.integer(year), tour = tour)

    if (!is.null(tournaments)) {
      schedule <- schedule[schedule$event_id %in% tournaments |
                           schedule$tournament_name %in% tournaments, ]
    }

    if (nrow(schedule) == 0) {
      next
    }

    for (i in seq_len(nrow(schedule))) {
      event_id <- schedule$event_id[i]
      tournament_name <- schedule$tournament_name[i]

      tryCatch({
        summary <- get_tournament_summary(event_id, top_n = top_n)
        if (!is.null(summary$scorecards) && nrow(summary$scorecards) > 0) {
          summary$scorecards$year <- year
          summary$scorecards$tournament_name <- tournament_name
          all_data[[length(all_data) + 1]] <- summary$scorecards
        }
      }, error = function(e) {
        cli::cli_inform("Failed to load event {event_id}: {conditionMessage(e)}")
      })
    }
  }

  if (length(all_data) == 0) {
    return(tibble::tibble())
  }

  result <- dplyr::bind_rows(all_data)

  # Save to directory if specified
  if (!is.null(dir)) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    for (year in unique(result$year)) {
      year_data <- result[result$year == year, ]
      filepath <- file.path(dir, sprintf("pga_%d_holes.csv", year))
      utils::write.csv(year_data, filepath, row.names = FALSE)
    }
  }

  result
}

#' Get Tournament Summary (Internal)
#'
#' Internal function to get leaderboard and scorecards for a tournament.
#'
#' @param event_id ESPN event ID
#' @param top_n Number of top finishers to get scorecards for
#' @return List with leaderboard and scorecards
#' @keywords internal
get_tournament_summary <- function(event_id, top_n = NULL) {
  # Get leaderboard using new API
  year <- as.integer(format(Sys.Date(), "%Y"))
  leaderboard <- fetch_leaderboard_fast(event_id, year, "pga")

  if (is.null(leaderboard) || nrow(leaderboard) == 0) {
    return(list(leaderboard = tibble::tibble(), scorecards = tibble::tibble()))
  }

  # Determine which players to get scorecards for
  if (!is.null(top_n) && top_n < nrow(leaderboard)) {
    athlete_ids <- leaderboard$player_id[1:top_n]
  } else {
    athlete_ids <- leaderboard$player_id
  }

  # Get scorecards
  scorecards_list <- lapply(athlete_ids, function(aid) {
    tryCatch(
      fetch_player_holes(event_id, aid, "pga"),
      error = function(e) NULL
    )
  })
  scorecards <- dplyr::bind_rows(scorecards_list)

  list(leaderboard = leaderboard, scorecards = scorecards)
}
