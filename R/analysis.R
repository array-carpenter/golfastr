#' Player Season Summary
#'
#' Get aggregate statistics for a player's season.
#'
#' @param name Player name (partial match)
#' @param year Optional year filter
#' @param db_path Path to database
#' @return Tibble with season statistics
#' @export
#' @examples
#' \dontrun{
#' player_summary("Scheffler")
#' player_summary("McIlroy", year = 2025)
#' }
player_summary <- function(name, year = NULL, db_path = "data/golfastr.duckdb") {
  data <- get_player(name, db_path = db_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  if (nrow(data) == 0) {
    stop("No data found for player")
  }

  player_name <- data$display_name[1]

  summary_stats <- tibble::tibble(
    player = player_name,
    tournaments = nrow(data),
    wins = sum(data$position == 1, na.rm = TRUE),
    top_5 = sum(data$position <= 5, na.rm = TRUE),
    top_10 = sum(data$position <= 10, na.rm = TRUE),
    top_25 = sum(data$position <= 25, na.rm = TRUE),
    avg_finish = round(mean(data$position, na.rm = TRUE), 1),
    best_finish = min(data$position, na.rm = TRUE),
    worst_finish = max(data$position, na.rm = TRUE)
  )

  return(summary_stats)
}

#' Compare Players
#'
#' Side-by-side comparison of multiple players.
#'
#' @param players Vector of player names
#' @param year Optional year filter
#' @param db_path Path to database
#' @return Tibble comparing player statistics
#' @export
#' @examples
#' \dontrun{
#' compare_players(c("Scheffler", "McIlroy", "Hovland"))
#' }
compare_players <- function(players, year = NULL, db_path = "data/golfastr.duckdb") {
  results <- lapply(players, function(p) {
    tryCatch({
      player_summary(p, year = year, db_path = db_path)
    }, error = function(e) NULL)
  })

  # Remove NULLs
  results <- results[!sapply(results, is.null)]

  if (length(results) == 0) {
    stop("No players found")
  }

  comparison <- dplyr::bind_rows(results)
  comparison <- comparison[order(comparison$avg_finish), ]

  return(comparison)
}

#' Leaderboard Snapshot
#'
#' Get formatted leaderboard for display.
#'
#' @param tournament Tournament name
#' @param year Season year
#' @param top_n Number of players to show (default: 10)
#' @param db_path Path to database
#' @return Formatted tibble for display
#' @export
leaderboard <- function(tournament, year, top_n = 10, db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path, tournament = tournament)
  data <- data[data$year == year, ]

  if (nrow(data) == 0) {
    stop("Tournament not found")
  }

  data <- data[order(data$position), ]
  data <- head(data, top_n)

  result <- data[, c("position", "display_name", "score_display", "total_score")]
  names(result) <- c("Pos", "Player", "Score", "Total")

  return(result)
}

#' Field Strength Analysis
#'
#' Analyze the strength of a tournament field.
#'
#' @param tournament Tournament name
#' @param year Season year
#' @param db_path Path to database
#' @return Tibble with field statistics
#' @export
field_strength <- function(tournament, year, db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path, tournament = tournament)
  data <- data[data$year == year, ]

  if (nrow(data) == 0) {
    stop("Tournament not found")
  }

  tibble::tibble(
    tournament = data$tournament_name[1],
    field_size = nrow(data),
    winning_score = data$total_score[data$position == 1][1],
    cut_line = ifelse(any(data$position > 65),
                      max(data$total_score[data$position <= 65], na.rm = TRUE),
                      NA),
    scoring_avg = round(mean(data$total_score, na.rm = TRUE), 1)
  )
}

#' Most Wins
#'
#' Get players with most wins.
#'
#' @param year Optional year filter
#' @param top_n Number of players to show
#' @param db_path Path to database
#' @return Tibble with win leaders
#' @export
win_leaders <- function(year = NULL, top_n = 10, db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  winners <- data[data$position == 1, ]

  win_counts <- as.data.frame(table(winners$display_name))
  names(win_counts) <- c("player", "wins")
  win_counts <- win_counts[order(-win_counts$wins), ]
  win_counts <- head(win_counts, top_n)

  tibble::as_tibble(win_counts)
}

#' Top 10 Leaders
#'
#' Get players with most top 10 finishes.
#'
#' @param year Optional year filter
#' @param min_events Minimum events played
#' @param top_n Number of players to show
#' @param db_path Path to database
#' @return Tibble with top 10 leaders
#' @export
top10_leaders <- function(year = NULL, min_events = 5, top_n = 10,
                          db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  # Calculate stats per player using base R
  players <- unique(data$display_name)

  player_stats <- lapply(players, function(p) {
    pdata <- data[data$display_name == p, ]
    tibble::tibble(
      display_name = p,
      events = nrow(pdata),
      top_10s = sum(pdata$position <= 10, na.rm = TRUE)
    )
  })

  player_stats <- dplyr::bind_rows(player_stats)
  player_stats$top_10_pct <- round(player_stats$top_10s / player_stats$events * 100, 1)

  # Filter and sort
  player_stats <- player_stats[player_stats$events >= min_events, ]
  player_stats <- player_stats[order(-player_stats$top_10s), ]
  player_stats <- head(player_stats, top_n)

  return(player_stats)
}

#' Scoring Average Leaders
#'
#' Get players with best scoring average.
#'
#' @param year Optional year filter
#' @param min_events Minimum events played
#' @param top_n Number of players to show
#' @param db_path Path to database
#' @return Tibble with scoring average leaders
#' @export
scoring_avg_leaders <- function(year = NULL, min_events = 5, top_n = 10,
                                db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  # Filter to valid scores only
  data <- data[!is.na(data$total_score), ]

  players <- unique(data$display_name)

  player_stats <- lapply(players, function(p) {
    pdata <- data[data$display_name == p, ]
    tibble::tibble(
      player = p,
      events = nrow(pdata),
      scoring_avg = round(mean(pdata$total_score, na.rm = TRUE), 2),
      best_score = min(pdata$total_score, na.rm = TRUE),
      worst_score = max(pdata$total_score, na.rm = TRUE)
    )
  })

  player_stats <- dplyr::bind_rows(player_stats)
  player_stats <- player_stats[player_stats$events >= min_events, ]
  player_stats <- player_stats[order(player_stats$scoring_avg), ]
  player_stats <- head(player_stats, top_n)

  return(player_stats)
}

#' Made Cuts Percentage
#'
#' Get players by percentage of cuts made.
#'
#' @param year Optional year filter
#' @param min_events Minimum events played
#' @param top_n Number of players to show
#' @param db_path Path to database
#' @return Tibble with made cuts leaders
#' @export
made_cuts_leaders <- function(year = NULL, min_events = 5, top_n = 10,
                              db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  players <- unique(data$display_name)

  player_stats <- lapply(players, function(p) {
    pdata <- data[data$display_name == p, ]
    # Assume made cut if position <= 65 (typical cut line)
    made_cut <- sum(!is.na(pdata$total_score) & pdata$position <= 65, na.rm = TRUE)
    tibble::tibble(
      player = p,
      events = nrow(pdata),
      cuts_made = made_cut,
      cut_pct = round(made_cut / nrow(pdata) * 100, 1)
    )
  })

  player_stats <- dplyr::bind_rows(player_stats)
  player_stats <- player_stats[player_stats$events >= min_events, ]
  player_stats <- player_stats[order(-player_stats$cut_pct), ]
  player_stats <- head(player_stats, top_n)

  return(player_stats)
}

#' Tournament History
#'
#' Get historical results for a specific tournament.
#'
#' @param tournament Tournament name (partial match)
#' @param db_path Path to database
#' @return Tibble with tournament winners by year
#' @export
tournament_history <- function(tournament, db_path = "data/golfastr.duckdb") {
  data <- load_from_db(db_path = db_path, tournament = tournament)

  if (nrow(data) == 0) {
    stop("Tournament not found")
  }

  # Get winners for each year
  winners <- data[data$position == 1, ]
  winners <- winners[order(winners$year), ]

  result <- winners[, c("year", "tournament_name", "display_name",
                        "score_display", "total_score")]
  names(result) <- c("Year", "Tournament", "Winner", "Score", "Total")

  return(result)
}
