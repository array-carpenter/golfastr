#' Player Season Summary
#'
#' Get aggregate statistics for a player's season.
#'
#' @param name Player name (partial match)
#' @param year Optional year filter
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with season statistics
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' player_summary("Scheffler", data = season)
#' player_summary("McIlroy", year = 2025, file_path = "golf_data.rds")
#' }
player_summary <- function(name, year = NULL, file_path = NULL, data = NULL) {
  data <- get_player(name, file_path = file_path, data = data)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  if (nrow(data) == 0) {
    cli::cli_abort("No data found for player {.val {name}}.")
  }

  player_name <- data$display_name[1]

  tibble::tibble(
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
}

#' Compare Players
#'
#' Side-by-side comparison of multiple players.
#'
#' @param players Vector of player names
#' @param year Optional year filter
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble comparing player statistics
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' compare_players(c("Scheffler", "McIlroy"), data = season)
#' }
compare_players <- function(players, year = NULL, file_path = NULL,
                            data = NULL) {
  results <- lapply(players, function(p) {
    tryCatch({
      player_summary(p, year = year, file_path = file_path, data = data)
    }, error = function(e) NULL)
  })

  results <- results[!vapply(results, is.null, logical(1))]

  if (length(results) == 0) {
    cli::cli_abort("No players found.")
  }

  comparison <- dplyr::bind_rows(results)
  comparison[order(comparison$avg_finish), ]
}

#' Leaderboard Snapshot
#'
#' Get a compact leaderboard for display.
#'
#' @param tournament Tournament name
#' @param year Season year
#' @param top_n Number of players to show (default: 10)
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Formatted tibble for display
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' leaderboard("Masters", 2025, data = season)
#' }
leaderboard <- function(tournament, year, top_n = 10, file_path = NULL,
                        data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path,
                                   tournament = tournament)
  data <- data[data$year == year, ]

  if (nrow(data) == 0) {
    cli::cli_abort("Tournament {.val {tournament}} not found.")
  }

  data <- data[order(data$position), ]
  data <- utils::head(data, top_n)

  result <- data[, c("position", "display_name", "score_display", "total_score")]
  names(result) <- c("Pos", "Player", "Score", "Total")

  result
}

#' Field Strength Analysis
#'
#' Analyze the strength of a tournament field.
#'
#' @param tournament Tournament name
#' @param year Season year
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with field statistics
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' field_strength("Masters", 2025, data = season)
#' }
field_strength <- function(tournament, year, file_path = NULL, data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path,
                                   tournament = tournament)
  data <- data[data$year == year, ]

  if (nrow(data) == 0) {
    cli::cli_abort("Tournament {.val {tournament}} not found.")
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
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with win leaders
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' win_leaders(data = season)
#' }
win_leaders <- function(year = NULL, top_n = 10, file_path = NULL,
                        data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  winners <- data[data$position == 1, ]

  win_counts <- as.data.frame(table(winners$display_name),
                              stringsAsFactors = FALSE)
  names(win_counts) <- c("player", "wins")
  win_counts$wins <- as.integer(win_counts$wins)
  win_counts <- win_counts[order(-win_counts$wins), ]
  win_counts <- utils::head(win_counts, top_n)

  tibble::as_tibble(win_counts)
}

#' Top 10 Leaders
#'
#' Get players with most top 10 finishes.
#'
#' @param year Optional year filter
#' @param min_events Minimum events played
#' @param top_n Number of players to show
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with top 10 leaders
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' top10_leaders(data = season)
#' }
top10_leaders <- function(year = NULL, min_events = 5, top_n = 10,
                          file_path = NULL, data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

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
  player_stats$top_10_pct <- round(
    player_stats$top_10s / player_stats$events * 100, 1
  )

  player_stats <- player_stats[player_stats$events >= min_events, ]
  player_stats <- player_stats[order(-player_stats$top_10s), ]
  utils::head(player_stats, top_n)
}

#' Scoring Average Leaders
#'
#' Get players with best scoring average.
#'
#' @param year Optional year filter
#' @param min_events Minimum events played
#' @param top_n Number of players to show
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with scoring average leaders
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' scoring_avg_leaders(data = season)
#' }
scoring_avg_leaders <- function(year = NULL, min_events = 5, top_n = 10,
                                file_path = NULL, data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

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
  utils::head(player_stats, top_n)
}

#' Made Cuts Percentage
#'
#' Get players by percentage of cuts made.
#'
#' @param year Optional year filter
#' @param min_events Minimum events played
#' @param top_n Number of players to show
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with made cuts leaders
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' made_cuts_leaders(data = season)
#' }
made_cuts_leaders <- function(year = NULL, min_events = 5, top_n = 10,
                              file_path = NULL, data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  players <- unique(data$display_name)

  player_stats <- lapply(players, function(p) {
    pdata <- data[data$display_name == p, ]
    made_cut <- sum(!is.na(pdata$total_score) & pdata$position <= 65,
                    na.rm = TRUE)
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
  utils::head(player_stats, top_n)
}

#' Tournament History
#'
#' Get historical results for a specific tournament.
#'
#' @param tournament Tournament name (partial match)
#' @param file_path Path to data file (.rds or .parquet)
#' @param data Optional leaderboard data frame. Use this instead of
#'   `file_path` when the data is already in memory.
#' @return Tibble with tournament winners by year
#' @export
#' @examples
#' \dontrun{
#' season <- load_leaderboard(2025)
#' tournament_history("Masters", data = season)
#' }
tournament_history <- function(tournament, file_path = NULL, data = NULL) {
  data <- resolve_leaderboard_data(data = data, file_path = file_path,
                                   tournament = tournament)

  if (nrow(data) == 0) {
    cli::cli_abort("Tournament {.val {tournament}} not found.")
  }

  winners <- data[data$position == 1, ]
  winners <- winners[order(winners$year), ]

  result <- winners[, c("year", "tournament_name", "display_name",
                        "score_display", "total_score")]
  names(result) <- c("Year", "Tournament", "Winner", "Score", "Total")

  result
}
