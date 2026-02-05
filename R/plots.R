#' Plot Player Results
#'
#' Visualize a player's finishes across tournaments.
#'
#' @param name Player name
#' @param year Optional year filter
#' @param file_path Path to data file (.rds or .parquet)
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' plot_player("Scheffler", file_path = "golf_data.rds")
#' }
plot_player <- function(name, year = NULL, file_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required. Install with: install.packages('ggplot2')")
  }

  data <- get_player(name, file_path = file_path)

  if (!is.null(year)) {
    data <- data[data$year == year, ]
  }

  player_name <- data$display_name[1]

  # Create short tournament names
  data$short_name <- gsub("^The |pres\\. by.*| Championship$", "",
                          data$tournament_name)
  data$short_name <- substr(data$short_name, 1, 15)

  # Order by event
  data$tournament_num <- seq_len(nrow(data))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = tournament_num, y = position)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(color = position <= 10), size = 3) +
    ggplot2::scale_color_manual(values = c("grey50", "darkgreen"),
                                labels = c("Outside Top 10", "Top 10"),
                                name = NULL) +
    ggplot2::scale_y_reverse(breaks = c(1, 10, 25, 50)) +
    ggplot2::labs(
      title = paste(player_name, "- Season Results"),
      x = "Tournament",
      y = "Finish Position"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}

#' Plot Tournament Leaderboard
#'
#' Bar chart of tournament leaderboard.
#'
#' @param tournament Tournament name
#' @param year Season year
#' @param top_n Number of players to show
#' @param file_path Path to data file (.rds or .parquet)
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' plot_leaderboard("Masters", 2025, file_path = "golf_data.rds")
#' }
plot_leaderboard <- function(tournament, year, top_n = 10, file_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  data <- load_data(file_path, tournament = tournament)
  data <- data[data$year == year, ]
  data <- data[order(data$position), ]
  data <- head(data, top_n)

  # Reverse order for plotting
  data$display_name <- factor(data$display_name,
                              levels = rev(data$display_name))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = display_name, y = total_score)) +
    ggplot2::geom_col(fill = "darkgreen") +
    ggplot2::geom_text(ggplot2::aes(label = score_display),
                       hjust = -0.2, size = 3.5) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste(data$tournament_name[1], year),
      x = NULL,
      y = "Total Strokes"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

#' Plot Win Distribution
#'
#' Pie/bar chart of wins by player.
#'
#' @param year Optional year filter
#' @param top_n Number of players to show
#' @param file_path Path to data file (.rds or .parquet)
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' plot_wins(year = 2025, file_path = "golf_data.rds")
#' }
plot_wins <- function(year = NULL, top_n = 10, file_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  data <- win_leaders(year = year, top_n = top_n, file_path = file_path)

  data$player <- factor(data$player, levels = rev(data$player))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = player, y = wins)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = wins), hjust = -0.3, size = 4) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = ifelse(is.null(year), "PGA Tour Wins", paste(year, "PGA Tour Wins")),
      x = NULL,
      y = "Wins"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

#' Plot Scoring Distribution
#'
#' Histogram of tournament scores.
#'
#' @param tournament Tournament name
#' @param year Season year
#' @param file_path Path to data file (.rds or .parquet)
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' plot_scoring("Masters", 2025, file_path = "golf_data.rds")
#' }
plot_scoring <- function(tournament, year, file_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  data <- load_data(file_path, tournament = tournament)
  data <- data[data$year == year, ]

  p <- ggplot2::ggplot(data, ggplot2::aes(x = total_score)) +
    ggplot2::geom_histogram(binwidth = 2, fill = "darkgreen",
                            color = "white", alpha = 0.8) +
    ggplot2::geom_vline(xintercept = mean(data$total_score, na.rm = TRUE),
                        linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = paste(data$tournament_name[1], year, "- Scoring Distribution"),
      x = "Total Strokes",
      y = "Count",
      caption = "Red line = field average"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

#' Plot Head-to-Head Comparison
#'
#' Compare two or more players' finishes across tournaments.
#'
#' @param players Vector of player names
#' @param year Optional year filter
#' @param file_path Path to data file (.rds or .parquet)
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' plot_head_to_head(c("Scheffler", "McIlroy"), file_path = "golf_data.rds")
#' }
plot_head_to_head <- function(players, year = NULL, file_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  all_data <- lapply(players, function(p) {
    tryCatch({
      data <- get_player(p, file_path = file_path)
      if (!is.null(year)) {
        data <- data[data$year == year, ]
      }
      data
    }, error = function(e) NULL)
  })

  all_data <- all_data[!sapply(all_data, is.null)]
  if (length(all_data) == 0) {
    stop("No players found")
  }

  combined <- dplyr::bind_rows(all_data)

  # Create tournament order
  tournaments <- unique(combined$tournament_name)
  combined$tournament_num <- match(combined$tournament_name, tournaments)

  p <- ggplot2::ggplot(combined, ggplot2::aes(x = tournament_num, y = position,
                                               color = display_name)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_reverse(breaks = c(1, 10, 25, 50)) +
    ggplot2::labs(
      title = "Head-to-Head Comparison",
      x = "Tournament",
      y = "Finish Position",
      color = "Player"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}
