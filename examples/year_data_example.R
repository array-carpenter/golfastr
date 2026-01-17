## Example: Fetching Full Year Tournament Data
## This demonstrates how to use get_year_data() to fetch and save
## PGA Tour data for an entire season

library(golfastr)
library(dplyr)

# Example 1: Get Leaderboards Only for 2025
# Fast - only fetches final standings, no hole-by-hole data
# Saves as both CSV and RDS formats
cat("===== Example 1: Leaderboards Only =====\n")
leaderboards_2025 <- get_year_data(
  year = 2025,
  data_level = "leaderboard",
  output_format = c("csv", "rds"),
  output_dir = "data/2025_leaderboards"
)

# Explore the data
cat(sprintf("\nTotal tournaments: %d\n",
            length(unique(leaderboards_2025$leaderboards$tournament_name))))
cat(sprintf("Total player records: %d\n",
            nrow(leaderboards_2025$leaderboards)))

# Example 2: Get Top 10 Finishers with Scorecards
# Medium speed - leaderboards + hole-by-hole for top 10 per tournament
# Great for analyzing the best players
cat("\n\n===== Example 2: Top 10 with Scorecards =====\n")
top10_2025 <- get_year_data(
  year = 2025,
  data_level = "top_n",
  top_n = 10,
  output_format = c("csv", "json"),
  output_dir = "data/2025_top10"
)

# Analyze top performers
if (!is.null(top10_2025$scorecards)) {
  birdie_leaders <- top10_2025$scorecards %>%
    filter(score_type == "BIRDIE") %>%
    group_by(athlete_id) %>%
    summarize(total_birdies = n()) %>%
    arrange(desc(total_birdies)) %>%
    head(10)

  cat("\nTop birdie makers (among top 10 finishers):\n")
  print(birdie_leaders)
}

# Example 3: Get Complete Data for Specific Tournaments
# Fetch full data only for major championships
cat("\n\n===== Example 3: Specific Tournaments Only =====\n")

# Major championship event IDs (example - verify actual IDs from schedule)
majors <- c(
  "401703504"  # Masters 2025
  # Add other major IDs here
)

majors_2025 <- get_year_data(
  year = 2025,
  data_level = "full",
  output_format = "parquet",  # Efficient format for large datasets
  output_dir = "data/2025_majors",
  tournaments = majors
)

cat(sprintf("\nMajors data collected for %d tournaments\n",
            length(unique(majors_2025$leaderboards$tournament_name))))

# Example 4: Full Season Data (WARNING: Takes a long time!)
# Uncomment to run - fetches everything for every tournament
# This will make hundreds of API calls and take 30+ minutes

# cat("\n\n===== Example 4: Complete Season Data =====\n")
# full_2025 <- get_year_data(
#   year = 2025,
#   data_level = "full",
#   output_format = c("csv", "rds", "parquet"),
#   output_dir = "data/2025_complete"
# )

# Example 5: Reading Saved Data
cat("\n\n===== Example 5: Reading Saved Data =====\n")

# Read CSV
leaderboards_csv <- read.csv("data/2025_leaderboards/2025_pga_leaderboards.csv")
cat(sprintf("Read %d records from CSV\n", nrow(leaderboards_csv)))

# Read RDS (preserves R data types better)
leaderboards_rds <- readRDS("data/2025_leaderboards/2025_pga_leaderboards.rds")
cat(sprintf("Read %d records from RDS\n", nrow(leaderboards_rds)))

# Read JSON
library(jsonlite)
leaderboards_json <- read_json("data/2025_top10/2025_pga_leaderboards.json",
                               simplifyVector = TRUE)
cat(sprintf("Read %d records from JSON\n", nrow(leaderboards_json)))

# Read Parquet (if arrow is installed)
if (requireNamespace("arrow", quietly = TRUE)) {
  leaderboards_parquet <- arrow::read_parquet("data/2025_majors/2025_pga_leaderboards.parquet")
  cat(sprintf("Read %d records from Parquet\n", nrow(leaderboards_parquet)))
}

cat("\n===== Examples Complete =====\n")
