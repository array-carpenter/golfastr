## Masters 2025 Data Pipeline
## This script demonstrates how to fetch and process data for the 2025 Masters Tournament

library(golfastr)
library(dplyr)

# Masters 2025 Event ID
MASTERS_EVENT_ID <- "401703504"

# Step 1: Fetch complete tournament data
# This gets both the leaderboard and all player scorecards
message("===== Fetching 2025 Masters Tournament Data =====\n")

masters_data <- get_tournament_summary(MASTERS_EVENT_ID)

# Extract components
leaderboard <- masters_data$leaderboard
scorecards <- masters_data$scorecards

# Step 2: Explore the data
message("\n===== Data Summary =====")
message(sprintf("Total players: %d", nrow(leaderboard)))
message(sprintf("Total hole-by-hole records: %d", nrow(scorecards)))

# View top 10
message("\n===== Top 10 Finishers =====")
print(head(leaderboard, 10))

# Step 3: Save processed data
message("\n===== Saving Data =====")

# Save as R data objects
usethis::use_data(leaderboard, overwrite = TRUE)
usethis::use_data(scorecards, overwrite = TRUE)

# Also save as CSV for external use
write.csv(leaderboard, "data-raw/masters_2025_leaderboard.csv", row.names = FALSE)
write.csv(scorecards, "data-raw/masters_2025_scorecards.csv", row.names = FALSE)

message("Data saved successfully!")

# Step 4: Some example analyses
message("\n===== Example Analyses =====")

# Top scorer by round
best_rounds <- scorecards %>%
  group_by(athlete_id, round) %>%
  summarize(round_score = first(round_score), .groups = "drop") %>%
  arrange(round_score) %>%
  head(10)

message("\nBest individual rounds:")
print(best_rounds)

# Most birdies in a round
birdie_counts <- scorecards %>%
  filter(score_type == "BIRDIE") %>%
  group_by(athlete_id, round) %>%
  summarize(birdies = n(), .groups = "drop") %>%
  arrange(desc(birdies)) %>%
  head(10)

message("\nMost birdies in a single round:")
print(birdie_counts)

# Eagles
eagle_counts <- scorecards %>%
  filter(score_type == "EAGLE") %>%
  group_by(athlete_id, hole) %>%
  summarize(eagles = n(), .groups = "drop") %>%
  arrange(desc(eagles), hole)

if (nrow(eagle_counts) > 0) {
  message("\nEagles by hole:")
  print(eagle_counts)
} else {
  message("\nNo eagles recorded")
}

message("\n===== Pipeline Complete! =====")
