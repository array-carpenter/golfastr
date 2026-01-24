# golfastR <img src="man/figures/logo.png" align="right" height="139" alt="golfastR logo" />

> Fast, tidy PGA Tour data in R

`golfastR` provides easy access to PGA Tour tournament data from ESPN, delivering leaderboards and hole-by-hole scoring in tidy data formats ready for analysis.

## Installation

```r
install.packages("golfastR")
```

## Quick Start

```r
library(golfastR)

# Get the tournament schedule
schedule <- load_schedule(2026)

# Load a tournament leaderboard
sony <- load_leaderboard(2026, "Sony")

# Get hole-by-hole scoring for top 10
holes <- load_holes(2026, "Sony", top_n = 10)
```

## Core Functions

### Tournament Schedule

```r
# Get schedule for a season
schedule <- load_schedule(2026)

# Returns: event_id, tournament_name, start_date, end_date
```

### Leaderboards

```r
# Load by tournament name (partial match)
masters <- load_leaderboard(2026, "Masters")
phoenix <- load_leaderboard(2026, "Phoenix")

# Load by event ID
lb <- load_leaderboard(2026, "401703504")

# Load all tournaments for the year
all_lb <- load_leaderboard(2026)
```

### Hole-by-Hole Scoring

```r
# Get scorecards for top finishers
holes <- load_holes(2026, "Sony", top_n = 10)

# Returns: player_id, player_name, round, hole, par, score, score_type
```

### Player Directory

```r
# Get players from recent tournaments
players <- load_players(2026)
```

## Data Fields

### Leaderboard

| Field | Description |
|-------|-------------|
| position | Final standing |
| player_id | ESPN athlete ID |
| player_name | Player display name |
| total_score | Total strokes |
| score_to_par | Score vs par (e.g., "-11") |
| status | Player status |

### Hole-by-Hole

| Field | Description |
|-------|-------------|
| round | Round number (1-4) |
| hole | Hole number (1-18) |
| par | Par for hole |
| score | Strokes on hole |
| score_type | BIRDIE, PAR, BOGEY, EAGLE, etc. |

## Local Caching

Store data locally for faster repeated access:

```r
# Save to local DuckDB database
save_to_db(leaderboard_data)

# Load from database
data <- load_from_db()
data <- load_from_db(tournament = "Masters")
```

## Analysis Functions

```r
# Player season summary
player_summary("Scheffler", year = 2026)

# Compare multiple players
compare_players(c("Scheffler", "McIlroy"), year = 2026)

# Win leaders
win_leaders(year = 2026)

# Scoring average leaders
scoring_avg_leaders(year = 2026)
```

## Data Source

Data is sourced from ESPN's public API. This package is for educational and research purposes.

## License

MIT
