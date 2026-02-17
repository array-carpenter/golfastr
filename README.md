# golfastR <img src="https://raw.githubusercontent.com/array-carpenter/golfastr/main/man/figures/logo.png" align="right" height="139" alt="golfastR logo" />

> Fast, tidy golf data in R

`golfastR` provides easy access to professional golf tournament data from ESPN, delivering leaderboards and hole-by-hole scoring in tidy data formats ready for analysis. Supports **PGA Tour**, **LIV Golf**, **LPGA**, **DP World Tour**, and **Champions Tour**.

## Installation

```r
# Install from CRAN
install.packages("golfastr")

# Or install development version from GitHub
# install.packages("pak")
pak::pak("array-carpenter/golfastr")
```

## Supported Tours

| Tour | Code | Example |
|------|------|---------|
| PGA Tour | `"pga"` | `load_schedule(2026, tour = "pga")` |
| LIV Golf | `"liv"` | `load_schedule(2026, tour = "liv")` |
| LPGA | `"lpga"` | `load_schedule(2026, tour = "lpga")` |
| DP World Tour | `"euro"` | `load_schedule(2026, tour = "euro")` |
| Champions Tour | `"champions"` | `load_schedule(2026, tour = "champions")` |

All functions accept the `tour` parameter. Default is `"pga"`.

## Quick Start

```r
library(golfastr)

# Get the PGA tournament schedule
schedule <- load_schedule(2026)

# Load a tournament leaderboard
sony <- load_leaderboard(2026, "Sony")

# Get hole-by-hole scoring for top 10
holes <- load_holes(2026, "Sony", top_n = 10)

# LIV Golf works the same way
liv_schedule <- load_schedule(2026, tour = "liv")
adelaide <- load_leaderboard(2026, "Adelaide", tour = "liv")
```

## Strokes Gained

Pre-built PGA Tour strokes gained data ships with the package:

```r
# Get all strokes gained data
sg <- load_strokes_gained()

# Look up a specific player
load_strokes_gained("Scheffler")
```

Returns per-round averages for all six SG categories:

| Field | Description |
|-------|-------------|
| sg_putt | Strokes Gained: Putting |
| sg_arg | Strokes Gained: Around the Green |
| sg_app | Strokes Gained: Approach the Green |
| sg_ott | Strokes Gained: Off the Tee |
| sg_t2g | Strokes Gained: Tee to Green |
| sg_total | Strokes Gained: Total |

## Core Functions

### Tournament Schedule

```r
# Get schedule for a season
schedule <- load_schedule(2026)

# LIV Golf schedule
liv <- load_schedule(2026, tour = "liv")

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

# LIV Golf leaderboard
liv_lb <- load_leaderboard(2026, "Adelaide", tour = "liv")
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

## Local Storage

Store data locally for faster repeated access:

```r
# Save to RDS (native R format)
save_to_rds(leaderboard_data, file_path = "golf_data.rds")

# Load from RDS
data <- load_from_rds(file_path = "golf_data.rds")

# Or use Parquet for cross-language compatibility (requires arrow)
save_to_parquet(leaderboard_data, file_path = "golf_data.parquet")
data <- load_from_parquet(file_path = "golf_data.parquet")

# Auto-detect format with load_data()
data <- load_data("golf_data.rds")
data <- load_data("golf_data.parquet", tournament = "Masters")
```

### Build a Season Database

```r
# Incrementally fetch all tournaments for a season
build_season(2025, file_path = "pga_2025.rds")

# Check progress
check_season(2025, file_path = "pga_2025.rds")
```

## Analysis Functions

```r
# Player season summary
player_summary("Scheffler", file_path = "golf_data.rds")

# Compare multiple players
compare_players(c("Scheffler", "McIlroy"), file_path = "golf_data.rds")

# Win leaders
win_leaders(file_path = "golf_data.rds")

# Scoring average leaders
scoring_avg_leaders(file_path = "golf_data.rds")
```

## Data Source

Tournament data is sourced from ESPN's Golf API (<https://www.espn.com/golf/>). Strokes gained data is sourced from the PGA Tour (<https://www.pgatour.com/stats>).

## License

MIT
