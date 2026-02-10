# golfastr (development version)

# golfastr 0.1.5

CRAN resubmission with documentation fixes.

* Changed ESPN API URLs from HTTP to HTTPS
* Added missing `@return` tags to legacy functions
* Fixed README logo to use absolute URL instead of relative path
* Clarified `\dontrun{}` vs `\donttest{}` usage in examples

# golfastr 0.1.4

This release focuses on CRAN compliance and simplifying the storage backend.

### Breaking Changes

* Removed `save_to_db()` and `load_from_db()` - DuckDB storage has been replaced with simpler file-based storage
* All functions using `db_path` parameter now use `file_path` instead
* Removed DBI dependency

### New Functions

* `save_to_parquet()` - Save tournament data to Parquet format for cross-language compatibility
* `load_from_parquet()` - Load tournament data from Parquet files
* `load_data()` - Unified data loading that auto-detects format (.rds or .parquet)

### Function Updates

* `build_season()` now accepts `file_path` parameter supporting both .rds and .parquet formats
* `check_season()` updated to work with file-based storage
* All analysis functions (`win_leaders()`, `top10_leaders()`, `tournament_history()`, etc.) now use `file_path` parameter

### Backend Changes

* Cache directory now uses `tools::R_user_dir()` for CRAN compliance
* Replaced all `cat()` calls with `message()` for suppressible output
* `arrow` package moved to Suggests (optional, only needed for Parquet)

---

# golfastr 0.1.0

Initial release.

### Data Loading

* `load_tournament()` - Load leaderboard data for any PGA Tour tournament
* `load_tournament_detail()` - Load tournament with hole-by-hole scorecards
* `load_holes()` - Load hole-by-hole scoring data
* `load_schedule()` - Get tournament schedule for a season
* `list_tournaments()` - List available tournaments

### Analysis Functions

* `get_player()` - Look up player results across tournaments
* `get_winners()` - Get tournament winners
* `get_majors()` - Get major championship results
* `player_summary()` - Aggregate player statistics
* `compare_players()` - Side-by-side player comparison
* `win_leaders()` - Players with most wins
* `top10_leaders()` - Players with most top-10 finishes
* `scoring_avg_leaders()` - Scoring average leaderboard
* `tournament_history()` - Historical results for a tournament

### Visualization

* `plot_player()` - Visualize player finishes
* `plot_leaderboard()` - Tournament leaderboard bar chart
* `plot_wins()` - Win distribution chart
* `plot_scoring()` - Scoring distribution histogram
* `plot_head_to_head()` - Compare multiple players

### Storage

* `save_to_rds()` / `load_from_rds()` - RDS file storage
* `build_season()` - Incrementally build season data file
* `check_season()` - Check season loading progress

### Caching

* `cache_info()` - View cache status
* `clear_cache()` - Clear cached data

### Data Sources

* ESPN Golf API (<https://www.espn.com/golf/>)
* PGA Tour season coverage (2020+)
