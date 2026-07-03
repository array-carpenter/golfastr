# golfastr (development version)

### Website

* golfastr.com is now a full pkgdown site: the README front page, a grouped
  function reference, a getting-started guide, and a field-descriptions
  article replace the old placeholder page.

### Data releases

* Season assets are now published in three formats — rds, parquet, and csv —
  so the hosted data can be used directly from Python and other tools.
* Hole-by-hole files gain derived columns computed at build time: `to_par`,
  `field_avg` (field scoring average on that hole in that round), `vs_field`
  (strokes vs the field average), and `cume_to_par` (running tournament score
  through each hole, playoff holes included).

### Deprecations

* `get_pga_schedule()`, `get_tournament_leaderboard()`,
  `get_player_scorecards()`, `load_pga_schedule()`, `load_pga_leaderboards()`,
  and `load_pga_hbh()` are deprecated in favor of `load_schedule()`,
  `load_leaderboard()`, and `load_holes()`. They keep working but now warn
  once per session.

### Other improvements

* Errors and messages are formatted with the cli package.
* Hosted-data downloads can be cached across R sessions: set
  `options(golfastr.cache = "filesystem")` to keep downloads for 24 hours in
  the package cache directory, or `"off"` to disable caching. The default
  (`"memory"`) caches for the current session as before.
* R CMD check now runs on GitHub Actions for every push and pull request.

### Hosted Season Data

* `load_leaderboard()` and `load_holes()` now load completed PGA Tour
  tournaments from pre-built files hosted on GitHub releases (updated daily)
  instead of querying ESPN tournament by tournament. Loading a full season of
  leaderboards drops from minutes to seconds, and hole-by-hole data now covers
  the full field rather than requiring one API request per player.
* Both functions gain a `live` argument to bypass the hosted data and fetch
  directly from ESPN (e.g., for in-progress tournaments). The live API is
  also used automatically as a fallback when hosted data is unavailable.
* `load_holes()` output now includes `position` (final leaderboard standing)
  and `year` columns.
* New `data-raw/update_season_data.R` pipeline builds and uploads the hosted
  files; runs daily via GitHub Actions.

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
