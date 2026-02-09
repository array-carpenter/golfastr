## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.4.2
* win-builder (devel)

## Resubmission

This is a resubmission. In this version I have:

* Changed cache directory from `~/.golfastr` to `tools::R_user_dir("golfastr", "cache")` to comply with CRAN policy on not writing to user's home directory.

* Replaced `cat()` with `message()` for all console output so messages can be suppressed.

* Removed default file path parameters - all storage functions now require explicit `file_path` argument.

* Replaced DuckDB storage with simpler RDS/Parquet file storage, removing DBI dependency.

* Changed API-calling examples to use `\donttest{}` per CRAN policy.

* Added `\value` tags to all exported function documentation.

## Note on \dontrun{} usage

Examples using `\dontrun{}` require a **user-created data file** (e.g., `file_path = "golf_data.rds"`). These files are created by the user after calling API functions like `load_leaderboard()` and saving results with `save_to_rds()`.

Since no pre-built data file ships with the package and the user must create their own, these examples cannot execute during CRAN checks. This is distinct from network-dependent examples (which use `\donttest{}`).

Affected functions: `load_data()`, `load_from_rds()`, `load_from_parquet()`, `save_to_rds()`, `save_to_parquet()`, `get_player()`, `get_winners()`, `get_majors()`, all `plot_*()` functions, and all analysis functions that operate on saved data files.
