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

* Changed all examples requiring network access to use `\dontrun{}`.

* Added `\value` tags to all exported function documentation.

## Note on examples

All examples that require network access or pre-existing data files use `\dontrun{}` as they cannot be executed during CRAN checks without internet connectivity.
