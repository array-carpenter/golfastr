## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.0
* GitHub Actions: macOS-latest (release), windows-latest (release),
  ubuntu-latest (release), ubuntu-latest (oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission

This is golfastr 0.2.1. CRAN currently has 0.2.0.

This version:

* Lets analysis and plot functions take a `data` argument so they work on
  tibbles returned by `load_leaderboard()`, not only on user-created files.
* Aligns older saved-file column names (`display_name`, `event_id`,
  `score_display`) with current loader names (`player_name`,
  `tournament_id`, `score_to_par`).
* Adds a `year` filter to `load_strokes_gained()`.
* Wraps the network example for `load_strokes_gained()` in `\donttest{}`.
  The runnable example uses `use_bundled = TRUE`.
* Documents LIV Golf, LPGA, DP World Tour, and Champions Tour in
  DESCRIPTION and in `tour` arguments.
* Updates the bundled strokes gained snapshot.

## Note on \dontrun{} usage

Examples using `\dontrun{}` still require a user-created data file or a
network call that the user opts into. Analysis examples now also show the
`data = load_leaderboard()` path inside `\dontrun{}` because that path
downloads hosted season files.
