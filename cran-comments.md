## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.0
* GitHub Actions: macOS-latest (release), windows-latest (release),
  ubuntu-latest (release), ubuntu-latest (oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

The note comes from the local machine, not the package:

* "checking HTML version of manual ... Skipping checking HTML validation:
  'tidy' doesn't look like recent enough HTML Tidy."

## Update

This is golfastr 0.3.0, an update to 0.2.0 (on CRAN since February 2026).

This version:

* Adds `load_shots()`, `load_tee_times()` and `load_course_setup()`, which
  read PGA Tour ShotLink season files hosted as GitHub release assets.
  When a file can't be downloaded they return an empty tibble with a
  message instead of an error.
* Adds `calculate_strokes_gained()` and `summarise_strokes_gained()`. The
  baseline they use is a small internal data set in `R/sysdata.rda`.
* Adds `add_win_probability()`, a win probability model for each hole of a
  PGA Tour event. Its fitted parameters are also in `R/sysdata.rda`.
* Adds `load_player_ids()` and `load_event_ids()`, which read an ID
  crosswalk hosted as a GitHub release asset, with the same empty-tibble
  fallback as the other loaders.
* Adds `load_logos()`, which reads a table of logo image URLs hosted as a
  GitHub release asset. It returns URLs only, not images.
* Adds `load_witb()` and `load_witb_switches()`, which read player
  equipment tables (what's in the bag) hosted as GitHub release assets,
  with the same empty-tibble fallback as the other loaders.
* Adds `update_db()`, which writes the hosted tables to a local SQLite
  database or any DBI connection. DBI and RSQLite are in Suggests and are
  checked with `requireNamespace()`. The example writes to `tempdir()`.
* Lets analysis and plot functions take a `data` argument so they work on
  tibbles returned by `load_leaderboard()`, not only on user-created files.
* Aligns older saved-file column names (`display_name`, `event_id`,
  `score_display`) with current loader names (`player_name`,
  `tournament_id`, `score_to_par`).
* Adds a `year` filter to `load_strokes_gained()`.
* Wraps the network example for `load_strokes_gained()` in `\donttest{}`.
  The runnable example uses `use_bundled = TRUE`.
* Adds `\donttest{}` examples to the deprecated `load_pga_schedule()`,
  `load_pga_leaderboards()` and `load_pga_hbh()`.
* Documents LIV Golf, LPGA, DP World Tour, and Champions Tour in
  DESCRIPTION and in `tour` arguments.
* Updates the bundled strokes gained snapshot.

## Method references

* Win probability: original model, no published reference. Each hole uses an ordered logit shift of that day's field score distribution for the hole. A round-level random effect updates as holes are played. Win probability after the last cut is exact, including ties.
* Strokes gained: the standard expected-strokes method. The baseline is fitted to public PGA Tour 'ShotLink' shot data from 2025 and 2026.

## Notes

* Large data sets are not in the package. They are GitHub release files, and the loaders download them on demand.
* If a download fails, the loaders return an empty tibble with a message, not an error.
* `\dontrun{}` examples need a user-created data file or a large download.
* Network examples use `\donttest{}`.
* The tarball is 268 KB.
