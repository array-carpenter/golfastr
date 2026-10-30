witb_release <- function() {
  base <- tempfile("releases")
  dir.create(file.path(base, "witb"), recursive = TRUE)
  saveRDS(tibble::tibble(
    bag_id = c("golfwrx:1", "golfwrx:1", "clubtracker:a", "golfwrx:2"),
    player_id = c("46046", "46046", "46046", NA),
    player_name = c("Scottie Scheffler", "Scottie Scheffler", "Scottie Scheffler", "Nelly Korda"),
    as_of_date = as.Date(c("2025-04-10", "2025-04-10", "2026-04-09", "2026-01-20")),
    event_name = c("Masters Tournament", "Masters Tournament", "Masters Tournament", NA),
    tournament_id = c("R2025014", "R2025014", "R2026014", NA),
    source = c("golfwrx", "golfwrx", "clubtracker", "golfwrx"),
    source_url = "https://example.com",
    slot = c("driver", "putter", "putter", "driver"),
    club_label = c("Driver", "Putter", "Putter", "Driver"),
    club_order = c(1L, 14L, 14L, 1L),
    head_brand = c("TaylorMade", "Scotty Cameron", "TaylorMade", "TaylorMade"),
    head_model = c("Qi10", "Phantom X 5", "Spider Tour X", "Qi35"),
    loft_stated = c(8, NA, NA, 10.5),
    qc_flags = NA_character_
  ), file.path(base, "witb", "witb_clubs.rds"))
  saveRDS(tibble::tibble(
    player_id = "46046", player_name = "Scottie Scheffler", slot = "putter",
    component = "head", old_value = "Scotty Cameron Phantom X 5",
    new_value = "TaylorMade Spider Tour X",
    last_seen_old = as.Date("2025-04-10"), first_seen_new = as.Date("2026-04-09"),
    first_seen_event = "Masters Tournament", tournament_id = "R2026014",
    old_bag_id = "golfwrx:1", new_bag_id = "clubtracker:a", gap_days = 364L,
    uncertain = FALSE, reason = NA_character_, brand_change = TRUE
  ), file.path(base, "witb", "witb_switches.rds"))
  base
}

test_that("witb loads and filters", {
  old <- options(golfastr.release_base = paste0("file://", witb_release()), golfastr.cache = "off")
  on.exit(options(old), add = TRUE)
  expect_equal(nrow(load_witb()), 4)
  expect_equal(nrow(load_witb("scheffler")), 3)
  expect_equal(load_witb("Scheffler", year = 2026)$head_model, "Spider Tour X")
  expect_equal(nrow(load_witb(slot = c("driver", "wedge"))), 2)
  expect_true(is.na(load_witb("Korda")$player_id))
  expect_equal(nrow(load_witb_switches("Scheffler", slot = "putter")), 1)
  expect_equal(nrow(load_witb_switches(slot = "driver")), 0)
  expect_true(all(names(load_witb()) %in% pga_field_descriptions("witb")$field))
  expect_true(all(names(load_witb_switches()) %in% pga_field_descriptions("witb_switches")$field))
})

test_that("a failed witb download gives an empty tibble and a message", {
  old <- options(golfastr.release_base = paste0("file://", tempfile()), golfastr.cache = "off")
  on.exit(options(old), add = TRUE)
  expect_message(out <- load_witb("Scheffler", year = 2026, slot = "putter"), "could not be downloaded")
  expect_equal(nrow(out), 0)
  expect_message(out <- load_witb_switches(), "could not be downloaded")
  expect_equal(nrow(out), 0)
})

test_that("hosted witb data has known slots and a source for every bag", {
  skip_on_cran()
  skip_if_offline()
  clubs <- load_witb()
  skip_if(nrow(clubs) == 0)
  expect_true(all(clubs$slot %in% c("driver", "fairway", "hybrid", "utility", "iron", "wedge", "putter")))
  expect_false(anyNA(clubs$source_url))
  expect_s3_class(clubs$as_of_date, "Date")
  expect_gt(nrow(load_witb("Scheffler", slot = "putter")), 0)
  expect_gt(nrow(load_witb_switches("McIlroy", slot = "driver")), 0)
})
