test_that("pga_field_descriptions works", {
  # Leaderboard descriptions
  lb_desc <- pga_field_descriptions("leaderboard")
  expect_s3_class(lb_desc, "data.frame")
  expect_gt(nrow(lb_desc), 5)
  expect_true("field" %in% names(lb_desc))
  expect_true("description" %in% names(lb_desc))

  # Holes descriptions
  holes_desc <- pga_field_descriptions("holes")
  expect_s3_class(holes_desc, "data.frame")
  expect_gt(nrow(holes_desc), 10)
})

test_that("every hosted loader column is documented in the dictionary", {
  base <- make_release_dir(2005)
  old <- options(golfastr.release_base = paste0("file://", base),
                 golfastr.cache = "off")
  on.exit(options(old), add = TRUE)

  lb <- load_leaderboard(2005)
  lb_dict <- pga_field_descriptions("leaderboard")$field
  expect_true(all(names(lb) %in% lb_dict))

  holes <- load_holes(2005, "Fake Masters")
  holes_dict <- pga_field_descriptions("holes")$field
  expect_true(all(names(holes) %in% holes_dict))
})

test_that("pga_score_types works", {
  score_types <- pga_score_types()
  expect_s3_class(score_types, "data.frame")
  expect_true("BIRDIE" %in% score_types$score_type)
  expect_true("EAGLE" %in% score_types$score_type)
  expect_true("PAR" %in% score_types$score_type)
})

test_that("pga_majors works", {
  majors <- pga_majors()
  expect_s3_class(majors, "data.frame")
  expect_equal(nrow(majors), 4)
  expect_true("Masters Tournament" %in% majors$tournament)
})
