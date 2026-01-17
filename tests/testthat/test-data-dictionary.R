test_that("pga_field_descriptions works", {
  # Leaderboard descriptions
  lb_desc <- pga_field_descriptions("leaderboard")
  expect_s3_class(lb_desc, "data.frame")
  expect_gt(nrow(lb_desc), 10)
  expect_true("field" %in% names(lb_desc))
  expect_true("description" %in% names(lb_desc))

  # Holes descriptions
  holes_desc <- pga_field_descriptions("holes")
  expect_s3_class(holes_desc, "data.frame")
  expect_gt(nrow(holes_desc), 10)
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
