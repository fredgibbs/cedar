# Tests for registration statistics (regstats)
# Tests R/cones/regstats.R

context("Registration Statistics")

test_that("get_reg_stats returns correct structure", {
  opt <- create_test_opt(list(
    term = 202510,
    thresholds = list(
      min_impacted = 10,
      min_wait = 3,
      pct_sd = 1.5,
      min_squeeze = 5
    )
  ))
  
  # result <- get_reg_stats(test_students, test_courses, opt)
  
  # expect_type(result, "list")
  # expect_true("bumps" %in% names(result))
  # expect_true("dips" %in% names(result))
  # expect_true("waits" %in% names(result))
  # expect_true("squeezes" %in% names(result))
  
  skip("get_reg_stats function needs verification")
})

test_that("regstats identifies enrollment bumps correctly", {
  # Bumps = higher than usual enrollment (> mean + pct_sd * sd)
  skip("Bump detection needs verification")
})

test_that("regstats identifies enrollment dips correctly", {
  # Dips = lower than usual enrollment (< mean - pct_sd * sd)
  skip("Dip detection needs verification")
})

test_that("regstats identifies high waitlists correctly", {
  opt <- create_test_opt(list(
    term = 202510,
    thresholds = list(min_wait = 3)
  ))
  
  # Courses with wait > min_wait should be flagged
  skip("Waitlist detection needs verification")
})

test_that("regstats identifies squeeze courses correctly", {
  opt <- create_test_opt(list(
    term = 202510,
    thresholds = list(min_squeeze = 5)
  ))
  
  # Courses with avail < min_squeeze should be flagged
  skip("Squeeze detection needs verification")
})

test_that("regstats applies min_impacted threshold", {
  opt <- create_test_opt(list(
    term = 202510,
    thresholds = list(min_impacted = 10)
  ))
  
  # Only courses with enrollment >= min_impacted should be analyzed
  skip("Min impacted threshold needs verification")
})

test_that("regstats calculates concern tiers correctly", {
  # critical_high, critical_low, moderate_high, moderate_low, marginally_high, marginally_low, normal
  skip("Tier calculation needs verification")
})

test_that("regstats handles early and late drops", {
  skip("Drop analysis needs verification")
})
