# Tests for enrollment analysis functions
# Tests get_enrl() and related enrollment functions

context("Enrollment Analysis")

test_that("get_enrl returns correct data structure", {
  opt <- create_test_opt()
  
  # result <- get_enrl(test_courses, opt)
  
  # expect_s3_class(result, "data.frame")
  # expect_true("enrolled" %in% colnames(result))
  # expect_true("avail" %in% colnames(result))
  # expect_true("SUBJ_CRSE" %in% colnames(result))
  
  skip("get_enrl function needs verification")
})

test_that("get_enrl aggregates by default group_cols", {
  opt <- create_test_opt()
  
  # Test default aggregation (should be course-level, not section-level)
  skip("Aggregation logic needs verification")
})

test_that("get_enrl aggregates by custom group_cols", {
  opt <- create_test_opt(list(
    group_cols = c("CAMP", "COLLEGE", "TERM", "SUBJ_CRSE", "CRN", "SECT")
  ))
  
  # Test section-level aggregation
  skip("Custom grouping needs verification")
})

test_that("get_enrl handles empty course list", {
  opt <- create_test_opt()
  empty_courses <- test_courses[0, ]
  
  # result <- get_enrl(empty_courses, opt)
  # expect_equal(nrow(result), 0)
  
  skip("Error handling needs verification")
})

test_that("get_enrl calculates availability correctly", {
  opt <- create_test_opt()
  
  # result <- get_enrl(test_courses, opt)
  # expect_true(all(result$avail == result$cap - result$enrolled))
  
  skip("Calculation logic needs verification")
})

test_that("get_enrl filters by required columns", {
  opt <- create_test_opt()
  
  # Test that required columns (CAMP, COLLEGE, TERM, SUBJ_CRSE, gen_ed_area) are always included
  skip("Required columns logic needs verification")
})

test_that("get_enrl excludes courses on exclude list when uel=TRUE", {
  opt <- create_test_opt(list(uel = TRUE))
  
  skip("Exclude list logic needs verification")
})

test_that("get_enrl handles AOP course compression", {
  # Test that AOP (All Online Programs) courses are compressed correctly
  skip("AOP compression logic needs verification")
})
