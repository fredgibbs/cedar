# Tests for rollcall function
# Tests R/cones/rollcall.R

context("Rollcall (Student Demographics)")

test_that("rollcall returns correct structure", {
  opt <- create_test_opt(list(course = "HIST 1110"))
  
  # result <- rollcall(test_students, opt)
  
  # expect_s3_class(result, "data.frame")
  # expect_true("Student Classification" %in% colnames(result) || 
  #             "Major" %in% colnames(result))
  
  skip("rollcall function needs verification")
})

test_that("rollcall filters by course correctly", {
  opt <- create_test_opt(list(course = "HIST 1110"))
  
  # result <- rollcall(test_students, opt)
  # expect_true(all(result$SUBJ_CRSE == "HIST 1110"))
  
  skip("Course filtering needs verification")
})

test_that("rollcall filters by term correctly", {
  opt <- create_test_opt(list(term = 202510))
  
  skip("Term filtering needs verification")
})

test_that("rollcall aggregates by student classification", {
  opt <- create_test_opt(list(
    course = "HIST 1110",
    group_cols = c("Student Classification")
  ))
  
  skip("Classification aggregation needs verification")
})

test_that("rollcall aggregates by major", {
  opt <- create_test_opt(list(
    course = "HIST 1110",
    group_cols = c("Major")
  ))
  
  skip("Major aggregation needs verification")
})

test_that("rollcall handles multiple group columns", {
  opt <- create_test_opt(list(
    course = "HIST 1110",
    group_cols = c("Student Classification", "Major")
  ))
  
  skip("Multiple grouping needs verification")
})

test_that("rollcall removes pre-201980 data", {
  # Should filter out terms before Fall 2019
  skip("Historical data filtering needs verification")
})
