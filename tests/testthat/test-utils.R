# Tests for utility functions
# Tests various helper functions

context("Utility Functions")

test_that("term parsing handles single terms", {
  # Test extracting start/end from single term like "202510"
  skip("Term parsing utilities need verification")
})

test_that("term parsing handles term pairs", {
  # Test extracting start/end from "202510,202410"
  skip("Term parsing utilities need verification")
})

test_that("term type detection works correctly", {
  # Fall = 80, Spring = 10, Summer = 60
  skip("Term type detection needs verification")
})

test_that("term subtraction calculates correctly", {
  # 202510 - 1 year = 202410
  skip("Term arithmetic needs verification")
})

test_that("department to subject code mapping works", {
  # HIST dept should map to HIST, WMST subjects
  skip("Dept mapping needs verification")
})

test_that("major to program mapping works", {
  # "History" major should map to program code
  skip("Major mapping needs verification")
})

test_that("program to department mapping works", {
  # Program code should map to DEPT code
  skip("Program mapping needs verification")
})

test_that("exclude list is applied correctly", {
  # When uel = TRUE, certain courses should be filtered
  skip("Exclude list needs verification")
})

test_that("crosslist handling works correctly", {
  # Test 'exclude' and 'compress' options for crosslisted courses
  skip("Crosslist logic needs verification")
})
