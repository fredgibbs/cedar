# Tests for headcount functions
# Tests R/cones/headcount.R

context("Headcount Analysis")

test_that("count_heads returns correct structure", {
  opt <- create_test_opt()
  
  # result <- count_heads(test_academic_studies, opt)
  
  # expect_s3_class(result, "data.frame")
  # expect_true("students" %in% colnames(result))
  # expect_true("major_name" %in% colnames(result))
  # expect_true("DEPT" %in% colnames(result))
  
  skip("count_heads function needs verification")
})

test_that("count_heads pivots to long format correctly", {
  opt <- create_test_opt()
  
  # Test that Major, Second Major, First Minor, Second Minor are pivoted
  skip("Pivot logic needs verification")
})

test_that("count_heads maps majors to programs correctly", {
  opt <- create_test_opt()
  
  # result <- count_heads(test_academic_studies, opt)
  # expect_true("PRGM" %in% colnames(result))
  
  skip("Major mapping needs verification")
})

test_that("count_heads maps programs to departments correctly", {
  opt <- create_test_opt()
  
  # result <- count_heads(test_academic_studies, opt)
  # expect_true("DEPT" %in% colnames(result))
  
  skip("Program to dept mapping needs verification")
})

test_that("count_heads filters by department correctly", {
  opt <- create_test_opt(list(dept = "HIST"))
  
  skip("Department filtering needs verification")
})

test_that("count_heads filters by term correctly", {
  opt <- create_test_opt(list(term = 202510))
  
  skip("Term filtering needs verification")
})

test_that("count_heads aggregates by student level", {
  opt <- create_test_opt()
  
  # result <- count_heads(test_academic_studies, opt)
  # expect_true("Student Level" %in% colnames(result))
  
  skip("Level aggregation needs verification")
})

test_that("count_heads handles multiple majors/minors", {
  # Student 003 has both Major and Second Major
  # Student 002 has both Major and First Minor
  
  skip("Multiple major/minor handling needs verification")
})
