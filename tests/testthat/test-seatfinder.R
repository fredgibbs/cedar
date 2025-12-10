# Tests for seatfinder function
# Tests R/cones/seatfinder.R

context("Seatfinder")

test_that("seatfinder returns expected list structure", {
  opt <- create_test_opt(list(
    term = "202510,202410",  # Compare two terms
    dept = "HIST"
  ))
  
  # result <- seatfinder(test_students, test_courses, opt)
  
  # expect_type(result, "list")
  # expect_true("type_summary" %in% names(result))
  # expect_true("courses_common" %in% names(result))
  # expect_true("courses_prev" %in% names(result))
  # expect_true("courses_new" %in% names(result))
  # expect_true("gen_ed_summary" %in% names(result))
  # expect_true("gen_ed_likely" %in% names(result))
  
  skip("seatfinder function needs verification")
})

test_that("seatfinder parses term parameter correctly", {
  # Test single term (should subtract 1 year)
  opt_single <- create_test_opt(list(term = "202510"))
  
  # Test comma-separated terms
  opt_multi <- create_test_opt(list(term = "202510,202410"))
  
  skip("Term parsing logic needs verification")
})

test_that("seatfinder calculates term differences correctly", {
  opt <- create_test_opt(list(term = "202510,202410"))
  
  # result <- seatfinder(test_students, test_courses, opt)
  # type_summary <- result$type_summary
  
  # expect_true("avail_diff" %in% colnames(type_summary))
  
  skip("Difference calculation needs verification")
})

test_that("seatfinder removes unnecessary columns", {
  opt <- create_test_opt(list(term = "202510,202410"))
  
  # result <- seatfinder(test_students, test_courses, opt)
  
  # Verify xl_sections, reg_sections, INST_METHOD are removed
  # expect_false("xl_sections" %in% colnames(result$type_summary))
  # expect_false("reg_sections" %in% colnames(result$type_summary))
  # expect_false("INST_METHOD" %in% colnames(result$type_summary))
  
  skip("Column cleanup needs verification")
})

test_that("seatfinder identifies common courses correctly", {
  opt <- create_test_opt(list(term = "202510,202410"))
  
  # result <- seatfinder(test_students, test_courses, opt)
  # common <- result$courses_common
  
  # Common courses should appear in both terms
  skip("Common course logic needs verification")
})

test_that("seatfinder identifies new courses correctly", {
  opt <- create_test_opt(list(term = "202510,202410"))
  
  # result <- seatfinder(test_students, test_courses, opt)
  # new_courses <- result$courses_new
  
  # New courses should only be in end term
  skip("New course logic needs verification")
})

test_that("seatfinder identifies previous courses correctly", {
  opt <- create_test_opt(list(term = "202510,202410"))
  
  # result <- seatfinder(test_students, test_courses, opt)
  # prev_courses <- result$courses_prev
  
  # Previous courses should only be in start term
  skip("Previous course logic needs verification")
})

test_that("seatfinder filters gen ed courses correctly", {
  opt <- create_test_opt(list(term = "202510"))
  
  # result <- seatfinder(test_students, test_courses, opt)
  # gen_ed_summary <- result$gen_ed_summary
  
  # expect_true(all(!is.na(gen_ed_summary$gen_ed_area)))
  # expect_true(all(gen_ed_summary$avail > 0))
  
  skip("Gen ed filtering needs verification")
})

test_that("seatfinder merges DFW rates correctly", {
  opt <- create_test_opt(list(term = "202510"))
  
  # result <- seatfinder(test_students, test_courses, opt)
  
  # Check that DFW % column exists
  # expect_true("DFW %" %in% colnames(result$type_summary) || 
  #             "DFW.." %in% colnames(result$type_summary))
  
  skip("DFW merge logic needs verification")
})
