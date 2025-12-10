# Tests for forecasting functions
# Tests R/cones/forecast/forecast.R and forecast-stats.R

context("Enrollment Forecasting")

test_that("forecast returns correct structure", {
  opt <- create_test_opt(list(
    course = "HIST 1110",
    term = 202510
  ))
  
  # result <- forecast(test_students, test_courses, opt)
  
  # expect_s3_class(result, "data.frame")
  # expect_true("rec_enrl_sections" %in% colnames(result))
  # expect_true("rec_cl_sections" %in% colnames(result))
  
  skip("forecast function needs verification")
})

test_that("forecast calculates conduit enrollment correctly", {
  # Conduit = students who took prerequisite course
  skip("Conduit calculation needs verification")
})

test_that("forecast calculates major enrollment correctly", {
  # Major = students in relevant degree programs
  skip("Major enrollment needs verification")
})

test_that("forecast compares multiple accuracy methods", {
  # Should compare: c_enrl_accr, c_dr_enrl_accr, c_cl_accr, c_dr_cl_accr, 
  #                 m_enrl_accr, m_dr_enrl_accr, m_cl_accr, m_dr_cl_accr
  skip("Accuracy comparison needs verification")
})

test_that("forecast selects preferred method automatically", {
  # pref_enrl_method and pref_cl_method should be determined
  skip("Method selection needs verification")
})

test_that("forecast handles missing prerequisite data", {
  opt <- create_test_opt(list(
    course = "HIST 2110",
    term = 202510,
    forecast_conduit_term = NULL
  ))
  
  skip("Missing data handling needs verification")
})

test_that("forecast accuracy stats are calculated correctly", {
  # Test avg_enrl_accr, avg_cl_accr, etc.
  skip("Accuracy stats need verification")
})

test_that("forecast recommends section counts", {
  # rec_enrl_sections and rec_cl_sections should be integers
  skip("Section recommendation needs verification")
})
