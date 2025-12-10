# Test setup - runs before all tests
# Create minimal test data fixtures

# Load necessary libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Create sample test data that mimics real CEDAR data structures
create_test_students <- function() {
  tibble::tibble(
    ID = c("001", "002", "003", "004", "005"),
    `Academic Period Code` = c(202510, 202510, 202510, 202580, 202580),
    `Course Campus Code` = c("ABQ", "ABQ", "ABQ", "ABQ", "TAO"),
    `Course College Code` = c("AS", "AS", "EN", "AS", "AS"),
    SUBJ_CRSE = c("HIST 1110", "HIST 1110", "MATH 1220", "HIST 2110", "ANTH 1130"),
    `Short Course Title` = c("Western Civ I", "Western Civ I", "Calc I", "Western Civ II", "Intro Anthro"),
    `Student Classification` = c("Freshman", "Sophomore", "Freshman", "Junior", "Senior"),
    Major = c("History", "History", "Engineering", "Political Science", "Anthropology"),
    `Second Major` = c(NA, NA, "Computer Science", NA, NA),
    `First Minor` = c(NA, "Anthropology", NA, "History", NA),
    `Student Campus` = c("Main", "Main", "Main", "Main", "Branch"),
    `Student Level` = c("Undergraduate", "Undergraduate", "Undergraduate", "Undergraduate", "Graduate/GASM"),
    `Registration Status Code` = c("RE", "RE", "RE", "RE", "RE"),
    `Final Grade` = c("A", "B", "C", "A-", "B+"),
    `Primary Instructor Last Name` = c("Smith", "Smith", "Johnson", "Williams", "Brown")
  )
}

create_test_courses <- function() {
  tibble::tibble(
    TERM = c(202510, 202510, 202510, 202580, 202580),
    CAMP = c("ABQ", "ABQ", "ABQ", "ABQ", "TAO"),
    COLLEGE = c("AS", "AS", "EN", "AS", "AS"),
    SUBJ_CRSE = c("HIST 1110", "HIST 1110", "MATH 1220", "HIST 2110", "ANTH 1130"),
    CRSE_TITLE = c("Western Civ I", "Western Civ I", "Calc I", "Western Civ II", "Intro Anthro"),
    CRN = c(12345, 12346, 12347, 12348, 12349),
    SECT = c("001", "002", "001", "001", "001"),
    PT = c("1H", "1H", "1H", "2H", "1H"),
    INST_METHOD = c("EA", "EA", "OL", "EA", "EA"),
    enrolled = c(25, 30, 40, 20, 15),
    cap = c(30, 35, 45, 25, 20),
    avail = c(5, 5, 5, 5, 5),
    wait = c(0, 2, 0, 0, 0),
    status = c("A", "A", "A", "A", "A"),
    level = c("LD", "LD", "LD", "UD", "LD"),
    gen_ed_area = c("HUM", "HUM", NA, "HUM", "SS"),
    xl_sections = c(0, 0, 0, 0, 0),
    reg_sections = c(1, 1, 1, 1, 1)
  )
}

create_test_academic_studies <- function() {
  tibble::tibble(
    ID = c("001", "002", "003", "004", "005"),
    term_code = c(202510, 202510, 202510, 202580, 202580),
    `Actual College` = c("AS", "AS", "EN", "AS", "AS"),
    `Translated College` = c("AS", "AS", "EN", "AS", "AS"),
    `Student Campus` = c("Main", "Main", "Main", "Main", "Branch"),
    `Student Level` = c("Undergraduate", "Undergraduate", "Undergraduate", "Undergraduate", "Graduate/GASM"),
    Degree = c("BA", "BA", "BS", "BA", "MA"),
    Major = c("History", "History", "Engineering", "Political Science", "Anthropology"),
    `Second Major` = c(NA, NA, "Computer Science", NA, NA),
    `First Minor` = c(NA, "Anthropology", NA, "History", NA),
    `Second Minor` = c(NA, NA, NA, NA, NA),
    `First Concentration` = c(NA, NA, NA, NA, NA),
    `Second Concentration` = c(NA, NA, NA, NA, NA),
    `Third Concentration` = c(NA, NA, NA, NA, NA),
    DEPT = c("HIST", "HIST", "ECE", "POLS", "ANTH")
  )
}

create_test_degrees <- function() {
  tibble::tibble(
    ID = c("001", "002", "003"),
    `Academic Period` = c("Fall 2024", "Spring 2024", "Fall 2023"),
    term_code = c(202480, 202410, 202380),
    `Degree College` = c("AS", "AS", "EN"),
    `Degree` = c("BA", "BA", "BS"),
    `Primary Major` = c("History", "Anthropology", "Engineering"),
    `Primary Major Code` = c("HIST", "ANTH", "ECE"),
    DEPT = c("HIST", "ANTH", "ECE")
  )
}

# Store fixtures in test environment
test_students <<- create_test_students()
test_courses <<- create_test_courses()
test_academic_studies <<- create_test_academic_studies()
test_degrees <<- create_test_degrees()

# Helper function to create standard opt list
create_test_opt <- function(overrides = list()) {
  default_opt <- list(
    course_campus = NULL,
    course_college = NULL,
    dept = NULL,
    term = NULL,
    pt = NULL,
    im = NULL,
    level = NULL,
    status = "A",
    uel = TRUE,
    group_cols = NULL
  )
  
  modifyList(default_opt, overrides)
}
