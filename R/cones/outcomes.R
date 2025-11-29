# outcomes.R provides data on student success outcomes and persistence patterns
# analyzes what happens to students after various course outcomes (pass/fail/withdraw)
# REQUIRES: opt$courses (list of courses to analyze)
# OPTIONAL: opt$term (to focus on specific terms), opt$dept, opt$campus


#' Student Outcomes Analysis
#'
#' This function analyzes student outcomes and persistence patterns after course completion.
#' Leverages existing gradebook.R infrastructure for consistent grade processing.
#'
#' @param students Data frame containing class list data with enrollment and grade information
#' @param opt List containing filtering and analysis options
#' @return List containing outcomes analysis tables and summaries
#' @export
#' @examples
#' \dontrun{
#' opt <- list(courses = c("MATH 1215", "ENGL 1110"))
#' outcomes_data <- get_outcomes(students, opt)
#' }
get_outcomes <- function(students, opt) {
  
  message("[outcomes.R] Starting student outcomes analysis...")
  
  opt <- list()
  opt[["course"]] <- c("HIST 1105")
  
  
  # Validate required parameters
  if (is.null(opt[["course"]])) {
    message("[outcomes.R] ERROR: opt$courses is required but not provided")
    return(NULL)
  }
  
  # Convert courses to list if needed (CEDAR pattern)
  courses_list <- convert_param_to_list(opt[["course"]])
  message("[outcomes.R] Analyzing outcomes for courses: ", paste(courses_list, collapse = ", "))
  
  # Use gradebook infrastructure to get comprehensive grade data
  message("[outcomes.R] Using gradebook infrastructure for grade processing...")
  grades_data <- get_grades(students, opt)
  
  if (is.null(grades_data) || length(grades_data) == 0) {
    message("[outcomes.R] No grade data available after processing")
    return(NULL)
  }
  
  # Initialize outcomes list for return data (CEDAR pattern)
  outcomes_list <- list()
  
  # Get filtered student data following gradebook pattern
  filtered_students <- filter_class_list(students, opt)
  filtered_students <- filtered_students %>% 
    filter(`Academic Period Code` >= 201980) %>%  # Match gradebook pattern
    mutate(`Final Grade` = ifelse(`Registration Status Code` == "DR", "Drop", `Final Grade`)) %>%
    distinct(`Student ID`, `Course Campus Code`, `Course College Code`, SUBJ_CRSE, 
             `Academic Period Code`, .keep_all = TRUE)
  
  # Limit to specified courses
  filtered_students <- filtered_students %>%
    filter(SUBJ_CRSE %in% courses_list)
  
  message("[outcomes.R] Filtered students data has ", nrow(filtered_students), " rows")
  
  # Analyze failure persistence using gradebook grade categories
  message("[outcomes.R] Analyzing failure persistence patterns...")
  failure_persistence <- analyze_failure_persistence_with_grades(filtered_students, grades_data)
  outcomes_list[["failure_persistence"]] <- failure_persistence
  
  # Analyze DFW patterns over time using existing grade summaries
  message("[outcomes.R] Analyzing DFW patterns over time...")
  dfw_trends <- analyze_dfw_trends(grades_data)
  outcomes_list[["dfw_trends"]] <- dfw_trends
  
  # Analyze instructor impact on outcomes
  message("[outcomes.R] Analyzing instructor impact on outcomes...")
  instructor_outcomes <- analyze_instructor_outcomes(grades_data)
  outcomes_list[["instructor_outcomes"]] <- instructor_outcomes
  
  # Analyze next term enrollment by outcome using grade categories
  message("[outcomes.R] Analyzing next term enrollment patterns...")
  next_term_enrollment <- analyze_next_term_enrollment_with_grades(filtered_students)
  outcomes_list[["next_term_enrollment"]] <- next_term_enrollment
  
  message("[outcomes.R] Outcomes analysis complete. Returning ", length(outcomes_list), " analysis tables.")
  return(outcomes_list)
}


#' Analyze Failure Persistence with Grade Data
#'
#' Uses gradebook infrastructure to track students who fail courses
#'
#' @param students Filtered student data
#' @param grades_data Grade data from get_grades()
#' @return Data frame with failure persistence analysis
analyze_failure_persistence_with_grades <- function(filtered_students, grades_data) {
  
  message("[outcomes.R] Using gradebook grade categorization for failure analysis...")
  
  # Get the course-level grade summary (includes DFW calculations)
  course_grades <- grades_data[["course"]]
  
  if (is.null(course_grades) || nrow(course_grades) == 0) {
    message("[outcomes.R] No course grade data available for persistence analysis")
    return(NULL)
  }
  
  # Get students who failed (using gradebook's passing_grades definition)
  failed_students <- filtered_students %>%
    filter(!`Final Grade` %in% passing_grades) %>%  # Use CEDAR's passing_grades list
    select(`Student ID`, SUBJ_CRSE, `Academic Period Code`, `Final Grade`, 
           `Course Campus Code`, `Course College Code`, `Student Campus Code`) 

  failed_students <- failed_students %>%
    add_next_term_col(`Academic Period Code`) %>%
    mutate(
      failed_term = `Academic Period Code`,      
    )
  
  message("[outcomes.R] Found ", nrow(failed_students), " instances of students failing courses")
  
  # Get all enrollments to check next term enrollment
  all_enrollments <- students %>%
    select(`Student ID`, `Academic Period Code`, `Student Campus Code`) %>%
    distinct()
  
  # Check if failed students enrolled in next term
  persistence_check <- failed_students %>%
    left_join(all_enrollments, 
              by = c("Student ID" = "Student ID", "next_term" = "Academic Period Code")) %>%
    mutate(
      enrolled_next_term = !is.na(`Student Campus Code.y`),
      same_campus = ifelse(enrolled_next_term, 
                          `Student Campus Code.x` == `Student Campus Code.y`, 
                          FALSE)
    )
  
  # Merge with course-level DFW data for context
  persistence_with_context <- persistence_check %>%
    group_by(SUBJ_CRSE, `Course Campus Code`, `Final Grade`) %>%
    summarize(
      total_failures = n(),
      enrolled_next_term = sum(enrolled_next_term, na.rm = TRUE),
      same_campus_next = sum(same_campus, na.rm = TRUE),
      persistence_rate = round(enrolled_next_term / total_failures * 100, 2),
      same_campus_rate = round(same_campus_next / total_failures * 100, 2),
      .groups = "drop"
    ) %>%
    left_join(course_grades %>% 
                select(SUBJ_CRSE, `Course Campus Code`, `DFW %`), 
              by = c("SUBJ_CRSE", "Course Campus Code")) %>%
    arrange(SUBJ_CRSE, `Course Campus Code`, `Final Grade`)
  
  message("[outcomes.R] Calculated persistence rates with DFW context for ", 
          nrow(persistence_with_context), " combinations")
  
  return(persistence_with_context)
}


#' Analyze DFW Trends Over Time
#'
#' Uses gradebook course_term data to show DFW trends
#'
#' @param grades_data Grade data from get_grades()
#' @return Data frame with DFW trends over time
analyze_dfw_trends <- function(grades_data) {
  
  message("[outcomes.R] Analyzing DFW trends using gradebook infrastructure...")
  
  course_term_data <- grades_data[["course_term"]]
  
  if (is.null(course_term_data) || nrow(course_term_data) == 0) {
    message("[outcomes.R] No course term data available for trend analysis")
    return(NULL)
  }
  
  # Add term year for trend analysis
  dfw_trends <- course_term_data %>%
    mutate(
      term_year = floor(`Academic Period Code` / 100),
      term_season = case_when(
        `Academic Period Code` %% 100 == 10 ~ "Spring",
        `Academic Period Code` %% 100 == 40 ~ "Summer", 
        `Academic Period Code` %% 100 == 80 ~ "Fall",
        TRUE ~ "Other"
      ),
      total_enrolled = passed + failed
    ) %>%
    filter(total_enrolled >= 10) %>%  # Only courses with meaningful enrollment
    arrange(SUBJ_CRSE, `Academic Period Code`)
  
  message("[outcomes.R] Calculated DFW trends for ", 
          length(unique(dfw_trends$SUBJ_CRSE)), " courses across ", 
          length(unique(dfw_trends$term_year)), " years")
  
  return(dfw_trends)
}


#' Analyze Instructor Impact on Outcomes
#'
#' Uses gradebook instructor-level data to compare outcomes
#'
#' @param grades_data Grade data from get_grades()
#' @return Data frame with instructor outcome analysis
analyze_instructor_outcomes <- function(grades_data) {
  
  message("[outcomes.R] Analyzing instructor impact using gradebook data...")
  
  instructor_data <- grades_data[["course_inst_avg"]]
  
  if (is.null(instructor_data) || nrow(instructor_data) == 0) {
    message("[outcomes.R] No instructor data available for outcome analysis")
    return(NULL)
  }
  
  # Analyze instructor performance relative to course averages
  course_averages <- grades_data[["course_avg"]] %>%
    select(SUBJ_CRSE, `Course Campus Code`, `Course College Code`, 
           course_avg_dfw = `DFW %`)
  
  instructor_comparison <- instructor_data %>%
    left_join(course_averages, 
              by = c("SUBJ_CRSE", "Course Campus Code", "Course College Code")) %>%
    mutate(
      dfw_difference = `DFW %` - course_avg_dfw,
      performance_category = case_when(
        dfw_difference <= -5 ~ "Significantly Better",
        dfw_difference <= -2 ~ "Better", 
        dfw_difference <= 2 ~ "Average",
        dfw_difference <= 5 ~ "Worse",
        TRUE ~ "Significantly Worse"
      )
    ) %>%
    filter(!is.na(`Primary Instructor Last Name`) & `Primary Instructor Last Name` != "") %>%
    arrange(SUBJ_CRSE, dfw_difference)
  
  message("[outcomes.R] Analyzed outcomes for ", 
          length(unique(instructor_comparison$`Primary Instructor Last Name`)), 
          " instructors")
  
  return(instructor_comparison)
}


#' Analyze Next Term Enrollment with Grade Categories
#'
#' Uses CEDAR's grade categorization for enrollment analysis
#'
#' @param students Filtered student data  
#' @return Data frame with next term enrollment analysis
analyze_next_term_enrollment_with_grades <- function(students) {
  
  message("[outcomes.R] Analyzing next term enrollment using CEDAR grade categories...")
  
  enrollment_analysis <- students %>%
    mutate(
      outcome_category = case_when(
        `Final Grade` %in% passing_grades ~ "Passed",
        `Final Grade` == "F" ~ "Failed",
        `Final Grade` == "W" ~ "Withdrew", 
        `Final Grade` == "Drop" ~ "Dropped",
        TRUE ~ "Other"
      ),
      current_term = `Academic Period Code`,
      next_term = sapply(`Academic Period Code`, get_next_term)
    ) %>%
    select(`Student ID`, SUBJ_CRSE, `Course Campus Code`, current_term, next_term, outcome_category)
  
  # Get next term enrollments for all students
  all_next_enrollments <- students %>%
    select(`Student ID`, `Academic Period Code`) %>%
    distinct() %>%
    rename(enrolled_next_term = `Academic Period Code`)
  
  # Check next term enrollment
  next_term_check <- enrollment_analysis %>%
    left_join(all_next_enrollments, 
              by = c("Student ID" = "Student ID", "next_term" = "enrolled_next_term")) %>%
    mutate(enrolled_next = !is.na(enrolled_next_term))
  
  # Summarize by outcome category
  next_term_summary <- next_term_check %>%
    group_by(SUBJ_CRSE, `Course Campus Code`, outcome_category) %>%
    summarize(
      total_students = n(),
      enrolled_next_term = sum(enrolled_next, na.rm = TRUE),
      retention_rate = round(enrolled_next_term / total_students * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(SUBJ_CRSE, `Course Campus Code`, outcome_category)
  
  message("[outcomes.R] Calculated next term enrollment rates for ", 
          nrow(next_term_summary), " course/campus/outcome combinations")
  
  return(next_term_summary)
}


#' Plot Outcomes for Course Report
#'
#' Creates visualizations leveraging gradebook infrastructure
#'
#' @param outcomes Outcomes list from get_outcomes()
#' @param opt Options list
#' @return Plotly object with outcomes visualization
plot_outcomes_for_course_report <- function(outcomes, opt) {
  
  message("[outcomes.R] Creating outcomes visualization...")
  
  if (is.null(outcomes) || length(outcomes) == 0) {
    message("[outcomes.R] No outcomes data available for plotting")
    return(NULL)
  }
  
  # Use DFW trends data for main plot (leverages gradebook infrastructure)
  if ("dfw_trends" %in% names(outcomes)) {
    
    plot_data <- outcomes[["dfw_trends"]] %>%
      filter(total_enrolled >= 10)  # Only courses with meaningful enrollment
    
    if (nrow(plot_data) == 0) {
      message("[outcomes.R] Insufficient data for visualization")
      return(NULL)
    }
    
    outcomes_plot <- plot_data %>%
      ggplot(aes(x = `Academic Period Code`, 
                 y = `DFW %`,
                 color = SUBJ_CRSE,
                 text = paste("Course:", SUBJ_CRSE,
                             "<br>Term:", `Academic Period Code`,
                             "<br>DFW%:", `DFW %`,
                             "<br>Enrolled:", total_enrolled))) +
      geom_line(alpha = 0.8, size = 1) +
      geom_point(alpha = 0.8, size = 2) +
      labs(title = "DFW Rates Over Time",
           subtitle = "Course performance trends using gradebook data",
           x = "Academic Period",
           y = "DFW Rate (%)",
           caption = "Only courses with 10+ students shown") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Convert to plotly for interactivity (CEDAR pattern)
    outcomes_plot <- ggplotly(outcomes_plot, tooltip = "text")
    
    message("[outcomes.R] Created outcomes plot with ", nrow(plot_data), " data points")
    return(outcomes_plot)
  }
  
  message("[outcomes.R] No suitable data for plotting")
  return(NULL)
}