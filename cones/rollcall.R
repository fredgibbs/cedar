# rollcall.R provides data on the major and classification of students in a course or set of courses
# no params required, but then too much data is outputted
# best to specify at least a course (or course_list) to see change over time, and/or a term


#' Summarize Classifications
#'
#' This function summarizes student classifications and majors based on specified grouping columns.
#' It calculates distinct counts, means, and percentages of enrollments.
#'
#' @param filtered_students A data frame of filtered student data.
#' @param opt A list of options, including `group_cols` for grouping columns.
#' @return A data frame summarizing student classifications and their percentages of course enrollments.
#' @examples
#' opt <- list(group_cols = c("Course Campus Code", "Major"))
#' summarize_classifications(filtered_students, opt)
summarize_classifications <- function(filtered_students, opt) {
  message("Summarizing students with group_cols...")

  group_cols <- opt[["group_cols"]]

  # Set default group_cols if not provided
  if (is.null(group_cols)) {
    group_cols <- c("Course Campus Code", "Course College Code", "Academic Period Code", "term_type", 
                    "Major", "Student Classification", "SUBJ_CRSE", "Short Course Title", "level")
  } else {
    group_cols <- convert_param_to_list(group_cols)
    group_cols <- as.character(group_cols)
  }

  # Group and count distinct students
  # TODO: this is similar to calc_cl_enrls but with Major and Classification
  # TODO: should probably handle those in that function instead of here
  summary <- filtered_students %>%
    group_by_at(group_cols) %>%
    distinct(`Student ID`, .keep_all = TRUE) %>%
    summarize(.groups = "keep", count = n())

  # Regroup without "Academic Period Code" to calculate means
  group_cols <- group_cols[-which(group_cols %in% c("Academic Period Code"))]

  # Calculate mean of counts of students by group_cols across all terms
  summary <- summary %>%
    group_by_at(group_cols) %>%
    mutate(mean = round(mean(count), digits = 1))

  # Count course enrollments and percentages
  reg_summary <- calc_cl_enrls(filtered_students)

  # Ungroup for merging and select relevant columns
  crse_enrollment <- reg_summary %>%
    ungroup() %>%
    select(c(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, `Academic Period Code`, registered, registered_mean))

  # Merge summary of majors and classifications with course enrollment data
  merge_sum_enrl <- merge(summary, crse_enrollment, by = c("Course Campus Code", "Course College Code", 
                                                           "Academic Period Code", "SUBJ_CRSE"))
  # Regroup and calculate percentages
  merge_sum_enrl <- merge_sum_enrl %>%
    group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE) %>%
    mutate(pct = round(count / registered * 100, digits = 1)) %>%
    select(-c(count, registered, registered_mean)) %>%
    arrange(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE, desc(pct))

  return(merge_sum_enrl)
}

#' Rollcall: Main Function
#'
#' This is the main function for processing student data. It filters students, removes pre-201980 data,
#' and aggregates student classifications and majors.
#'
#' @param students A dataframe of aggregated MyReports Class List Reports
#' @param opt A list of options for filtering and grouping, including `group_cols`, `term`, and `course`.
#' @return A summarized dataframe of student classifications and majors.
#' @examples
#' opt <- list(group_cols = c("Major", "Student Classification"), term = 202110)
#' rollcall(students, opt)
rollcall <- function(students, opt) {
  message("\nWelcome to Rollcall!")

  # for studio testing...
  # opt <- list()
  # opt$term <- NULL
  # opt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", 
  #                          "Student Classification", "SUBJ_CRSE","Short Course Title","level")
  # opt$course <- "BIOL 2305"

  # Set default group_cols if not provided
  if (is.null(opt[["group_cols"]])) {
    message("Everything works better with the group_cols param. Grouping with Student Classification and Major...")
    opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "term_type", 
                             "Student Classification", "SUBJ_CRSE", "Short Course Title", "level")
  }

  if (is.null(opt[["registration_status_code"]])) {
    message("No registration status code provided; defaulting to Registered (RE and RS) students only...")
    opt[["registration_status_code"]] <- c("RE", "RS")
  }
  
  
  # Set the "use exclude list" flag
  message("Setting --uel flag to TRUE...")
  opt$uel <- TRUE

  # Filter students based on options
  filtered_students <- filter_class_list(students, opt)

  # Remove students from terms before 201980
  message("Removing students pre-201980...")
  students$`Academic Period Code` <- as.integer(students$`Academic Period Code`)
  filtered_students <- filtered_students %>% filter(`Academic Period Code` >= 201980)

  # Summarize classifications
  message("Aggregating the student majors/classifications...")
  summary <- summarize_classifications(filtered_students, opt)

  message("Returning summary from rollcall...")
  return(summary)
}