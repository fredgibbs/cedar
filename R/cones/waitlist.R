#' Get unique waitlisted student IDs not registered for a course
#'
#' @param students Data frame of student enrollments, already filtered by opt params.
#' @param opt The usual opt list
#' @return Dataframe of counts by Campus who are waitlisted and not registered for the course.
get_unique_waitlisted <- function(filtered_students, opt) {
    
  message("welcome to get_unique_waitlisted!")

  select_cols <- c("Course Campus Code", "Academic Period Code", "SUBJ_CRSE", "Short Course Title", "Student ID")

  # Get waitlisted student IDs
  waitlisted <- filtered_students %>%
    filter(`Registration Status` == "Wait Listed") %>%
    select(all_of(select_cols)) %>%
    unique()
  
  # Get registered student IDs
  registered <- filtered_students %>%
    filter(`Registration Status` %in% c("Student Registered", "Registered")) %>%
    select(all_of(select_cols)) %>%
    unique()
  
  
  only_waitlisted <- setdiff(waitlisted, registered)

  only_waitlisted <- only_waitlisted %>%
    group_by(`Course Campus Code`, SUBJ_CRSE) %>%
    summarize(count = n(), .groups = "drop") %>%
    arrange(`Course Campus Code`, SUBJ_CRSE, desc(count))


  # Return waitlisted IDs not also registered
  message("returning waitlisted students not registered...")
  return (only_waitlisted)
}


#' Inspect Waitlist by Major and Classification
#'
#' This function summarizes the waitlist for a given set of students and options.
#' It aggregates waitlist data by major and by classification, and counts the number of
#' unique students who are waitlisted but not registered.
#'
#' @param students A data frame of student enrollments.
#' @param opt A list of options for filtering and grouping.
#' @return A list containing summaries by major, by classification, and the count of unique waitlisted students.
#' @examples
#' inspect_waitlist(students, list(course = "MATH 1430", term = 202580))
inspect_waitlist <- function (students, opt) {

  #### for studio testing
  # opt <- list()
  # opt$course <- "MATH 1430"
  # opt$term <- 202580
  
  message("welcome to inspect_waitlist!")
  
  message("filtering students from params...")
  filtered_students <- filter_class_list(students,opt) 

  # Get only waitlisted students
  filtered_students <- filtered_students  %>% filter(`Registration Status` == "Wait Listed")

  # Set groups in case multiple courses are selected
  filtered_students <- filtered_students %>%
    group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, `term_type`, 
           Major, SUBJ_CRSE, `Short Course Title`, level)

  # Create empty list for waitlist data
  waitlist_data <- list()

  # Set group_cols for Major
  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "term_type", 
                          "Major", "SUBJ_CRSE", "Short Course Title", "level")

  waitlist_data[["majors"]] <- summarize_classifications(filtered_students, opt) %>% 
    ungroup() %>% 
    select (-c(`Course College Code`, level, term_type, mean, registered, registered_mean, pct)) %>% 
    arrange (`Course Campus Code`, desc(count))


# Set group_cols for Classification
  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "term_type", 
                          "Student Classification", "SUBJ_CRSE", "Short Course Title", "level")

  waitlist_data[["classifications"]] <- summarize_classifications(filtered_students, opt) %>% 
    ungroup() %>% 
    select (-c(`Course College Code`, level, term_type, mean, registered, registered_mean, pct)) %>% 
    arrange (`Course Campus Code`, desc(count))

  waitlist_data[["count"]] <- get_unique_waitlisted(filtered_students, opt)
  
  message("returning waitlist data...")
  
  return(waitlist_data)
}