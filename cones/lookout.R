# This file contains a set of three functions that "looks out" to see where students are before, after, and during a course.
# All functions are term agnostic, so output data is averages from class list data across terms.
# REQUIRED: opt$course

# TODO: use filter class list to process opt params and allow for more specific inquiries, like term types or a specific year


# WHERE_AT, for a given course, finds OTHER courses taken by students at same time
where_at <- function (students,opt) {
  message("\n Figuring out where ELSE students are besides target course...")
  
  target_course <- opt[["course"]]
  
  # get unique ids from students in target_course
  student_list <- students %>% filter (SUBJ_CRSE == target_course ) %>% 
    select(`Course Campus Code`, `Course College Code`, `Academic Period Code`,`Student ID`) %>% distinct()
  
  # rename `Academic Period Code` to target_term
  student_list <- student_list %>%  rename (target_term = `Academic Period Code`)
  
  # merge all student data with IDs from target course to get all student data across courses
  # this provides all student data for all students enrolled in target_course for each term
  student_courses <- merge(student_list, students, by.y=c("Course Campus Code", "Course College Code", "Student ID", "Academic Period Code"), by.x=c("Course Campus Code", "Course College Code", "Student ID", "target_term"))
  student_courses <- student_courses %>% distinct()
  
  # group by term, course, and classification and sum to get total enrollment
  student_courses_summary <- student_courses %>% 
    group_by(`Course Campus Code`,`Course College Code`, target_term,SUBJ_CRSE,`Student Classification`,term_type) %>%  
    summarize (enrolled=n(), .groups="keep")
  
  # drop term, but keep term_type to compute mean number of students in each course by term type
  student_courses_summary <- student_courses_summary %>% 
    group_by(`Course Campus Code`,`Course College Code`, SUBJ_CRSE,`Student Classification`,term_type) %>% 
    mutate(enrl_from_target = mean(enrolled))
  
  # create summary table of courses and mean enrl
  courses_avgs <- student_courses_summary %>% 
    select(`Course Campus Code`,`Course College Code`, SUBJ_CRSE,enrl_from_target) %>%  
    distinct()
  
  # filter out target course
  courses_avgs <- courses_avgs %>% filter (SUBJ_CRSE != target_course)
  
  # create col to indicate target course
  courses_avgs$in_crse <- target_course
  
  #TODO: include target semester variance?
  # get enrollment for current term (via opt), avg for target course, and display delta
  
  message("returning where_AT results...")
  return(courses_avgs)
}


# for a given course, finds courses taken by students in the following semester
# this is semester agnostic, so these are averages from class list data
where_to <- function (students,opt) {
  
  target_course <- opt[["course"]]
  
  message("\n Figuring out where students go after target_course: ", target_course ,"...")
  
  if (!is.null(opt[["summer"]])) {
    incl_summer <- opt[["summer"]]
  }
  else {
    incl_summer <- FALSE
  }
  
  # TODO: filter by year or always calc averages for a course?
  # ... probably both to see if a current year is deviating from the norm
  
  # get unique ids from students in target_course
  # differentiate spring from fall term in filter
  # and so will need to keep term_type in merge and hope that works
  student_list <- students %>% filter (SUBJ_CRSE == target_course ) %>% 
    select(`Course Campus Code`,`Course College Code`, `Academic Period Code`,`Student ID`) %>% 
    distinct()
  
  student_list <- student_list %>%  rename (target_term = 1)
  
  # create and populate next_term col
  student_list <- student_list %>% add_next_term_col("target_term",summer=incl_summer)
  
  # merge students with student list to get just the student data from the course and term
  disp_merge <- merge(student_list, students, by.y=c("Course Campus Code", "Course College Code", "Student ID", "Academic Period Code"), by.x=c("Course Campus Code", "Course College Code","Student ID", "next_term"))
  disp_merge <- disp_merge %>% distinct()
  
  disp_merge_summary <- disp_merge %>% 
    group_by(`next_term`,`Course Campus Code`, `Course College Code`, SUBJ_CRSE,`Student Classification`,term_type) %>%  
    summarize (enrolled=n(), .groups="keep")
  
  disp_merge_summary <- disp_merge_summary %>% 
    group_by(`Course Campus Code`, `Course College Code`,SUBJ_CRSE,`Student Classification`,term_type) %>% 
    mutate(avg_contrib = mean(enrolled))
  
  # create summary table of courses and avg enrl
  dispersal_avgs <- disp_merge_summary %>% 
    select(`Course Campus Code`, `Course College Code`,SUBJ_CRSE,`Student Classification`,term_type,avg_contrib) %>%  
    distinct()
  
  dispersal_avgs_wo_class <- dispersal_avgs %>% 
    group_by(`Course Campus Code`, `Course College Code`,SUBJ_CRSE,term_type) %>%  
    summarize(avg_contrib = sum(avg_contrib), .groups="keep") %>% 
    arrange(desc(avg_contrib))
  
  # create col to indicate target course
  dispersal_avgs$from_crse <- target_course
  
  #TODO: as part of avg table, include target semester variance from mean
  # get enrollment for current term (via opt), avg for target course, and display delta
  
  message("returning where_TO results...")
  return(dispersal_avgs)
}


# show courses students are taking BEFORE target course, and contribution to target_course
where_from <- function (students, opt) {
  
  message("\nFiguring out where students are FROM...")
  
  target_course <- opt[["course"]]
  
  if (!is.null(opt[["summer"]])) {
    incl_summer <- opt[["summer"]]
  } else {
    incl_summer <- FALSE
  }
  
  # get list of distinct student IDs in target class
  # TODO: does registration status matter?
  target_student_list <-  students %>% filter (SUBJ_CRSE == target_course ) %>% 
    select(`Course Campus Code`, `Course College Code`, `Academic Period Code`,`Student ID`) %>% distinct()
  
  # rename APC col to target_term
  target_student_list <- target_student_list %>%  rename (target_term = `Academic Period Code`)
  
  # for each row, get the previous term code and add to DF
  target_student_list <- target_student_list %>% add_prev_term_col("target_term",summer=incl_summer)

  # get other courses taken (conduit courses) the semester prior to target course
  # merge target IDs from target_term with all student data for conduit terms
  conduit_students <- merge(target_student_list, students, by.y=c("Course Campus Code", "Course College Code", "Student ID", "Academic Period Code"), by.x=c("Course Campus Code", "Course College Code","Student ID", "prev_term"))
  
  # summarize conduit_students to see number of students in each course contributing to target course each term
  conduit_students_summary <- conduit_students %>% 
    group_by(`target_term`, `Course Campus Code`, `Course College Code`, SUBJ_CRSE, `Student Classification`,term_type) %>%  
    summarize (enrolled=n(), .groups="keep")
  
  # sum contributions across student classifications
  where_from_dispersal_avgs_wo_class <- conduit_students_summary %>% 
    group_by(target_term, `Course Campus Code`, `Course College Code`, SUBJ_CRSE,term_type) %>%  
    summarize(term_contrib = sum(enrolled), .groups="keep") %>% 
    arrange(desc(term_contrib))
  
  # get average contribution per conduit course across all terms
  # add avg_contrib column
  where_from_dispersal_avgs_wo_class <- where_from_dispersal_avgs_wo_class %>%
    group_by(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, term_type) %>%
    summarize(avg_contrib = mean(term_contrib), .groups="keep")
  
  # create col to indicate target course
  where_from_dispersal_avgs_wo_class$to_crse <- target_course
  
  message("returning where_FROM results...")
  return(where_from_dispersal_avgs_wo_class)
}


# main function for external calls
# primary work is to manage a loop to call functions above for each course specified in opt params
lookout <- function (students,opt) {
  
  # for studio testing
  # opt <- list()
  # opt$course <-"HIST 412"
  # opt$status <- "A"
  
  # check for required course param
  if (is.null(opt[["course"]])){
    stop("Required params: -c (course)
         For example: -c 'ENGL 1120'", call.=FALSE)
  }
  
  # convert opt$course into a list
  course_list <- convert_param_to_list(opt[["course"]])
  
  # create DFs storing rows for courses
  where_to_summary <- data.frame()
  where_from_summary <- data.frame()
  where_at_summary <- data.frame()
  
  # create list for final return data
  lookout_summary <- list()
  
  # loop through courses, subsetted by year
  message("about to loop through courses...")
  for (course in course_list) {
    opt$course <- course
    message("processing: ",course)
    
    ###### WHERE_TO
    to_courses <- where_to(students,opt)
    #add new DF to summary table
    where_to_summary <- rbind(where_to_summary,to_courses)
    
    
    ###### WHERE_FROM
    from_courses <- where_from(students,opt)
    #add new DF to summary table
    where_from_summary <- rbind(where_from_summary,from_courses)
    
    
    ###### WHERE_AT
    at_courses <- where_at(students,opt)
    #add new DF to summary table
    where_at_summary <- rbind(where_at_summary,at_courses)
    
  } # end loop through course list
  
  lookout_summary[["where_to"]] <- where_to_summary
  lookout_summary[["where_from"]] <- where_from_summary
  lookout_summary[["where_at"]] <- where_at_summary
  
  return (lookout_summary)
}  
