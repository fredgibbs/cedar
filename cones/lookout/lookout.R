# This file contains a set of three functions that "looks out" to see where students are before, after, and during a course.
# All functions are term agnostic, so output data is averages from class list data across terms.
# REQUIRED: opt$course

# TODO: use filter class list to process opt params and allow for more specific inquiries, like term types or a specific year


# WHERE_AT, for a given course, finds OTHER courses taken by students at same time
where_at <- function (students,opt) {
  message("figuring out where ELSE students are besides target course...")
  
  target_course <- opt[["course"]]
  
  # get unique ids from students in target_course
  student_list <- students %>% filter (SUBJ_CRSE == target_course ) %>% 
    select(`Academic Period Code`,`Student ID`) %>% distinct()
  
  # rename `Academic Period Code` to target_term
  student_list <- student_list %>%  rename (target_term = 1)
  
  # merge all student data with IDs from target course to get all student data across courses
  # this provides all student data for all students enrolled in target_course for each term
  student_courses <- merge(student_list, students, by.y=c("Student ID", "Academic Period Code"), by.x=c("Student ID", "target_term"))
  student_courses <- student_courses %>% distinct()
  
  # group by term, course, and classification and sum to get total enrollment
  student_courses_summary <- student_courses %>% 
    group_by(target_term,SUBJ_CRSE,`Student Classification`,term_type) %>%  
    summarize (enrolled=n())
  
  # drop term, but keep term_type to compute mean number of students in each course by term type
  student_courses_summary <- student_courses_summary %>% 
    group_by(SUBJ_CRSE,`Student Classification`,term_type) %>% 
    mutate(enrl_from_target = mean(enrolled))
  
  # create summary table of courses and mean enrl
  courses_avgs <- student_courses_summary %>% 
    select(SUBJ_CRSE,enrl_from_target) %>%  
    distinct()

  # filter out target course
  courses_avgs <- courses_avgs %>% filter (SUBJ_CRSE != target_course)

  # create col to indicate target course
  courses_avgs$in_crse <- target_course
  
  #TODO: as part of avg table, include target semester variance
  # get enrollment for current term (via opt), avg for target course, and display delta
  
  message("returning where_AT results...")
  return(courses_avgs)
}


# for a given course, finds courses taken by students in the following semester
# this is semester agnostic, so these are averages from class list data
where_to <- function (students,opt) {
  message("figuring out where students go after target_course...")
  
  target_course <- opt[["course"]]
  
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
    select(`Academic Period Code`,`Student ID`) %>% distinct()
  
  student_list <- student_list %>%  rename (target_term = 1)
  
  # create and populate next_term col
  next_terms <- lapply(student_list$target_term,add_term,summer=incl_summer)
  student_list$next_term <- next_terms
  
  # student_list  <- student_list %>% mutate (next_term = add_term(target_term, TRUE))
  # can i get the same thing by student_list %>% mutate (next_term = add_term(target_term))
  
  # merge students with student list to get just the student data from the course and term
  disp_merge <- merge(student_list, students, by.y=c("Student ID", "Academic Period Code"), by.x=c("Student ID", "next_term"))
  disp_merge <- disp_merge %>% distinct()
  
  disp_merge_summary <- disp_merge %>% 
    group_by(`next_term`,SUBJ_CRSE,`Student Classification`,term_type) %>%  
    summarize (enrolled=n())
  
  disp_merge_summary <- disp_merge_summary %>% 
    group_by(SUBJ_CRSE,`Student Classification`,term_type) %>% 
    mutate(avg_contrib = mean(enrolled))
  
  # create summary table of courses and avg enrl
  dispersal_avgs <- disp_merge_summary %>% 
    select(SUBJ_CRSE,`Student Classification`,term_type,avg_contrib) %>%  
    distinct()
  
  
  message("course contributions w/o classifications")
  dispersal_avgs_wo_class <- dispersal_avgs %>% 
    group_by(SUBJ_CRSE,term_type) %>%  
    summarize(avg_contrib = sum(avg_contrib)) %>% 
    arrange(desc(avg_contrib))
  
  dispersal_avgs_wo_class %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # create col to indicate target course
  dispersal_avgs$from_crse <- target_course
  
  #TODO: as part of avg table, include target semester variance from mean
  # get enrollment for current term (via opt), avg for target course, and display delta

  message("returning where_TO results...")
  return(dispersal_avgs)
}

  
# show courses students are taking BEFORE target course, and contribution to target_course
where_from <- function (students,opt) {
  
  message("figuring out where students are from...")
  
  target_course <- opt[["course"]]
  
  if (!is.null(opt[["summer"]])) {
    incl_summer <- opt[["summer"]]
  } else {
    incl_summer <- FALSE
  }
  
  # get list of student ID in target class
  target_student_list <-  students %>% filter (SUBJ_CRSE == target_course ) %>% 
    select(`Academic Period Code`,`Student ID`) %>% distinct()
  
  # rename APC col to target_term
  target_student_list <- target_student_list %>%  rename (target_term =`Academic Period Code`)
  
  # for each row, get the previous term code and add to DF
  prev_terms <- lapply(target_student_list$target_term,subtract_term,summer=incl_summer)
  target_student_list$prev_term = prev_terms
  
  # now target_student_list looks like:
  # target_term Student ID prev_term
  # 202010 101821300 201980
  
  # get other courses taken (conduit courses) the semester prior to target course
  # merge target IDs from target_term with all student data for conduit terms
  conduit_students <- merge(target_student_list, students, by.y=c("Student ID", "Academic Period Code"), by.x=c("Student ID", "prev_term"))
  
  # summarize conduit_students to see numbers of students in each course contributing to target course each term
  # add enrolled column
  conduit_students_summary <- conduit_students %>% 
    group_by(`target_term`,SUBJ_CRSE,`Student Classification`,term_type) %>%  
    summarize (enrolled=n())
  
  # sum contributions across student classifications
  message("course contributions w/o classifications:")
  where_from_dispersal_avgs_wo_class <- conduit_students_summary %>% 
    group_by(target_term,SUBJ_CRSE,term_type) %>%  
    summarize(term_contrib = sum(enrolled)) %>% 
    arrange(desc(term_contrib))
  
  # get average contribution per conduit course across all terms
  # add avg_contrib column
  where_from_dispersal_avgs_wo_class <- where_from_dispersal_avgs_wo_class %>%
    group_by(SUBJ_CRSE,term_type) %>%
    summarize(avg_contrib = mean(term_contrib, .groups="keep"))

  where_from_dispersal_avgs_wo_class %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # create col to indicate target course
  where_from_dispersal_avgs_wo_class$to_crse <- target_course
  
  message("returning where_FROM results...")
  return(where_from_dispersal_avgs_wo_class)
}
  

# main body
# check for required course param
# manage a loop to call calc functions above for each course specified (via comma string, named list, etc)
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
  
  # create lists for storing DFs
  where_to_summary <- data.frame()
  where_from_summary <- data.frame()
  where_at_summary <- data.frame()
  
  # loop through courses, subsetted by year
  message("about to loop through courses...")
  for (course in course_list) {
    opt$course <- course
    
    message("processing: ",course)
    
    ###### WHERE_TO
    to_courses <- where_to(students,opt)
    to_courses %>% arrange(desc(avg_contrib)) %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
    
    #add new DF to summary table
    where_to_summary <- rbind(where_to_summary,to_courses)
    
    ###### WHERE_FROM
    from_courses <- where_from(students,opt)
    from_courses %>% arrange(desc(avg_contrib)) %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
    
    #add new DF to summary table
    where_from_summary <- rbind(where_from_summary,from_courses)
    
    
    ###### WHERE_AT
    at_courses <- where_at(students,opt)
    at_courses %>% arrange(desc(enrl_from_target)) %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
    
    where_at_summary <- rbind(where_at_summary,at_courses)
  } # end loop through course list
  
}  
