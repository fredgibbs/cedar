# rollcall.R provides data on the major and classification of students in a course or set of courses
# no params required, but then too much data is outputted
# best to specify at least a course (or course_list) to see change over time, or also a term



# group by term, course, major, classification
# used by nosedive (not that helpful for terminal output)
get_agg_course_classification_major <- function(students) {
  
  summary <- students %>% group_by(SUBJ_CRSE, `Academic Period Code`, term_type, Major, `Student Classification`,`Short Course Title`) %>% 
    summarize(count = n()) %>% 
    arrange(desc(count),SUBJ_CRSE,Major,`Student Classification`,term_type)
  
  message("course_classification_major:")    
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


# focus on classification without major by term
get_agg_course_classification <- function(students) {
  
  summary <- students %>% group_by(`Academic Period Code`,`Student Classification`,SUBJ_CRSE,`Short Course Title`) %>% 
    summarize(count=n()) %>% 
    group_by(`Academic Period Code`, `Student Classification`) 
  
  message("course_classification:")    
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}

# focus on average classification without major across terms
get_agg_course_classification_avg <- function(students) {
  
  summary <- students %>% group_by(`Academic Period Code`,`Student Classification`,SUBJ_CRSE,`Short Course Title`) %>% 
    summarize(count=n()) %>% 
    group_by(`Academic Period Code`, `Student Classification`) 
  
  summary <- summary %>%  group_by(`Student Classification`) %>% 
    summarize(mean = mean(count))
  
  message("course_classification_avg:")    
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


get_agg_course_major <- function(students) {
  
  summary <- students %>% group_by(`Academic Period Code`,`Major`,SUBJ_CRSE,`Short Course Title`) %>% 
    summarize(count=n()) %>% 
    arrange(`Academic Period Code`, Major)
  
  message("course_major:")    
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


get_agg_course_major_avg <- function(students) {
  
  summary <- students %>% group_by(`Academic Period Code`,`Major`,SUBJ_CRSE,`Short Course Title`) %>% 
    summarize(count=n()) %>% 
    arrange(`Academic Period Code`, Major)
  
  summary <- summary %>%  group_by(`Major`) %>% 
    summarize(mean = mean(count)) %>% arrange (desc(mean))
  
  message("course_major_avg:")    
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


get_agg_major <- function(students) {
  
  # if counting majors ACROSS COURSES, keep only one record per student
  students <- students %>% distinct(`Student ID`,`Academic Period Code`, .keep_all=TRUE)
  
  # after getting only unique students, it doesn't make sense to get course info across students 
  # since students are only listed in one course at this point
  summary <- students %>% group_by(`Academic Period Code`,`Major`,SUBJ_CRSE,`Short Course Title`) %>% 
    summarize(count=n()) %>% arrange(desc(count),`Academic Period Code`)
  
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # if single term, rank by count; if multi term, rank by term
  if (nrow(distinct(students,`Academic Period Code`)) > 1) {
    summary <- students %>% group_by(`Major`,`Academic Period Code`) %>% 
      summarize(count=n()) %>% group_by(`Major`) %>%  arrange(Major,`Academic Period Code`, .by_group=TRUE) 
    summary  <- summary %>% mutate(pct_diff = (count / lag(count, order_by=`Academic Period Code`)* 100))
    
    summary <- summary %>%  filter(count > 20) %>% arrange(desc(pct_diff), .by_group=TRUE)
  } 
  else {
    summary <- students %>% group_by(`Major`,`Academic Period Code`) %>% 
      summarize(count=n()) %>% arrange(desc(count),`Academic Period Code`,Major)
  }
  
  message("major:")    
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


get_agg_major_wide <- function(students) {
  
  message("MAJOR_WIDE: summarizing by term, course, major...")
  
  # get course enrollments from class lists
  crse_enrollment  <- calc_cl_enrls(students)
  
  # count number of majors in each course
  agg_by_major <- students %>% group_by(`Academic Period Code`, SUBJ_CRSE,`Short Course Title`,`Major`) %>% 
    summarize(majors=n()) %>% 
    arrange(SUBJ_CRSE,desc(majors))# %>% 
  
  # combine raw counts of majors with enrollment data to compute students' percent of course
  merge_sum_enrl <- merge(agg_by_major,crse_enrollment,by=c("Academic Period Code", "SUBJ_CRSE"))
  merge_sum_enrl <- merge_sum_enrl %>% group_by(`Academic Period Code`,SUBJ_CRSE) %>%  mutate(pct = majors/count*100) %>% 
    select (-c(majors,count)) %>% 
    arrange (`Academic Period Code`,desc(pct))
  
  agg_by_major_w <- merge_sum_enrl %>% pivot_wider(names_from = `Academic Period Code`, values_from=pct)
  agg_by_major_w <- agg_by_major_w %>% mutate_if(is.numeric, round, digits=1)
  agg_by_major_w %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  summary <- agg_by_major_w
  
  message("major_wide:")    
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


get_agg_classification_wide <- function(students) {
  
  message("CLASSIFICATION_WIDE: summarizing by term, course, classification...")
  
  # get course enrollments from class lists (in enrl.R)
  crse_enrollment  <- calc_cl_enrls(students)
  
  agg_by_class <- students %>% group_by(`Academic Period Code`, SUBJ_CRSE,`Short Course Title`,`Student Classification`) %>% 
    summarize(class_count=n()) %>% 
    arrange(`SUBJ_CRSE`,desc(class_count)) 
  
  # merge with course enrollments to calc percent from each classification
  merge_sum_enrl <- merge(agg_by_class,crse_enrollment,by=c("Academic Period Code", "SUBJ_CRSE"))
  merge_sum_enrl <- merge_sum_enrl %>% group_by(`Academic Period Code`,SUBJ_CRSE) %>%  mutate(pct = class_count/count*100) %>% 
    select (-c(class_count,count)) %>% 
    arrange (`Academic Period Code`,desc(pct))
  
  agg_by_class_w <- merge_sum_enrl %>% pivot_wider(names_from = `Academic Period Code`, values_from=pct)
  agg_by_class_w <- agg_by_class_w %>% mutate_if(is.numeric, round, digits=1)
  agg_by_class_w %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  summary <- agg_by_class_w
  
  message("major_wide:")    
  summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}



aggregate_rolls <- function(students,opt) {
  message("aggregating the rolls...")
  
  agg_param <- opt[["aggregate"]]
  
  # ignore courses before common course numbering
  message("removing students pre 201980...")
  students$`Academic Period Code` <- as.integer(students$`Academic Period Code`)
  students <- students %>% filter (`Academic Period Code` >= 201980) 
  
  message("creating aggregate report by ", agg_param,"...")
  
  
  if (agg_param == "course_classification_major") {
    summary <- get_agg_course_classification_major(students)
  }
  else if (agg_param == "course_classification") {
    summary <- get_agg_course_classification(students)
  }
  else if (agg_param == "course_classification_avg") {
    summary <- get_agg_course_classification_avg(students)
  }  
  else if (agg_param == "course_major") {
    summary <- get_agg_course_major(students)
  }
  else if (agg_param == "course_major_avg") {
    summary <- get_agg_course_major_avg(students)
  }
  else if (agg_param == "major") {
    summary <- get_agg_major(students)
  }
  else if (agg_param == "major_wide") { # used by course-report 
    summary <- get_agg_major_wide(students)
  }
  else if (agg_param == "classification_wide") {   # used by course-report 
    summary <- get_agg_classification_wide(students)
  }
  else if (agg_param == "all") {
    summary <- get_agg_course_classification_major(students)
    summary <- get_agg_course_classification(students)
    summary <- get_agg_course_classification_avg(students)
    summary <- get_agg_course_major(students)
    summary <- get_agg_major_wide(students)
    summary <- get_agg_classification_wide(students)
  }
  else  {
    stop("opt$aggregate is invalid.")  
  }
  
  message("unique students: ",nrow(students %>%  distinct(`Student ID`, .keep_all=TRUE)))
  
  # no matter what aggregation is, return the summary
  return(summary)
  
} # end get_ags



########### main function ##########
rollcall <- function (students,opt) {
  message("Welcome to Rollcall!")
  
  if (is.null(opt[["aggregate"]])) {
    message("Everything works better with the -a (aggregate) flag: course_classification_major, course_classification, course_major, major, major_wide all.
          Defaulting to 'all', which prints a little of everything in the terminal but doesn't return any data.")
    opt[["aggregate"]] <- "all"
  }
  
  # opt <- list()
  # opt$term <- "202410"
  # opt$rollcall_agg_by <- "all"
  # #opt$dept <- ""
  # opt$course <- "MATH 1230"
  
  # opt$registration_status <- "Student Registered"
  # #opt$classification <-  'Freshman, 1st Yr, 1st Sem'
  # opt$major <- "Nursing, Pre Business Administration, Pre Psychology, Pre Biology, Undecided, Pre Mechanical Engineering, Pre FDMA, Pre Computer Science"
  
  # TODO: process a course list
  
  if(is.null(opt$registration_status)) {
    opt$registration_status <- "Registered" # this will also catch "Student Registered" per filtering functions
  }

  # always use course exclude list
  message("setting --uel flag to TRUE...")
  opt$uel <- TRUE 
  
  # filter courses according to options
  filtered_students <- filter_class_list(students, opt)
  
  # check to see if we should aggregate results
  if(!is.null(opt$aggregate)) {
    agg_rolls <- aggregate_rolls(filtered_students,opt) 
    message("returning agg_rolls from rollcall...")
    agg_rolls %>% tibble::as_tibble() %>% print(n = 13, width=Inf)
    return(agg_rolls) 
  } 
  else {
    message("not aggregating, just listing some results after filtering...")
    filtered_students %>% tibble::as_tibble() %>% print(n = 25, width=Inf)
  }
  
}