# rollcall.R provides data on the major and classification of students in a course or set of courses
# no params required, but then too much data is outputted
# best to specify at least a course (or course_list) to see change over time, or also a term



# group by term, course, major, classification
# used by nosedive (not that helpful for terminal output)
get_agg_course_classification_major <- function(students) {
  
  summary <- students %>% group_by(SUBJ_CRSE, `Academic Period Code`, term_type, Major, `Student Classification`,`Short Course Title`) %>% 
    distinct(`Student ID`,.keep_all=TRUE) %>% 
    summarize(count = n()) %>% 
    arrange(desc(count),SUBJ_CRSE,Major,`Student Classification`,term_type)
  
  # message("course_classification_major:")    
  # summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


# focus on classification without major by term
get_agg_course_classification <- function(students) {
  
  summary <- students %>% group_by(`Academic Period Code`,`Student Classification`,SUBJ_CRSE,`Short Course Title`) %>% 
    distinct(`Student ID`,.keep_all=TRUE) %>% 
    summarize(count=n()) %>% 
    group_by(`Academic Period Code`, `Student Classification`) 
  
  # message("course_classification:")    
  # summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}

# focus on average classification without major across terms
get_agg_course_classification_avg <- function(students) {
  
  summary <- students %>% group_by(`Academic Period Code`,`Student Classification`,SUBJ_CRSE,`Short Course Title`) %>% 
    distinct(`Student ID`,.keep_all=TRUE) %>% 
    summarize(count=n()) %>% 
    group_by(`Academic Period Code`, `Student Classification`) 
  
  summary <- summary %>%  group_by(`Student Classification`,SUBJ_CRSE,`Short Course Title`) %>% 
    summarize(mean = mean(count))
  
  #message("course_classification_avg:")    
  #summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}



get_agg_classification_wide <- function(students) {
  
  message("CLASSIFICATION_WIDE: summarizing by term, course, classification...")
  
  # get course enrollments from class lists (in enrl.R)
  reg_summary <- calc_cl_enrls(students)
  crse_enrollment <- reg_summary %>% ungroup() %>% select(c(SUBJ_CRSE,`Academic Period Code`,registered))
  
  
  
  agg_by_class <- students %>% group_by(`Academic Period Code`, SUBJ_CRSE,`Short Course Title`,`Student Classification`) %>%
    distinct(`Student ID`,.keep_all=TRUE) %>% 
    summarize(class_count=n()) %>% 
    arrange(`SUBJ_CRSE`,desc(class_count)) 
  
  # merge with course enrollments to calc percent from each classification
  merge_sum_enrl <- merge(agg_by_class,crse_enrollment,by=c("Academic Period Code", "SUBJ_CRSE"))
  merge_sum_enrl <- merge_sum_enrl %>% group_by(`Academic Period Code`,SUBJ_CRSE) %>%  mutate(pct = class_count/registered*100) %>% 
    select (-c(class_count,registered)) %>% 
    arrange (`Academic Period Code`,desc(pct))
  
  agg_by_class_w <- merge_sum_enrl %>% pivot_wider(names_from = `Academic Period Code`, values_from=pct)
  agg_by_class_w <- agg_by_class_w %>% mutate_if(is.numeric, round, digits=1)
  agg_by_class_w %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  summary <- agg_by_class_w
  
  #message("major_wide:")    
  #summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}





get_agg_course_major <- function(students) {
  
  summary <- students %>% group_by(`Academic Period Code`,`Major`,SUBJ_CRSE,`Short Course Title`) %>% 
    summarize(count=n()) %>% 
    arrange(`Academic Period Code`, Major)
  
  #message("course_major:")    
  #summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


get_agg_course_major_avg <- function(students) {
  
  summary <- students %>% group_by(`Academic Period Code`,`Major`,SUBJ_CRSE,`Short Course Title`) %>% 
    summarize(count=n()) %>% 
    arrange(`Academic Period Code`, Major)
  
  summary <- summary %>%  group_by(`Major`) %>% 
    summarize(mean = mean(count)) %>% arrange (desc(mean))
  
  #message("course_major_avg:")    
  #summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(summary)    
}


get_agg_major <- function(students) {
  
  message("AGG_MAJOR: summarizing by term, course, major...")

  # get course enrollments from class lists (gets only registered students)
  reg_summary <- calc_cl_enrls(students)
  crse_enrollment <- reg_summary %>% ungroup() %>% select(c(SUBJ_CRSE,`Academic Period Code`,registered))
  
  
  # count number of majors in each course
  agg_by_major <- students %>% group_by(`Academic Period Code`, SUBJ_CRSE, `Major`) %>% 
    distinct(`Student ID`,.keep_all=TRUE) %>% 
    summarize(majors=n()) %>% 
    arrange(SUBJ_CRSE,desc(majors))
  
  # combine raw counts of majors with enrollment data to compute students' percent of course
  agg_by_major <- merge(agg_by_major,crse_enrollment,by=c("Academic Period Code", "SUBJ_CRSE"))
  
  # calculate % of enrollment for different majors
  agg_by_major <- agg_by_major %>% group_by(`Academic Period Code`,SUBJ_CRSE) %>%  mutate(pct = majors/registered*100) %>% 
    select (-c(majors,registered)) %>% 
    arrange (`Academic Period Code`,desc(pct))
  
  return(agg_by_major)    
}


get_agg_major_wide <- function(students) {
  
  message("MAJOR_WIDE: summarizing by term, course, major...")
  
  agg_major <- get_agg_major(students)
  
  agg_by_major_w <- agg_major %>% pivot_wider(names_from = `Academic Period Code`, values_from=pct)
  agg_by_major_w <- agg_by_major_w %>% mutate_if(is.numeric, round, digits=1)
  agg_by_major_w %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  #message("major_wide:")    
  #agg_by_major_w %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(agg_by_major_w)    
}


get_agg_major_woc <- function(students) {
  
  message("MAJOR_WIDE: summarizing by term, major...")
  
  # if counting majors ACROSS COURSES, use distinct IDs keep only one record per student
  students <- students %>% distinct(`Student ID`,`Academic Period Code`, .keep_all=TRUE)
  
  # count number of majors 
  agg_major_woc <- students %>% group_by(`Academic Period Code`, term_type, `Major`) %>%
    summarize(majors=n()) %>%
    arrange(term_type,desc(`Academic Period Code`))# %>%
  
  agg_major_woc <- agg_major_woc %>% mutate_if(is.numeric, round, digits=1)
  
  #message("agg_major_woc:")
  #agg_major_woc %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # 1                 202480 fall      Accounting                     93
  # 2                 202480 fall      Adv Magnetic Reson Imaging      1
  # 3                 202480 fall      American Studies               22
  # 4                 202480 fall      Anesthesia                     15
  # 5                 202480 fall      Anthropology                  131
  # 
  
  # try to summarize across terms to get trends or dimps (dips and bumps)
  options(scipen=999)
  
  agg_major_woc <- add_term_bins(agg_major_woc,"Academic Period Code")
  
  stats <- agg_major_woc %>% group_by(Major) %>% group_modify(~ broom::tidy(lm(majors ~ term_bin, data = .x)))
  stats <- stats  %>% filter (term == "term_bin") %>% select(Major, estimate) %>%  arrange (desc(estimate))
  stats %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  
  return(agg_major_woc)
}




get_agg_major_woc_wide <- function(students) {

  message("MAJOR_WIDE: summarizing by term, major...")

  # use distinct IDs
  students <- students %>% distinct(`Student ID`,`Academic Period Code`, .keep_all=TRUE)
  
  # count number of majors in each course
  agg_by_major <- students %>% group_by(`Academic Period Code`, term_type, `Major`) %>%
    summarize(majors=n()) %>%
    arrange(term_type,desc(`Academic Period Code`))# %>%

  
  agg_by_major_w <- agg_by_major %>% pivot_wider(names_from = `Academic Period Code`, values_from=majors)
  agg_by_major_w <- agg_by_major_w %>% mutate_if(is.numeric, round, digits=1)

  message("major_wide:")
  agg_by_major_w %>% tibble::as_tibble() %>% print(n = 20, width=Inf)

  return(agg_by_major_w)
}




aggregate_rolls <- function(students,opt) {
  message("aggregating the rolls...")
  
  agg_param <- opt[["aggregate"]]
  
  # ignore courses before common course numbering
  message("removing students pre 201980...")
  students$`Academic Period Code` <- as.integer(students$`Academic Period Code`)
  students <- students %>% filter (`Academic Period Code` >= 201980) 
  
  message("creating aggregate report by ", agg_param,"...")
  
  
  # in general, wide versions display academic years as columns
  
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
  else if (agg_param == "major_woc") { 
    summary <- get_agg_major_woc(students)
  }
  else if (agg_param == "major_woc_wide") { 
    summary <- get_agg_major_woc_wide(students)
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
  message("\n Welcome to Rollcall!")
  
  if (is.null(opt[["aggregate"]])) {
    message("Everything works better with the -a (aggregate) flag: course_classification_major, course_classification, course_major, major, major_wide all.
          Defaulting to 'all', which prints a little of everything in the terminal but doesn't return any data.")
    opt[["aggregate"]] <- "all"
  }
  
  # opt <- list()
  # opt$term <- NULL
  # opt$aggregate <- "major_wide"
  # opt$course <- "ENGL 2210"
  # 
  # students <- load_students(opt)
  
  # opt$registration_status <- "Student Registered"
  # #opt$classification <-  'Freshman, 1st Yr, 1st Sem'
  # opt$major <- "Nursing, Pre Business Administration, Pre Psychology, Pre Biology, Undecided, Pre Mechanical Engineering, Pre FDMA, Pre Computer Science"
  
  
  # TODO: process a course list
  
  # unless explicitly specified, use only registered students
  if(is.null(opt$registration_status)) {
    opt$registration_status <- "Registered" # this will also catch "Student Registered" per filtering functions
  }

  # always use course exclude list
  message("setting --uel flag to TRUE...")
  opt$uel <- TRUE 
  
  # filter courses according to options
  filtered_students <- filter_class_list(students, opt)
  
  # aggregate flag set to all by default
  agg_rolls <- aggregate_rolls(filtered_students,opt) 
  
  message("returning agg_rolls from rollcall...")
  return(agg_rolls) 

}