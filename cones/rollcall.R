# rollcall.R provides data on the major and classification of students in a course or set of courses
# no params required, but then too much data is outputted
# best to specify at least a course (or course_list) to see change over time, and/or a term


# generic summary function based on group_cols
# replaces the many variants of aggregate
summarize_classifications <- function (filtered_students, opt) {
  message("summarizing students with group_cols...")

  group_cols <- opt[["group_cols"]]
  
  # set default group_cols
  if (is.null(group_cols)) {
    group_cols <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Major", "Student Classification", "SUBJ_CRSE","Short Course Title","level")
  }
  else {
    group_cols <- convert_param_to_list(group_cols)
    print(str(group_cols))
    group_cols <- as.character(group_cols)
    print(str(group_cols))
  }
  
  summary <- filtered_students %>% group_by_at(group_cols) %>% 
    distinct(`Student ID`,.keep_all=TRUE) %>% 
    summarize(.groups="keep", count=n())
  
  # regroup without APC to calc means
  group_cols <- group_cols[-which(group_cols %in% c('Academic Period Code') )]

  # get means across term_types
  summary <- summary %>% group_by_at(group_cols) %>% 
    mutate(mean = round(mean(count),digits=1))
  
  
  # get enrollment for courses to calc % of enrollment
  # get course enrollments from class lists (in enrl.R)
  # NOTE: this gets cl enrollment, not registered enrollment
  reg_summary <- calc_cl_enrls(filtered_students)
  
  crse_enrollment <- reg_summary %>% ungroup() %>% select(c(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, `Academic Period Code`, registered, reg_mean))
  
  # merge with course enrollments to calc percent from each classification
  merge_sum_enrl <- merge(summary,crse_enrollment,by=c("Course Campus Code", "Course College Code", "Academic Period Code", "SUBJ_CRSE"))
  merge_sum_enrl <- merge_sum_enrl %>% group_by(`Course Campus Code`, `Course College Code`,`Academic Period Code`,SUBJ_CRSE) %>% 
    mutate(pct = round(count / registered * 100, digits=1)) %>% 
    select (-c(count,registered,reg_mean)) %>% 
    arrange (`Academic Period Code`,desc(pct))
  
  return(merge_sum_enrl)
}



########### main function ##########
rollcall <- function (students,opt) {
  message("\nWelcome to Rollcall!")
  
  if (is.null(opt[["group_cols"]])) {
    message("Everything works better with the group_cols param. Grouping with Student Classificaiton and Major...")
    opt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Student Classification","SUBJ_CRSE","Short Course Title","level")
  }
  
  # opt <- list()
  # opt$term <- NULL
  # opt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Student Classification", "SUBJ_CRSE","Short Course Title","level")
  # opt$course <- "ENGL 1120"
  # # 
  # #opt$classification <-  'Freshman, 1st Yr, 1st Sem'
  # opt$major <- "Nursing, Pre Business Administration, Pre Psychology, Pre Biology, Undecided, Pre Mechanical Engineering, Pre FDMA, Pre Computer Science"

  # always use course exclude list
  message("setting --uel flag to TRUE...")
  opt$uel <- TRUE 
  
  # students <- load_students()
  filtered_students <- filter_class_list(students, opt)

  # ignore courses before common course numbering
  message("removing students pre 201980...")
  students$`Academic Period Code` <- as.integer(students$`Academic Period Code`)
  filtered_students <- filtered_students %>% filter (`Academic Period Code` >= 201980) 
    

  # TODO: process a course list
  
  message("aggregating the student majors/classifications...")
  summary <- summarize_classifications(filtered_students,opt)
  
  message("returning summary from rollcall...")
  return(summary) 

}
