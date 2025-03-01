# this function uses class lists to report all grades
# grade points are added as new column
get_all_grades <- function(filtered_students) {
  
  message("filtering out drops from students with grades...")
  filtered_students <- filtered_students %>% filter (substring(`Registration Status Code`,1,1) != "D") 
  
  # get distinct IDs in each course
  filtered_students <- filtered_students %>% distinct(`Student ID`,SUBJ_CRSE, .keep_all=TRUE)
  
  # calculate grade points from letter grade received and add col to student data 
  # grades_to_points is defined in mappings.R
  message("merging grade points...")
  grades <- merge(filtered_students,grades_to_points,by.x="Final Grade",by.y="grade",keep.all= T)
  
  return(grades)
}

# create DF of just students who dropped specified courses(s)
# DR codes omitted here, for student who drop before drop deadline 
get_dropped <- function(filtered_students) {
  drop_codes <- list("DG","DW","DD")
  dropped <- filtered_students %>% filter (`Registration Status Code` %in% drop_codes) 
  return(dropped)
}

# produce summary of grades (how many students got each grade) for specified courses(s)
# the output (grades_summary) is used to as input for additional aggregating and filtering
get_grades_summary <- function(grades) {
  message("getting grades summary...")
  
  grades_summary <- grades %>% group_by(`Academic Period Code`, SUBJ_CRSE, level, `Long Course Title`, `Primary Instructor Last Name`, `Instruction Delivery Mode Code`, `Sub-Academic Period Code` , `Final Grade`) %>% 
    summarize (count=n())
  
  # arranging by final grade helps the grades stay in order when pivoting to wide
  message("re-ordering grades...")
  grades_summary$`Final Grade` <- factor(grades_summary$`Final Grade`,levels=unlist(grades_to_points[1]))
  grades_summary <- grades_summary %>% arrange(`Final Grade`)
  
  message("initial grades summary:")
  grades_summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(grades_summary)
}


# create a summary of pass / fail / drop
# passing_grades defined in includes/lists.R
get_pf_sum <- function(grades,dropped) {
  message("creating summaries for passing, not passing, etc...")
  passed <- grades %>% filter (`Final Grade` %in% passing_grades) %>%  
    group_by(`Academic Period Code`, SUBJ_CRSE, level, `Primary Instructor Last Name`,`Instruction Delivery Mode Code`, `Sub-Academic Period Code` ) %>% 
    summarize (passed=n(),.groups="keep")
  failed   <- grades %>% filter (!`Final Grade` %in% passing_grades) %>% 
    group_by(`Academic Period Code`, SUBJ_CRSE, level, `Primary Instructor Last Name`,`Instruction Delivery Mode Code`, `Sub-Academic Period Code` ) %>% 
    summarize (failed=n(),.groups="keep")
  dropped_summary   <- dropped %>% 
    group_by(`Academic Period Code`, SUBJ_CRSE, level, `Primary Instructor Last Name`,`Instruction Delivery Mode Code`, `Sub-Academic Period Code` ) %>% 
    summarize (dropped=n(),.groups="keep")
  
  # create row for each term/course/instructor/method/pt combo
  pf_sum <- merge(passed,failed,all="TRUE")
  pf_sum <- merge(pf_sum,dropped_summary,all="TRUE")
  
  # replace all NAs with 0s
  pf_sum <- pf_sum %>% mutate_if(is.numeric, ~replace_na(., 0))
  
  return(pf_sum)
}


# return a summary of pass / fail / drop / data by term and course
# this usually gets appended to full grade data
get_pf_sum_by_course <- function(pf_sum) {
  pf_sum_by_course <- pf_sum %>% group_by(`Academic Period Code`, SUBJ_CRSE ) %>% 
    summarize (passed = sum(passed), failed = sum(failed), dropped = sum(dropped))
  
  message("pass/fail/drop summary:")
  pf_sum_by_course %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(pf_sum_by_course)
}


# produces a wide view of how many students got each grade BY COURSE, aggregating across all instructors
# so, there is one line for each course, with all outcomes as columns
get_grades_summary_by_course <- function(grades_summary,pf_sum_by_course) {
  message("getting grades summary by course...")
  
  # summarize WITHOUT INSTRUCTOR and WITHOUT COURSE TITLE (to group all topics titles together)
  # TODO: could add a grades_summary_by_course_topic if needed
  grades_summary_by_course  <- grades_summary %>% 
    group_by(`Academic Period Code`, SUBJ_CRSE, level,`Final Grade`) %>% 
    summarize (total = sum(count))
  
  message("re-ordering grades...")
  grades_summary_by_course$`Final Grade` <- factor(grades_summary_by_course$`Final Grade`,levels=unlist(grades_to_points[1]))
  grades_summary_by_course <- grades_summary_by_course %>% arrange(`Final Grade`)
  
  # swing to wide
  grades_summary_by_course <- grades_summary_by_course %>% pivot_wider(names_from=`Final Grade`, values_from=total) %>% 
    arrange(`Academic Period Code`)
  
  # change NAs to zeros
  message("changing NAs to 0s...")
  #grades_summary_by_course[is.na(grades_summary_by_course)] <- 0
  grades_summary_by_course <- grades_summary_by_course %>% mutate_if(is.numeric, ~replace_na(., 0))
  
  # merge with pass/fail rates
  grades_summary_by_course <- merge(grades_summary_by_course,pf_sum_by_course)
  
  # compute DFW %
  grades_summary_by_course <- grades_summary_by_course %>% 
    #mutate (`DFW %`=round(failed/(passed+failed)*100,digits=2), .after = `Primary Instructor Last Name` ) %>% 
    mutate (`DFW %`=round((dropped+failed)/(dropped+passed+failed)*100,digits=2), .after=`SUBJ_CRSE` ) %>% 
    arrange(`Academic Period Code`,SUBJ_CRSE)
  
  message("the master gradebook (w/o instructors): course grades (wide) by course and term:")
  grades_summary_by_course %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(grades_summary_by_course)
}


# summarize grades by course averages across all terms
get_course_avg <- function(grades_summary_by_course) {
  
  # don't summarize with long course title because of variations that muck up aggregate reporting
  grades_summary_by_course_avg <- grades_summary_by_course %>% 
    group_by(SUBJ_CRSE, level) %>% 
    summarize(passed = sum(passed), failed = sum(failed), dropped = sum(dropped)) %>% 
    mutate (`DFW %`=round((dropped+failed)/(passed+failed+dropped)*100,digits=2)) %>% 
    arrange(desc(`DFW %`))
  
  message("course averages:")
  grades_summary_by_course_avg %>% tibble::as_tibble() %>% print(n =20, width=Inf)
  
  return(grades_summary_by_course_avg)
}


get_grades_summary_by_course_term_avg <- function(grades_summary_by_course) {
  
  grades_summary_by_course_term_avg <- grades_summary_by_course %>% 
    group_by(`Academic Period Code`, SUBJ_CRSE, level) %>% 
    summarize(passed = sum(passed), failed = sum(failed), dropped = sum(dropped)) %>% 
    mutate (`DFW %`=round((dropped+failed)/(passed+failed+dropped)*100,digits=2)) %>% 
    arrange(`Academic Period Code`,SUBJ_CRSE)
  
  message("course averages per semester:")
  grades_summary_by_course_term_avg %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(grades_summary_by_course_term_avg)
}


# summarize grades by instructor averages for each term
get_inst_term_avg <- function(grades_summary_w){
  
  grades_summary_by_inst_term_avg <- grades_summary_w %>% 
    group_by(`Academic Period Code`, `Primary Instructor Last Name`, level) %>% 
    summarize(passed = sum(passed), failed = sum(failed)) %>% 
    mutate (`DFW %`=round(failed/(passed+failed)*100,digits=2), .after = `Primary Instructor Last Name` ) %>% 
    arrange(`Academic Period Code`,`Primary Instructor Last Name`)
  
  message("course + inst averages per semester:")
  grades_summary_by_inst_term_avg %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return (grades_summary_by_inst_term_avg)
}


# summarize grades by instructor averages across all terms
get_inst_avg <- function(grades_summary_w){
  
  grades_summary_by_inst_avg <- grades_summary_w %>% 
    group_by(`Primary Instructor Last Name`, level) %>% 
    summarize(passed = sum(passed), failed = sum(failed)) %>% 
    mutate (`DFW %`=round(failed/(passed+failed)*100,digits=2), .after = `Primary Instructor Last Name` ) %>% 
    arrange(`Primary Instructor Last Name`)
  
  message("course + inst averages:")
  grades_summary_by_inst_avg %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(grades_summary_by_inst_avg)
}




# main controller for function, to be called from cedar or course report; dept report uses next function)
# no parameters required, but usually a course will be specified 
get_grades <- function(students,opt) {
  
  # for studio testing
  # opt <- list()
  # students <- load_students(opt)
  # opt$course <- "AFST 1110"
  # opt$term <- "202460"

  if (is.null(opt$aggregate)) {
    message("no aggregate param found. setting -a to 'all'.")
    opt$aggregate <- "all"
  }
  
  course <- opt[["course"]]
  message("procesing course: ",course)
  
  if (!is.null(course) && is.character(course)){
    # if course set to "existing", use list of courses already in forecast_table.
    # this is a good way to round out forecasts.
    if (as.character(course) == "forecasts") {
      forecast_data <- load_forecasts(list())
      opt$course <- unique(as.list(forecast_data$SUBJ_CRSE))
      message("finished processing course as forecasts!")
    }
    else if (as.character(course) == "dimps") {
      opt$course <- get_dimp_courses(students,courses,opt)
      message("finished processing course as dimps!")
    }
  }
  
  # filter students from opt params (usually course and term combination)
  message("filtering students from opt params...")
  filtered_students <- filter_class_list(students,opt)
  
  message("only using data since 2019, after Gen Ed implementation.")
  filtered_students <- filtered_students %>% filter (`Academic Period Code` >= 201980)
  
  message("setting Final Grade to `Drop` if registration status code is `DR`.")
  filtered_students <- filtered_students %>% mutate (`Final Grade` = ifelse(`Registration Status Code`=="DR", "Drop", `Final Grade`))
  
  # get all grades
  grades <- get_all_grades(filtered_students)
  
  # get LONG view of grades received in each course 
  grades_summary <- get_grades_summary(grades)
  
  # find students who dropped
  dropped <- get_dropped(filtered_students)
  
  # create pass/fail summary
  pf_sum <- get_pf_sum(grades,dropped)
  pf_sum_by_course <- get_pf_sum_by_course(pf_sum)
  
  # create wide view of all grades received for each course each term, incl sums of passed, failed, dropped
  # this gets passed to more specific aggregating functions specified by opt params
  grades_summary_by_course <- get_grades_summary_by_course(grades_summary,pf_sum_by_course)
  
  
  #TODO: restore inst info
  # if (opt$aggregate == "inst_term_avg" || opt$aggregate == "all") {
  #   grades_summary_by_inst_term_avg <- get_inst_term_avg(grades_summary_by_course)
  # }
  # 
  # if (opt$aggregate == "inst_avg" || opt$aggregate == "all") {
  #   grades_summary_by_inst_avg <- get_inst_avg(grades_summary_by_course)
  # }
  # 
  
  message("processing gradebook_agg_by param...")
  
  # returns the basic grade summary by course, which is used for the other views
  # grades_summary_by_course is always called above for all functions, so we can just return it
  if (opt$aggregate == "course") {
    return (grades_summary_by_course)
  }
  
  if (opt$aggregate == "course_term_avg") {
    grades_summary_by_course_term_avg <- get_grades_summary_by_course_term_avg(grades_summary_by_course)
    return(grades_summary_by_course_term_avg)
  }
  
  if (opt$aggregate == "course_avg") {
    grades_summary_by_course_avg <- get_course_avg(grades_summary_by_course)
    return (grades_summary_by_course_avg)
  }
  
  if (opt$aggregate == "all") {
    # grades_summary_by_course is always outputted above
    grades_summary_by_course_term_avg <- get_grades_summary_by_course_term_avg(grades_summary_by_course)
    grades_summary_by_course_avg <- get_course_avg(grades_summary_by_course)
  }
}


# this is specifically for creating dept report outputs using d_params 
# it sets gradebook_agg_by to "course" and does additional filtering for lower division courses if available
get_grades_for_dept_report <- function(students,opt,d_params) {
  
  # opt <- list()
  # opt$dept <- "POLS"
  
  # for plotting
  myopt <- opt
  myopt["dept"] <- d_params$dept_code 
  myopt$aggregate <- "course_avg"
  grades_summary_by_course_avg <- get_grades(students,myopt)
  
  # for chart (with more details)
  opt$gradebook_agg_by <- "course"
  grades_summary_by_course <- get_grades(students,myopt)
  
  # filter for lower division, unless there aren't any (like MSST)
  grades_summary_for_ld <- grades_summary_by_course %>% filter (level == "lower")
  
  if (nrow(grades_summary_for_ld) == 0) {
    grades_summary_for_ld <- grades_summary_by_course
  }
  
  message("adding grades_summary_for_ld to d_params...")
  d_params$tables[["grades_summary_for_ld"]] <- grades_summary_for_ld 
  
  
  # use course averages for plotting
  grades_summary_for_ld <- grades_summary_by_course_avg %>% filter (level == "lower")
  
  if (nrow(grades_summary_for_ld) == 0) {
    grades_summary_for_ld <- grades_summary_by_course
  }
  
  grades_summary_for_ld <- grades_summary_for_ld %>% ungroup()
  
  grades_summary_for_ld_plot <- grades_summary_for_ld %>% 
    mutate(SUBJ_CRSE = fct_reorder(SUBJ_CRSE, `DFW %`)) %>%
    ggplot(aes(y=SUBJ_CRSE, x=`DFW %`)) + 
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    ylab("Course") + xlab("mean DFW % (since 2019)") 
  
  grades_summary_for_ld_plot
  
  message("adding grades_summary_for_ld_plot to d_params...")
  d_params$plots[["grades_summary_for_ld_plot"]] <- grades_summary_for_ld_plot 
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}

