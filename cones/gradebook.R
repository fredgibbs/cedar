# this function uses class lists to report all grades
# grade points are added as new column
get_all_grades <- function(filtered_students) {
  
  message("filtering out drops from students with grades...")
  filtered_students <- filtered_students %>% filter (substring(`Registration Status Code`,1,1) != "D") 
  
  # get distinct IDs in each course
  filtered_students <- filtered_students %>% distinct(`Student ID`,`Course Campus Code`,`Course College Code`,SUBJ_CRSE, .keep_all=TRUE)
  
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
  
  grades_summary <- grades %>% group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE, level, `Long Course Title`, `Primary Instructor Last Name`, `Instruction Delivery Mode Code`, `Sub-Academic Period Code` , `Final Grade`, .groups="keep") %>% 
    summarize (count=n(), .groups="keep")
  
  # arranging by final grade helps the grades stay in order when pivoting to wide
  message("re-ordering grades...")
  grades_summary$`Final Grade` <- factor(grades_summary$`Final Grade`,levels=unlist(grades_to_points[1]))
  grades_summary <- grades_summary %>% arrange(`Final Grade`)
  
  # message("initial grades summary:")
  # grades_summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(grades_summary)
}


# create a summary of pass / fail / drop
# passing_grades defined in includes/lists.R
get_pf_sum <- function(grades,dropped) {
  message("creating summaries for passing, not passing, etc...")
  
  group_cols <- c("Course Campus Code", "Course College Code", "Academic Period Code", "SUBJ_CRSE", "level", "Primary Instructor Last Name","Instruction Delivery Mode Code", "Sub-Academic Period Code") 
  
  passed <- grades %>% filter (`Final Grade` %in% passing_grades) %>%  
    group_by_at(group_cols) %>% 
    summarize (passed=n(), .groups="keep")
  failed   <- grades %>% filter (!`Final Grade` %in% passing_grades) %>% 
    group_by_at(group_cols) %>% 
    summarize (failed=n(), .groups="keep")
  dropped_summary   <- dropped %>% 
    group_by_at(group_cols) %>% 
    summarize (dropped=n(), .groups="keep")
  
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
  pf_sum_by_course <- pf_sum %>% group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE ) %>% 
    summarize (passed = sum(passed), failed = sum(failed), dropped = sum(dropped))

  return(pf_sum_by_course)
}


# produces a wide view of how many students got each grade BY COURSE, aggregating across all instructors
# so, there is one line for each course, with all outcomes as columns
get_grades_summary_by_course <- function(grades_summary,pf_sum_by_course) {
  message("getting grades summary by course...")
  
  # summarize WITHOUT INSTRUCTOR and WITHOUT COURSE TITLE (to group all topics titles together)
  # TODO: could add a grades_summary_by_course_topic if needed
  grades_summary_by_course  <- grades_summary %>% 
    group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE, level,`Final Grade`) %>% 
    summarize (total = sum(count), .groups="keep")
  
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
    mutate (`DFW %`=round((dropped+failed)/(dropped+passed+failed)*100,digits=2), .after=`SUBJ_CRSE` ) %>% 
    arrange(`Course Campus Code`, `Course College Code`, `Academic Period Code`,SUBJ_CRSE)
  
  return(grades_summary_by_course)
}


# summarize grades takes a vector of cols to group by and summarizes 
summarize_grades <- function(grades_summary_by_course,opt) {
  group_cols <- opt[["group_cols"]]
  group_cols <- convert_param_to_list(group_cols)
  group_cols <- as.character(group_cols)
  
  summary <- grades_summary_by_course %>% 
    group_by_at(group_cols) %>% 
    summarize(passed = sum(passed), failed = sum(failed), dropped = sum(dropped), .groups="keep") %>% 
    mutate (`DFW %`=round((dropped+failed)/(passed+failed+dropped)*100,digits=2))
  
  message("returning summary...")
  return (summary)
}



# main controller for function, to be called from cedar or course report; dept report uses next function)
# no parameters required, but usually a course will be specified 
get_grades <- function(students,opt) {
  
  # for studio testing
  # opt <- list()
  # students <- load_students()
  # opt$course <- "AFST 1110"
  # opt$dept <- "HIST"
  # opt$term <- "202460"
  #opt <- myopt
  
  if (is.null(opt$aggregate)) {
    message("no aggregate param found. setting -a to 'all'.")
    opt$aggregate <- "all"
  }
  
  course <- opt[["course"]]
  message("procesing course: ",course)
  
  if (!is.null(course) && is.character(course)){
    # if course set to "forecasts", use list of courses already in forecast_table.
    # this is a good way to round out forecast data
    if (as.character(course) == "forecasts") {
      forecast_data <- load_forecasts()
      opt$course <- unique(as.list(forecast_data$SUBJ_CRSE))
      message("finished processing course as forecasts!")
    }
    # TODO: accept CSV file as input
  }
  
  # filter students from opt params (usually course and term OR dept for dept reports)
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
  
  
  message("processing gradebook_agg_by param...")
  
  grades <- list()
  
  # returns the basic grade summary by course, which is used for the other views
  # grades_summary_by_course is always called above for all functions, so we can just return it
  if (opt$aggregate == "course") {
    grades[["course"]] <- grades_summary_by_course
  }
  
  if (opt$aggregate == "course_term_avg") {
    opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "SUBJ_CRSE", "level")
    grades[["course_term"]] <- summarize_grades(grades_summary_by_course, opt)
  }
  
  if (opt$aggregate == "course_avg") {
    opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "SUBJ_CRSE", "level")
    grades[["course_avg"]] <- summarize_grades(grades_summary_by_course, opt)
  }

  message("returning grades...")
  return (grades)  
}


# this is specifically for creating dept report outputs using d_params 
# it does additional filtering for lower division courses if available
get_grades_for_dept_report <- function(students,opt,d_params) {
  
  # studio testing set up  
  # opt <- list()
  # students <- load_students()

  # for plotting
  myopt <- opt
  myopt[["dept"]] <- d_params$dept_code 
  # myopt[["dept"]] <- "HIST"
  
  # limit to ABQ campus until we have better plotting across campuses
  myopt[["campus"]] <- c("ABQ","EA")
  
  # get grades by each course and term for table in dept report
  myopt$aggregate <- "course"
  grades_summary_by_course <- get_grades(students,myopt)[["course"]]
  
  # filter for lower division, unless there aren't any (like MSST)
  grades_summary_by_course_ld <- grades_summary_by_course %>% filter (level == "lower")
  
  if (nrow(grades_summary_by_course_ld) == 0) {
    grades_summary_by_course_ld <- grades_summary_by_course
  }
  
  message("adding grades_summary_by_course_ld to d_params...")
  d_params$tables[["grades_summary_for_ld"]] <- grades_summary_by_course_ld 
  
  
  # get average grades for plot in dept report
  myopt$aggregate <- "course_avg"
  grades_summary_by_course_avg <- get_grades(students,myopt)[["course_avg"]]
  
  grades_summary_by_course_avg_ld <- grades_summary_by_course_avg %>% filter (level == "lower") 
  # %>% 
  #   filter (`Course Campus Code` =="ABQ")
  # 
  if (nrow(grades_summary_by_course_avg_ld) == 0) {
    grades_summary_by_course_avg_ld <- grades_summary_by_course_avg
  }
  
  grades_summary_by_course_avg_ld <- grades_summary_by_course_avg_ld %>% ungroup()
  
  grades_summary_for_ld_plot <- grades_summary_by_course_avg_ld %>% 
    mutate(SUBJ_CRSE = fct_reorder(SUBJ_CRSE, `DFW %`)) %>%
    ggplot(aes(y=SUBJ_CRSE, x=`DFW %`, fill=`Course Campus Code`)) + 
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity", position=position_dodge()) +
    ylab("Course") + xlab("mean DFW % (since 2019)") 
  
  grades_summary_for_ld_plot
  
  message("adding grades_summary_for_ld_abq_ea_plot to d_params...")
  d_params$plots[["grades_summary_for_ld_abq_ea_plot"]] <- grades_summary_for_ld_plot 

  # # repeat for EA campus
  # grades_summary_by_course_avg_ld <- grades_summary_by_course_avg %>% filter (level == "lower") %>% 
  #   filter (`Course Campus Code` == "EA")
  # 
  # if (nrow(grades_summary_by_course_avg_ld) == 0) {
  #   grades_summary_by_course_avg_ld <- grades_summary_by_course_avg
  # }
  # 
  # grades_summary_by_course_avg_ld <- grades_summary_by_course_avg_ld %>% ungroup()
  # 
  # grades_summary_for_ld_plot <- grades_summary_by_course_avg_ld %>% 
  #   mutate(SUBJ_CRSE = fct_reorder(SUBJ_CRSE, `DFW %`)) %>%
  #   ggplot(aes(y=SUBJ_CRSE, x=`DFW %`)) + 
  #   theme(legend.position="bottom") +
  #   guides(color = guide_legend(title = "")) +
  #   geom_bar(stat="identity") +
  #   ylab("Course") + xlab("mean DFW % (since 2019)") 
  # 
  # #grades_summary_for_ld_plot
  # 
  # message("adding grades_summary_for_ld_ea_plot to d_params...")
  # d_params$plots[["grades_summary_for_ld_ea_plot"]] <- grades_summary_for_ld_plot 
  # 
  
  
  
  
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}

