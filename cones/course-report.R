# course_report produces a basic report for a particular course.
# This calls and gathers various other reporting functions, such as enrl, forecast, rollout, lookout, gradebook,
# Designed to be run from the command line via cedar.R -f course-report -d DEPTCODE
# Reports are saved in CEDAR_OUTPUT_DIR/course-reports (in either.
# REQUIRES: opt$course and opt$term
# OPTIONAL: output-format: html (default) or aspx

# TODO: create loops to manage and accept course and term lists for batch processing
# some already in cedar.R; need to separate processing from actual report call as with forecast, regstats, etc

get_course_data <- function(students, courses, forecasts, opt) {
  # for studio testing...
  # students <- load_students()
  # courses <- load_courses()
  # opt <- list()
  # opt[["course"]] <- "ENGL 1120"
  # opt[["term"]] <- 202510

  message("\nWelcome to get_course_data!")
  
  # init payload list for return value
  course_data <- list()
  
  # these should always be set this way
  opt$status <- "A"
  opt$uel <- TRUE
  
  # create term agnostic opt param for getting historic enrollments
  myopt <- opt
  myopt[["term"]] <- NULL
  myopt[["group_cols"]] <- c("CAMP","COLLEGE","TERM", "term_type", "SUBJ", "SUBJ_CRSE", "CRSE_TITLE")
  
  # get basic enrollment data for all terms
  message("getting basic enrollment data for course-report...")
  enrls <- get_enrl(courses,myopt)
  
  # get forecast data for course
  message("filtering forecast data for course-report...")
  forecast_data <- forecasts
  forecast_data <- forecast_data %>% filter (SUBJ_CRSE == myopt[["course"]])
  forecast_data <- add_term_type_col(forecast_data,"TERM") 
  
  # use 6 as threshold because the table has major and conduit projections (= to 3 term_types)
  # don't forecast in case we never offer a course that semester_type, since there's no previous target data
  message("checking forecast data for fall, spring, summer...")
  
  if (nrow(enrls %>% filter(term_type == "fall")) > 0 && nrow(forecast_data[forecast_data$term_type=="fall",]) < 6  ) {
    message("need more fall forecasts. retroactively forecasting!")
    message("setting  myopt$term to 'tl_falls' (from includes/lists.R)")
    myopt$term <- "tl_falls"
    forecast(students, courses, myopt)
  }
  
  if (nrow(enrls %>% filter(term_type == "spring")) > 0 && nrow(forecast_data[forecast_data$term_type=="spring",]) < 6) {
    message("need more spring forecasts. retroactively forecasting!")
    message("setting  myopt$term to 'tl_springs' (from includes/lists.R)")
    myopt$term <- "tl_springs"
    forecast(students, courses, myopt)
  }
  
  if (nrow(enrls %>% filter(term_type == "summer")) > 0 && nrow(forecast_data[forecast_data$term_type=="summer",]) < 6) {
    message("need more summer forecasts. retroactively forecasting!")
    message("setting  myopt$term to 'tl_summers' (from includes/lists.R)")
    myopt$term <- "tl_summers"
    forecast(students, courses, myopt)
  } 
  
  # reset term after forecasting
  myopt$term <- NULL
  
  # get forecast stats (w enrollments and accuracy)
  forecasts <- calc_forecast_accuracy(students, courses, myopt) # returns a list with short and long versions
  forecast_short <- forecasts[["forecast_short"]]
  
  # if any data, select cols
  if (nrow(forecast_short) > 0) {
    course_data[["forecasts"]] <- forecast_short %>% select(-c(de_mean,dl_mean,use_enrl_vals,use_cl_vals))
  }
  else {
    course_data[["forecasts"]] <- forecast_short
  }
  
  # use regstats to find flagged courses
  flagged <-  get_reg_stats(students,courses,myopt)
  course_data[["reg_stats_flagged"]] <-  flagged
  
  
  # combine enrls and regstats and add squeeze calc
  message("filtering STUDENTS by course...")
  filtered_students <- students %>% filter (SUBJ_CRSE %in% opt[["course"]])
  message("left with ",nrow(filtered_students)," students.")
  
  regstats <- calc_cl_enrls(filtered_students)
  
  # TODO: very similar code duplicated in regstats.R
  message("calculating squeeze...")
  squeezes <- merge(enrls,regstats,by.x=c("CAMP","COLLEGE","TERM","SUBJ_CRSE"),by.y=c("Course Campus Code","Course College Code","Academic Period Code","SUBJ_CRSE"),all.x=TRUE )
  squeezes <- squeezes %>% mutate(squeeze = round(avail/da_mean,digits=2))
  course_data[["enrls"]] <- squeezes
  
  
  # run lookout functions to see where students are coming and going from  
  message("getting lookout data...")
  course_data[["where_from"]] <- where_from(students,myopt)
  course_data[["where_to"]] <- where_to(students,myopt)
  course_data[["where_at"]] <- where_at(students,myopt)
  
  # get rollcall data and pivot to wide
  message("getting rollcall data...")

  myopt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Student Classification", "SUBJ_CRSE","Short Course Title","level")
  rollcall_out <- rollcall(students,myopt)
  rollcall_out <- rollcall_out %>% select(-c(term_type, mean)) %>% 
    pivot_wider(names_from = `Academic Period Code`, values_from = pct)
  course_data[["rollcall_by_class"]] <- rollcall_out
  
  myopt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Major", "SUBJ_CRSE","Short Course Title","level")
  rollcall_out <- rollcall(students,myopt)
  rollcall_out <- rollcall_out %>% select(-c(term_type, mean)) %>% 
    pivot_wider(names_from = `Academic Period Code`, values_from = pct)
  course_data[["rollcall_by_major"]] <- rollcall_out
  
  
  # get grade data; opt term should be null to get all data
  message("getting gradebook data...")
  myopt[["aggregate"]] <- "course"
  course_data[["grades"]] <- get_grades(students,myopt)[["course"]]
  
  return (course_data)
}


use_NSO_data_for_forecasts <- function() {
  ############### 
  # UNFINISHED: use nosedive to find out freshman contribution to course we're reporting on
  ###############
  message("looking for NSO flog and if target term type is fall...")
  if (opt$nso && get_term_type(opt[["term"]]) == "fall") {
    
    # load NSO data
    NSOers <- load_NSO_data()
    
    # calculate NSO Freshman contribution to course
    # use opt, which should have target term specified
    # forecast_enrl_from_majors uses rollcall, so make sure necessary params are set
    # TODO: needs to fit new rollcall code
    myopt[["group_col"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "Student Classification", "Major", "SUBJ_CRSE","Short Course Title")
    prog_NSO_enrl <- forecast_enrl_from_majors(NSOers,students,opt)
    message("results from forecast_enrl_from_majors:")
    print(prog_NSO_enrl)
    
    # get just course for report 
    prog_NSO_enrl <- prog_NSO_enrl %>% 
      filter (SUBJ_CRSE == opt$course) 
    
    # merge freshman projection
    message("merging nso_enrl_projections from nosedive with forecast data...")
    forecast_next_term <- merge (forecast_next_term, prog_NSO_enrl[ , c("SUBJ_CRSE","fresh_proj")], by = "SUBJ_CRSE")
    forecast_next_term %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
    
    # look up how many current NSOers are registered for specified target term (from opt)
    filtered_students <- students %>% filter_class_list(opt)
    
    message("getting number of NSOers registered in courses...")
    NSOers_in_course <- get_NSOers_in_courses(NSOers, filtered_students, opt)
    
    forecast_next_term <- merge (forecast_next_term, NSOers_in_course[ , c("SUBJ_CRSE","count")], by = "SUBJ_CRSE")
    forecast_next_term %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  } else {
    message("ignoring nso data.")
  }
  
}



create_course_report <- function(students, courses, forecasts, opt) {
  
  message("\nWelcome to create_course_report!")
  
  course_data <- get_course_data(students, courses, forecasts, opt)
  
  # payload
  d_params <- list("opt" = opt,
                   "course_data" = course_data
                   )
  
  message("rendering report for ", opt[["course"]],"...")
  
  # set output data
  output_filename <- sub(" ", "_", opt[["course"]])
  d_params$output_filename <- output_filename
  d_params$rmd_file <- paste0(cedar_base_dir,"/Rmd/course-report.Rmd")
  d_params$output_dir_base <- paste0(cedar_output_dir,"course-reports/")
  
  create_report(opt,d_params)
}
