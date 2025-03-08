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
  # opt[["course"]] <- "UHON 301"
  #opt[["term"]] <- 202510
  
  # init payload list for return value
  course_data <- list()
  
  # these should always be set this way
  opt$status <- "A"
  opt$uel <- TRUE
  
  
  # create term agnostic opt param for getting historic enrollments
  myopt <- opt
  myopt[["term"]] <- NULL
  myopt[["aggregate"]] <- "subj_crse" # not sure if this is used by called functions
  myopt[["group_cols"]] <- c("TERM", "SUBJ", "SUBJ_CRSE", "CRSE_TITLE", "level", "gen_ed_area")
  
  # get basic enrollment data for all terms
  message("getting basic enrollment data for course-report...")
  enrls <- get_enrl(courses,myopt)
  
  # TODO: likely redundant, already in get_enrl
  # TODO: this seems only used by the forecasting logic below; and gets overwritten later by calc_squeezes
  enrls <- add_term_type_col(enrls,"TERM")
  
  forecast_data <- forecasts
  forecast_data <- forecast_data %>% filter (SUBJ_CRSE == myopt[["course"]])
  forecast_data <- add_term_type_col(forecast_data,"TERM") 
  
  # use 8 as threshold because the table has major and conduit projections (= to 4 terms)
  # don't forecast in case we never offer a course that semester, since there's no previous target data
  if (nrow(enrls %>% filter(term_type == "fall")) > 0 && nrow(forecast_data[forecast_data$term_type=="fall",]) < 6  ) {
    message("need more fall forecasts. retroactively forecasting!")
    message("setting  myopt$term to 'tl_falls' (from includes/lists.R)")
    myopt$term <- "tl_falls"
    forecast(students,courses,myopt)
  }
  
  if (nrow(enrls %>% filter(term_type == "spring")) > 0 && nrow(forecast_data[forecast_data$term_type=="spring",]) < 6) {
    message("need more spring forecasts. retroactively forecasting!")
    message("setting  myopt$term to 'tl_springs' (from includes/lists.R)")
    myopt$term <- "tl_springs"
    forecast(students,courses,myopt)
  }
  
  if (nrow(enrls %>% filter(term_type == "summer")) > 0 && nrow(forecast_data[forecast_data$term_type=="summer",]) < 6) {
    message("need more summer forecasts. retroactively forecasting!")
    message("setting  myopt$term to 'tl_summers' (from includes/lists.R)")
    myopt$term <- "tl_summers"
    forecast(students,courses,myopt)
  } 
  
  # reset term
  myopt[["term"]] <- NULL
  
  
  # use forecast-report.R to load forecast data with enrollments and accuracy
  forecasts <- calc_forecast_accuracy(students, courses, myopt) # returns a list with short and long versions
  forecasts <- forecasts[["forecast_short"]]
  course_data[["forecasts"]] <- forecasts %>% select(-c(de_mean,dl_mean,use_enrl_vals,use_cl_vals))
  
  
  # use regstats to find flagged courses
  course_data[["reg_stats_flagged"]] <-  get_reg_stats(students,courses,myopt)
  
  # get class list enrollment data for more thorough reporting
  filtered_students <- students %>% filter_class_list(myopt)
  
  # get squeezes, already merged with enrl_cl_data already merged with enrl data
  # TODO: this overwrites an existing enrls from get_enrl, which is bad
  # TODO: need to keep enrl data and squeeze data separate in functions and combine here
  course_data[["enrls"]] <- calc_squeezes(filtered_students,courses,myopt) # calls calc_cl_enrls and returns all regstats info
  
  # message("merging forecast summary data with enrollment summary data...")
  # enrl_w_forecast <- merge (enrl_w_forecast, cl_enrls, by.x=c("SUBJ_CRSE", "TERM","term_type"), by.y=c("SUBJ_CRSE", "Academic Period Code","term_type") ,all.x=T)
  
  # run lookout functions to see where students are coming and going from  
  message("getting lookout data...")
  course_data[["where_from"]] <- where_from(students,myopt)
  course_data[["where_to"]] <- where_to(students,myopt)
  course_data[["where_at"]] <- where_at(students,myopt)
  
  # get rollcall data
  message("getting rollcall data...")
  myopt[["aggregate"]] <- "classification_wide"
  course_data[["rollcall_by_class"]] <- rollcall(students,myopt)
  
  myopt[["aggregate"]] <- "major_wide"
  course_data[["rollcall_by_major"]] <- rollcall(students,myopt)
  
  
  # get grade data; opt term should be null to get all data
  message("getting gradebook data...")
  myopt[["aggregate"]] <- "course"
  course_data[["grades"]] <- get_grades(students,myopt)
  
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
    myopt[["aggregate"]] <- "course_classification_major"
    
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
  
  course_data <- get_course_data(students,courses, forecasts, opt)
  
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
