# course_report produces a basic report for a particular course.
# This calls and gathers various other reporting functions, such as enrl, forecast, rollout, lookout, gradebook,
# Designed to be run from the command line via cedar.R -f course-report -d DEPTCODE
# Reports are saved in CEDAR_OUTPUT_DIR/course-reports (in either.
# REQUIRES: opt$course and opt$term
# OPTIONAL: output-format: html (default) or aspx

# TODO: create loops to manage and accept course and term lists for batch processing

course_report <- function(students, courses, opt) {
  
  # for studio testing...
  # students <- load_students(opt)
  # courses <- load_courses(opt)
  # opt <- list()
  # opt[["course"]] <- "HIST 491"
  # opt[["term"]] <- 202510
  
  # these should always be set this way
  opt$status <- "A"
  opt$uel <- TRUE
  opt$summer <- FALSE
  opt$aggregate <- "course"
  
  print(opt)
  
  # create term agnostic opt param
  myopt <- opt
  myopt[["term"]] <- NULL
  
  # get basic enrollment data for all terms
  enrls <- get_enrl(courses,myopt)
  
  # load forecast data from forecast table; see forecast.R
  forecast_data <- get_forecast_data(students,courses,opt)
  
  # see if we have at least 3 terms of data to report 
  # if missing forecast data, forecast!
  if (nrow(forecast_data) < length(tl_recents)) {
    message("forecast history too short... retroactively forecasting!")
    message("setting  opt$term to 'tl_recents' (from includes/lists.R)")
    myopt$term <- "tl_recents"
    forecast_summary <- forecast(students,courses,myopt)
  } else {
    message("forecast data found for all specified terms!")
  }
  
  # use forecast-report.R to load forecast data with enrollments and accuracy
  forecast_summary <- calc_forecast_accuracy(courses, myopt)
  
  # set history to go up to the term prior to the specified forecast term
  forecast_history <- forecast_summary %>% filter (TERM <= subtract_term(opt[["term"]]))
  message("forecast history:")
  print(forecast_history)
  
  # isolate target term's forecast
  forecast_next_term <- forecast_summary %>% 
    filter (TERM == opt$term) %>% 
    select(-c("fts_accr","conduit_accr","major_accr"))
  
  forecast_next_term %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  if (opt$nso && get_term_type(opt[["term"]]) == "Fall") {
    
    
    ############### 
    # use nosedive to find out freshman contribution to course we're reporting on
    
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
  }
  
  
  # run lookout functions to see where students are coming and going from  
  message("getting lookout data...")
  where_from_data <- where_from(students,myopt)
  where_to_data <- where_to(students,myopt)
  where_at_data <- where_at(students,myopt)
  
  # get rollcall data
  message("getting rollcall data...")
  myopt[["aggregate"]] <- "classification_wide"
  agg_by_class <- rollcall(students,myopt)
  
  myopt[["aggregate"]] <- "major_wide"
  agg_by_major <- rollcall(students,myopt)
  
  
  # get grade data
  # set term as null to get all data
  message("getting gradebook data...")
  myopt[["aggregate"]] <- "course"
  grade_data <- get_grades(students,myopt)
  
  
  # payload
  d_params <- list("course" = opt[["course"]],
                   "term"  = opt[["term"]],
                   "opt" = opt,
                   "tables" = list(
                     "enrls" = enrls,
                     "where_from_data" = where_from_data,
                     "where_to_data" = where_to_data,
                     "where_at_data" = where_at_data,
                     "agg_by_class" = agg_by_class,
                     "agg_by_major" = agg_by_major,
                     "grade_data" = grade_data,
                     "forecast_history" = forecast_history,
                     "forecast_next_term" = forecast_next_term
                   )
  )
  
  
  message("rendering report for ", opt[["course"]],"...")
  
  # set output data
  output_filename <- sub(" ", "_", opt[["course"]])
  d_params$output_filename <- output_filename
  d_params$rmd_file <- "cones/course-report/course-report.Rmd"
  d_params$output_dir_base <- paste0(cedar_output_dir,"course-reports/")
  
  create_report(opt,d_params)
}
