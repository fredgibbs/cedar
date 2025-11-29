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
  # opt[["course"]] <- "MATH 1350"
  # opt[["term"]] <- 202580
  # opt[["skip_forecast"]] <- TRUE

  message("[course_report.R] Welcome to get_course_data!")

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
  message("[course_report.R] Getting basic enrollment data for course-report...")
  enrls <- get_enrl(courses,myopt)
  
  # check if skipping new forecasts for shiny speed
  if (is.null(opt[["skip_forecast"]]) || opt[["skip_forecast"]] == FALSE) {
    
    # get forecast data for course to see if we need to do more...
    message("[course_report.R] Getting and filtering forecast data for course-report...")
    forecast_data <- forecasts
    
    # check for rows in case forecasts file doesn't exist
    if (!is.null(forecast_data) && nrow(forecast_data) > 0) {
      forecast_data <- forecast_data %>% filter (SUBJ_CRSE == myopt[["course"]])
      forecast_data <- add_term_type_col(forecast_data,"TERM") 
    } # end forecast table exists
    else {
      message("[course_report.R] No forecasting file found. Creating empty table...")
      forecast_data <- data.frame() 
    }
    
    # TODO: need better way to determine if we have enough rows now that we have campus data
    # don't forecast in case we never offer a course for that semester_type, since there's no previous target data
    message("[course_report.R] Checking forecast data for fall, spring, summer...")
    
    if (nrow(enrls %>% filter(term_type == "fall")) > 0 && nrow(forecast_data[forecast_data$term_type=="fall",]) < 6  ) {
      message("[course_report.R] Need more fall forecasts. Retroactively forecasting!")
      message("[course_report.R] Setting myopt$term to 'tl_falls' (from includes/lists.R)")
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
  } # end check if skip forecasting
  else {
    message("[course_report.R] Skipping forecasting as per opt$skip_forecast=TRUE.")
  } 
  
  # reset term after forecasting
  myopt$term <- NULL
  
  # get forecast stats (w enrollments and accuracy)
  forecasts <- calc_forecast_accuracy(students, courses, myopt) # returns a list with short and long versions
  
  if (!is.null(forecasts)) {
    message("[course_report.R] Getting forecast_short data...")
    forecast_short <- forecasts[["forecast_short"]]
    
    # if any forecast short data, select cols
    if (!is.null(forecast_short) && nrow(forecast_short) > 0) {
      course_data[["forecasts"]] <- forecast_short %>% 
        select(-c(dr_early_mean,dr_late_mean,use_enrl_vals,use_cl_vals))  %>% 
        filter (SUBJ_CRSE %in% opt[["course"]])
    }
    else {
      course_data[["forecasts"]] <- forecast_short 
    }
  }
  
  #################### 
  # use REGSTATS to find flagged courses
  message("[course_report.R] Getting regstats flagged data...")
  flagged <-  get_reg_stats(students,courses,myopt)
  course_data[["reg_stats_flagged"]] <-  flagged
  
  
  # combine enrls and regstats and add squeeze calc
  message("[course_report.R] Filtering STUDENTS by course...")
  filtered_students <- students %>% filter (SUBJ_CRSE %in% opt[["course"]])
  message("[course_report.R] Left with ",nrow(filtered_students)," students.")

  message("[course_report.R] Calling calc_cl_enrls to get regstats data...")
  regstats <- calc_cl_enrls(filtered_students)
  
  # TODO: very similar code duplicated in regstats.R
  message("[course_report.R] Calculating squeezes and merging enrls and regstats data...")
  enrls <- merge(enrls,regstats,by.x=c("CAMP","COLLEGE","TERM","SUBJ_CRSE"),by.y=c("Course Campus Code","Course College Code","Academic Period Code","SUBJ_CRSE"),all.x=TRUE )
  enrls <- enrls %>% mutate(squeeze = round(avail/dr_all_mean,digits=2))
  course_data[["enrls"]] <- enrls
  
  
  #################### 
  # run LOOKOUT functions to see where students are coming and going from  
  message("[course_report.R] Getting lookout data...")
  course_data[["where_from"]] <- where_from(students,myopt)
  course_data[["where_to"]] <- where_to(students,myopt)
  course_data[["where_at"]] <- where_at(students,myopt)
  
  
  ################### 
  # get ROLLCALL data (and pivot to wide for report display)
  message("[course_report.R] Getting rollcall data...")

  # Only get registered students
  message("[course_report.R] Setting myopt to look for RE and RS registration status codes only.")
  myopt[["registration_status_code"]] <- c("RE","RS")
  
  # rollcall by classification
  message("[course_report.R] Getting rollcall by classification...")
  myopt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Student Classification", "SUBJ_CRSE","Short Course Title","level")
  rollcall_out <- rollcall(students,myopt)
  rollcall_out <- rollcall_out %>% select(-c(term_type, count, mean, registered, registered_mean)) %>% 
    pivot_wider(names_from = `Academic Period Code`, values_from = pct)
  course_data[["rollcall_by_class"]] <- rollcall_out
  
  # rollcall by major
  message("[course_report.R] Getting rollcall by major...")
  myopt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Major", "SUBJ_CRSE","Short Course Title","level")
  rollcall_out <- rollcall(students,myopt)
  rollcall_out <- rollcall_out %>% select(-c(term_type, count, mean, registered, registered_mean)) %>% 
    pivot_wider(names_from = `Academic Period Code`, values_from = pct)
  course_data[["rollcall_by_major"]] <- rollcall_out
  
  
  # get grade data; opt term should be null to get all data
  message("[course_report.R] Getting gradebook data...")
  grade_data <- get_grades(students,myopt)
  course_data[["grades"]] <- grade_data[["course_term"]]

  message("[course_report.R] Data gathering complete. Returning course data object...")
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
  message("[course_report.R] Welcome to create_course_report!")

  message("[course_report.R] Gathering course data for ", opt[["course"]], "...")
  course_data <- get_course_data(students, courses, forecasts, opt)
  
  # payload
  d_params <- list("opt" = opt,
                   "course_data" = course_data
  )

  message("[course_report.R] Rendering report for ", opt[["course"]], "...")

  # set output data
  message("[course_report.R] Setting output parameters...")
  d_params$output_filename  <- sub(" ", "_", opt[["course"]])
  message("[course_report.R] d_params$output_filename set to: ", d_params$output_filename)
  d_params$rmd_file <- paste0(cedar_base_dir,"/Rmd/course-report.Rmd")
  message("[course_report.R] d_params$rmd_file set to: ", d_params$rmd_file)
  d_params$output_dir_base <- paste0(cedar_output_dir,"course-reports/")
  message("[course_report.R] d_params$output_dir_base set to: ", d_params$output_dir_base)

  message("[course_report.R] Calling create_report...")
  create_report(opt,d_params)
}
