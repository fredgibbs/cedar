# This set of functions helps forecast enrollment for specified courses and terms
# The actual methods are in separate files
# Can be run from CEDAR, and is also called from course-report.R
# REQUIRES students, courses, and opt 


# calc_forecast_accuracy loads previously generated forecasting data from the forecasts.Rda file
# and merges enrollment data with forecast data to display together and compute accuracy of forecasts
# OPTIONAL param: opt$course for filtering returned data

calc_forecast_accuracy <- function(courses, opt) {
  message("Welcome to calc_forecast_accuracy!")
  
  # uncomment for studio testing
  # courses <- load_courses(opt)
  # opt <- list()
  # opt$course <- 'HIST 491'
  
  # load existing forecast data
  forecast_data <- load_forecasts(opt)
  
  if (!is.null(opt$course)) {
    message("filtering for course...")
    course_list <- convert_param_to_list(opt$course)
    forecast_data <- forecast_data %>% filter (SUBJ_CRSE %in% course_list)
  }
  
  if (!is.null(forecast_data) && nrow(forecast_data) == 0) {
    stop("no forecast data for that course.")  
  }
  
  message("original forecast data:")
  forecast_data %>% tibble::as_tibble() %>% print(n = 10, width=Inf)
  
  # make easier to read with wide format
  message("pivoting to wide...")
  forecast_data <- forecast_data %>% select (-c(continuing_forecast,incoming_forecast)) 
  forecast_data_wide <- forecast_data %>% pivot_wider(names_from = method, values_from = forecast)
  forecast_data_wide %>% tibble::as_tibble() %>% print(n = 10, width=Inf)
  
  
  # get enrollment data for courses in forecast_data
  myopt <- opt
  myopt$aggregate <- "course"
  myopt[["term"]] <- NULL
  myopt[["course"]] <- as.list(unique(forecast_data$SUBJ_CRSE))
  
  message("getting enrollment data...")
  enrls <- get_enrl(courses,myopt)
  
  message("merging forecast summary data with enrollment summary data...")
  enrl_w_forecast <- merge (forecast_data_wide, enrls ,by=c("SUBJ_CRSE", "TERM"),all.x=T)
  print(enrl_w_forecast)
  
  message("selecting fields...")
  forecast_summary <- enrl_w_forecast[, c("TERM","SUBJ","SUBJ_CRSE","level","CRSE_TITLE","enrolled","avail","waiting","fall-to-spring", "conduit", "major")]
  
  message("computing accuracy...")
  forecast_summary <- forecast_summary %>% 
    mutate (fts_accr = `fall-to-spring`/enrolled, conduit_accr = conduit/enrolled, major_accr = major/enrolled ) 
  
  
  # add term type col
  forecast_summary <- forecast_summary %>%
    mutate(term_type = case_when(
      substring(`TERM`,5,6) == 80 ~ "fall",
      substring(`TERM`,5,6) == 10 ~ "spring",
      substring(`TERM`,5,6) == 60 ~ "summer"
    ))  
  
  # if (!is.null(opt$term)) {
  #   message("filtering for term...")
  #   forecast_summary <- filter_by_term(forecast_summary,opt$term,"TERM")
  # }
  
  forecast_summary <- forecast_summary %>% arrange(SUBJ_CRSE,term_type,TERM)
  
  message("forecast-report done and returning forecast_summary...")
  return(forecast_summary)
}




# get_forecast_data loads previously generated forecasting data
get_forecast_data <- function (students,courses,opt) {
  message("welcome to get_forecast_data! ")

  #TODO: make course optional (for general reporting)
  #TODO: don't automatically use tl_recents for term; allow override from opt
  
  # these should always be set this way
  opt$status <- "A"
  opt$uel <- TRUE

  # print(opt)
  
  # if opt has only single course/term, make it into course list 
  if (is.null(opt[["course"]]))  {
    message("please specify either a course, comma-separated list of courses, or a named list of courses (from lists.R") 
    stop()
  }
  
  course_list <- convert_param_to_list(opt[["course"]])
  
  # check if forecast.Rda exists and has data for desired course data
  # default is to use tl_recents from lists.R, usually fall and spring terms since fall 2021
  # if not, call forecast to create row and reload
  
  # load existing forecast data
  forecast_data <- load_forecasts(opt)
  
  # filter for specified course
  message("filtering for specified course: ",course_list)
  filtered_fd <- forecast_data %>% filter(SUBJ_CRSE %in% course_list) %>% filter(TERM %in% tl_recents)
  
  # if missing forecast data, create it
  # if (nrow(filtered_fd) < length(tl_recents)) {
  #   message("not enough forecast data found...forecasting!")
  #   
  #   # forecast doesn't yet return a value, but it saves an Rda file
  #   message("setting  opt$term_list to 'tl_recents' (from includes/lists.R)")
  #   opt$term_list <- "tl_recents"
  #   forecast_summary <- forecast(students,courses,opt)
  #   
  #   message("loading forecast table AGAIN...")
  #   load (forecast_rda_file) # DF is called summary
  #   filtered_fd <- forecast_data %>% filter(SUBJ_CRSE %in% course_list) %>% filter(TERM %in% tl_recents)
  # }
  # else {
  #   message("forecast data found for all specified terms!")
  # }
  # 
  
  message("returning forecast_data...")
  return(filtered_fd)
}


# function to add a new row of data for a course to the forecasts.Rda table
add_to_forecast_table <- function(new_forecast_row) {
  
  message("adding new row to forecast table...")
  print(new_forecast_row)
  
  # keep table in long format
  #TERM   SUBJ_CRSE   forecast     method
  #202410 MATH 1220   442.956522   basic
  
  target_term <- new_forecast_row[["TERM"]]
  target_course <- new_forecast_row[["SUBJ_CRSE"]]
  forecast_method <- new_forecast_row[["method"]]
  
  forecast_data <- load_forecasts(opt)
  
  if (exists("forecast_data")) {
    # remove previous rows w same course/term forecasts
    forecast_data <- forecast_data %>% filter (!(TERM == as.character(target_term) & SUBJ_CRSE == target_course & method == forecast_method))
    
    # add new forecast row to old forecast data
    forecast_data <- rbind(forecast_data,new_forecast_row)
  }
  else {
    forecast_data <- new_forecast_row
  }
  
  # save new data
  forecast_rda_file <- paste0(cedar_data_dir,"processed/forecasts.Rda")
  save(forecast_data, file=forecast_rda_file)
  message("updated ",forecast_rda_file, " with new forecast row.")
}


########### UNFINISHED
########## RESET TABLE FUNCTION 
# load Rda, remove rows for specified course, re-save
reset_forecast_course <- function(opt) {
  
  if (is.null(opt$course)) {
    stop("Please specify a course (-c or --course).")
  }
  
  # for studio use...
  opt$course <- "HIST 491"
  
  forecast_rda_file <- paste0(cedar_output_dir,"forecast/forecasts.Rda")
  load(forecast_rda_file) # loads forecast_data
  forecast_data <- forecast_data %>% filter (SUBJ_CRSE != opt$course)
  
  # save new data
  message("saving forecasts.Rda...")
  save(forecast_data,file=forecast_rda_file)
}



############### GENERAL FUNCTION START ##################3

forecast <- function(students, courses, opt) {
  message("welcome to forecast!")

  # load methods
  source(paste0(cedar_base_dir,"cones/forecast/method-fall-to-spring.R"))
  source(paste0(cedar_base_dir,"cones/forecast/method-conduit.R"))
  source(paste0(cedar_base_dir,"cones/forecast/method-major.R"))
    
  # these should always be set this way; tell user they are being overridden
  message("setting opt$uel = TRUE; opt$summer = FALSE; opt$aggregate = TRUE...")
  opt$uel <- TRUE
  opt$summer <- FALSE
  
  # for studio testing...
  # opt <- list()
  # opt$method <- "all" 
  # #opt$course <- "MATH 1240"
  # opt$term <- 202410
  
  # opt$course <- "cl_comm1000"
  # opt$term <- "springs"
  
  req_message <- "Required params: -c (course), --t (term)  
    For example: forecast -c 'BIOL 2305' -t 202480"
  
  # check for required params, and make single courses and terms into lists
  if (is.null(opt[["course"]])){
    stop(req_message, call.=FALSE)
  }
  
  if (is.null(opt[["term"]])){
    stop(req_message, call.=FALSE)
  }
  
  # unless forecasting FOR summer, we usually don't want to include it in calcs
  if (is.null(opt$summer) ){
    message("opt$summer is NULL; setting to FALSE...")
    opt$summer <- FALSE
  }
  
  # default to all forecasting methods
  if (is.null(opt$forecast_method) ){
    message ("no forecasting method specified. set --forecast_method to either fall-to-spring, conduit, major, or all. defaulting to all.")
    forecast_method <- "all"
  }
  
  # if opt has only single course/term, make it into course list for process loops
  course_list <- convert_param_to_list(opt[["course"]])
  term_list <- convert_param_to_list(opt[["term"]])
  
  # loop through courses, subsetted by year
  message("about to loop through courses in ", course_list)
  for (course in course_list) {
    
    # extract dept from course param
    dept <- subj_to_dept_map[[substring(course, 1, gregexpr(pattern = " ",course)[[1]] - 1 ) ]]
    message("setting opt$dept to: ", dept)
    opt$dept <- dept
    
    # uncomment for step-thru testing
    # course <- "COMM 1115"
    opt[["course"]] <- course
    
    message("processing course: ", course)
    
    message("about to loop through terms in ", term_list)
    for (term in term_list) {
      
      # uncomment for step-thru testing
      # term <- "202410"
      opt[["term"]] <- term
      
      message("processing term: ", term, "...")
      
      if (forecast_method == "fall-to-spring" || forecast_method == "all") {
        sum_row <- fall_to_spring_forecast(students, courses, opt)
      } 
      
      if(forecast_method == "conduit" || forecast_method == "all") {
        sum_row <- conduit_forecast(students, courses, opt)
      }

      if(forecast_method == "major" || forecast_method == "all") {
        sum_row <- major_forecast(students,opt)
      }  
      
    } # end loop through term list
  } # end loop through course list
  
  message("done looping through terms and courses!")
  
  # get full report from report func
  message("\n getting raw forecast data from table...")
  report <- get_forecast_data(students,courses,opt)
  
  # filter for course we're forecasting for and print
  report %>% tibble::as_tibble() %>% 
    filter (SUBJ_CRSE == opt[["course"]]) %>% 
    print(n = nrow(.), width=Inf)
  
  message("all done in forecast!")
  
  return(report)
  }
