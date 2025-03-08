# This set of functions helps forecast enrollment for specified courses and terms
# The actual methods for calculation are in separate files
# Can be run from CEDAR, and is also called from course-report.R
# REQUIRES: students, courses, opt 


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
  forecast_data <- load_forecasts()
  
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
  message("welcome to add_to_forecast_table!")
  
  print(new_forecast_row)
  
  # keep table in long format
  #TERM   SUBJ_CRSE   forecast     method
  #202410 MATH 1220   442.956522   basic
  
  target_term <- new_forecast_row[["TERM"]]
  target_course <- new_forecast_row[["SUBJ_CRSE"]]
  forecast_method <- new_forecast_row[["method"]]
  
  # load existing data or create new tibble to hold data
  forecast_data <- load_forecasts()

  # if any data in forecast table, remove previous rows w same course/term forecasts
  if (nrow(forecast_data) > 0) {
    message("removing old forecasts from target term...")
    forecast_data <- forecast_data %>% filter (!(TERM == as.character(target_term) & SUBJ_CRSE == target_course & method == forecast_method))
    
    # add new forecast row to old forecast data
    message("adding new row to forecast table...")
    forecast_data <- rbind(forecast_data,new_forecast_row)
  }
  else {
    forecast_data <- new_forecast_row
  }
  
  # save new data
  forecast_rds_file <- paste0(cedar_data_dir,"processed/forecasts.Rds")
  saveRDS(forecast_data, file=forecast_rds_file)
  message("updated ",forecast_rds_file, " with new forecast row.")
}



############### GENERAL FUNCTION START ##################3

forecast <- function(students, courses, opt) {
  message("\n Welcome to FORECAST!")

  # load methods
  #source(paste0(cedar_base_dir,"cones/forecast/method-fall-to-spring.R"))
  source(paste0(cedar_base_dir,"cones/forecast/method-conduit.R"))
  source(paste0(cedar_base_dir,"cones/forecast/method-major.R"))
    
  # these should always be set this way; tell user they are being overridden
  message("setting opt$uel = TRUE; opt$aggregate = TRUE...")
  opt$uel <- TRUE
  opt$aggregate <- "course"
  
  # for studio testing...
  # opt <- list()
  # opt$method <- "all" 
  # #opt$course <- "MATH 1240"
  # opt$term <- 202410
  
  # opt$course <- "cl_comm1000"
  # opt$term <- "tl_recents"
  
  req_message <- "Required params: -c (course), --t (term)  
    For example: forecast -c 'BIOL 2305' -t 202480"
  
  course <- opt[["course"]]
  
  # process course flag
  if (is.null(course)){
    stop(req_message, call.=FALSE)
  }
  # if course set to "forecasts", use list of courses already in forecast_table.
  else if (course == "forecasts") {
    message("course set to FORECASTS...")
    forecast_data <- load_forecasts()
    course_list <- unique(as.list(forecast_data$SUBJ_CRSE))
    opt[["course"]] <- course_list
  }
  # check if given name of .csv file
  # this allows any function to create and save a csv file on any filtering/finding logic.
  else if (substring(course,nchar(course)-3,nchar(course)) == ".csv") {
    message("course set as CSV file...")
    
    filename <- paste0(cedar_output_dir,"/csv/",course)
    message("attempting to load: ", filename)
    
    if (file.exists(filename)) {
      message("loading CSV data...")
      csv_courses <- read.csv(filename)
    }
    else {
      stop("Sorry, cannot find CSV file.")
    }
    
    # check for basic SUBJ_CRSE column to provide helpful error message.
    if(!"SUBJ_CRSE" %in% colnames(csv_courses)) {
      stop("CSV file loaded, but no SUBJ_CRSE column.")
    }
    else {  
      # convert to list from tibble
      course_list <- as.list(csv_courses$SUBJ_CRSE)
      
      # pretend input param is a list of courses
      # term should remain as user opt original
      opt[["course"]] <- course_list
      
      message("finished loading CSV file as course list!")
      print(course_list)
    }
  }
  else { # opt$course not null; make course_list from course param
      course_list <- convert_param_to_list(opt[["course"]])
  }
  
  # process term param
  if (is.null(opt[["term"]])) {
    stop(req_message, call.=FALSE)
  }
  else {
    # if opt has only single course/term, make it into course list for process loops
    message("found opt$term...")
    #TODO: make sure this is a usable term list or error out
    term_list <- convert_param_to_list(opt[["term"]])
  }

  # if a custom conduit is supplied, there should be is only one term to forecast for
  if (!is.null(opt[["forecast_conduit_term"]])) {
      termstring <- opt[["forecast_conduit_term"]]
    # double check we have a pair of terms, one for conduit, and one of term to apply it (otherwise we use usual target-1)
    if (grepl("[0-9]{6},[0-9]{6}",opt[["forecast_conduit_term"]])) {
      # extract start and end term codes
      opt[["custom_conduit"]] <- substring(termstring, 1,6)
      opt[["conduit_for_term"]] <- substring(termstring, 8,13)
    } 
  } # end custom conduit

# unless forecasting FOR summer, we usually don't want to include it in calcs
if (is.null(opt$summer) ){
  message("opt$summer is NULL; setting to FALSE...")
  opt$summer <- FALSE
}

  # default to all forecasting methods
  if (is.null(opt$forecast_method) ){
    message ("no forecasting method specified. set --forecast_method to conduit, major, or all. defaulting to all.")
    forecast_method <- "all"
  } else {
    forecast_method <- opt[["forecast_method"]]
  }
  
  
  # loop through courses, subsetted by year
  total_courses <- length(course_list)
  message("about to loop through ",total_courses ," courses:")
  print(course_list)
  counter <- 1
  
  for (course in course_list) {
    course <- as.character(course)
    message("\n FORECAST now processing course ",counter," of ",total_courses,": ",course,"...")
    
    # uncomment for studio testing
    # course <- "CHEM 1120C"
    myopt <- opt
    myopt[["course"]] <- course
    
    message("processing course: ", course)
    
    message("about to loop through terms in ", term_list)
    for (term in term_list) {
      
      # uncomment for studio testing
      # term <- "202180"
      
      myopt[["term"]] <- term
      
      message("processing term: ", term, "...")
      
      # if (forecast_method == "fall-to-spring" || forecast_method == "all") {
      #   sum_row <- fall_to_spring_forecast(students, courses, opt)
      # } 
      
      if(forecast_method == "conduit" || forecast_method == "all") {
        sum_row <- conduit_forecast(students, courses, myopt)
      }

      if(forecast_method == "major" || forecast_method == "all") {
        sum_row <- major_forecast(students, courses, myopt)
      }  
      
    } # end loop through term list
    counter <- counter + 1
  } # end loop through course list
  
  message("done looping through terms and courses!")
  
  message("all done in forecast!")
  
  return(opt)
  }
