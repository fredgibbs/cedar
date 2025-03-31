# This set of functions helps forecast enrollment for specified courses and terms
# The actual methods for calculation are in separate files


########### UNFINISHED; code just for manual use; this never gets run
########## RESET TABLE
# load Rds, remove rows for specified course, re-save
reset_forecast_course <- function(opt) {
  
  if (is.null(opt$course)) {
    stop("Please specify a course (-c or --course).")
  }
  
  # for studio use...
  # opt$course <- "HIST 491"
  
  # even though load_forecast_data can be used, we still need to set the filename for saving
  forecast_rds_file <- paste0(cedar_data_dir,"/processed/forecasts.Rds")
  forecast_data <- readRDS(forecast_rds_file) # loads forecast_data
  
  # a few ways of cleaning data for studio use.
  # here we can find things we don't want...
  unique(forecast_data$SUBJ_CRSE)
  forecast_data <- forecast_data %>% filter (SUBJ_CRSE != "HIST 1160")
  forecast_data <- forecast_data[grep("JAPN",forecast_data$SUBJ_CRSE, invert=TRUE),]
  forecast_data <- forecast_data[grep(" [0-9]{3}$",forecast_data$SUBJ_CRSE, invert=TRUE),]
  
  # save new data
  message("saving forecasts.Rds...")
  saveRDS(forecast_data, file=forecast_rds_file)
}




# function to add a new row of data for a course to the forecasts.Rda table
add_to_forecast_table <- function(new_forecast_row) {
  message("\nWelcome to add_to_forecast_table!")
  print(new_forecast_row)
  
  # check for empty table
  if (nrow(new_forecast_row) == 0 ) {
    message("new row data is empty.")
    return()
  }
  
  # always refresh data
  forecast_data <- load_forecasts()

  # if any data in forecast table, remove previous rows with the same course/term forecasts
  fd_rows <- nrow(forecast_data)
  if (fd_rows > 0) {
    message("removing old forecasts from target term...")
    
    # anti-join by all columns except forecast
    forecast_data <- forecast_data %>% anti_join(new_forecast_row, by = c("CAMP", "COLLEGE", "TERM", "SUBJ_CRSE", "method"))
    message("removed ", fd_rows - nrow(forecast_data), " rows.")
    
    # add new forecast row to old forecast data
    message("adding new row to forecast table...")
    forecast_data <- rbind(forecast_data,new_forecast_row)
  }
  else {
    forecast_data <- new_forecast_row
  }
  
  # save new data
  if (as.logical(Sys.getenv("shiny"))) {
    message("trying to save forecasts in Shiny...")
    forecast_rds_file <- "forecasts.Rds"
  }
  else {
    forecast_rds_file <- paste0(cedar_data_dir,"processed/forecasts.Rds")  
  }
  
  saveRDS(forecast_data, file=forecast_rds_file)
  message("updated ",forecast_rds_file, " with new forecast row.")
}



############### GENERAL FUNCTION START ##################3

forecast <- function(students, courses, opt) {
  message("\nWelcome to FORECAST!")

  # load methods
  source(paste0(cedar_base_dir,"cones/forecast/method-conduit.R"))
  source(paste0(cedar_base_dir,"cones/forecast/method-major.R"))
    
  # for studio testing...
  # opt <- list()
  # opt$method <- "all" 
  # #opt$course <- "MATH 1240"
  # opt$term <- 202410
  
  # opt$course <- "cl_comm1000"
  # opt$term <- "tl_recents"
  
  # these should always be set this way; tell user they are being overridden
  message("setting opt$uel = TRUE")
  opt$uel <- TRUE

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
      
      if(forecast_method == "conduit" || forecast_method == "all") {
        conduits <- conduit_forecast(students, courses, myopt)
        add_to_forecast_table(conduits)
      }

      if(forecast_method == "major" || forecast_method == "all") {
        majors <- major_forecast(students, courses, myopt)
        add_to_forecast_table(majors)
      }  
      
    } # end loop through term list
    counter <- counter + 1
  } # end loop through course list
  
  message("done looping through terms and courses! returning new forecasts data...")
  forecast_data <- load_forecasts()
  
  return(forecast_data)
  }
