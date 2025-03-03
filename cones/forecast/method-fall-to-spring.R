############ FALL TO SPRING FORECAST #####################
# For a given course, this looks at the percent change in that course's enrollment from the previous semester 
# (relative to given course), and applies that difference to the enrollment for that course a year prior.

# This works best for SPRING predictions, since there is more enrollment continuity moving from fall to spring than spring to fall.
# This method makes most sense for courses offered every term, as it assumes there is some relatively stable relationship between enrollments across terms.
# Summer is set to FALSE by default. Summer forecasting is better handled by summer-report

# This function itself is term agnostic, but requires a target term anyway to match functionality of other forecasting methods just by simple filtering at the end

# This method is BASIC, obviously, but can be useful for an average elective class, and maybe some routine program courses.

fall_to_spring_forecast <- function(students, courses, opt) {
  message("welcome to basic forecasting!")
  
  #uncomment for studio testing
  # students <- load_students()
  # courses <- load_courses()
  # opt <- list()
  # opt$term <- "202480"
  # opt$course <- "HIST 491"

  target_term <- opt[["term"]]
  target_course <- opt[["course"]]
  
  myopt <- opt # preserve original opt, but need to modify for immediate filtering
  myopt[["term"]] <- NULL

  # ignore summer terms unless forecasting for it  
  if (get_term_type(target_term) == "Summer") {
    myopt$summer <- TRUE  
  } else {
    myopt$summer <- FALSE   
  }
  

  myopt$aggregate <- "course"
  
  # message("myopts:")
  # print(myopt)
  
  # if using 1215X data, filter out everything before 202010, since that's the oldest data for 1215X
  if (grepl("MATH 1215", target_course)) {
    message("filtering out pre-202010 courses...")
    courses <- courses %>% filter (TERM >= 202010)
  }
  
  # get summarized enrollments for course (courses,subj_crse,term)
  message("getting aggregate enrollment totals...")
  course_enrls <- get_enrl(courses,myopt) %>% 
    group_by(SUBJ_CRSE,TERM) %>% 
    select (TERM, SUBJ_CRSE, enrolled)

  # if trying to forecast for a future semester that doesn't have a DESR yet, we need to add a row manually
  # this can be known if target term is greater than last term in summary DF
  # NOTE: if this errors out because summary table is empty, usually the course doesn't exist
  current_term <- course_enrls$TERM[nrow(course_enrls)]
  
  if (target_term > current_term) {
    # get last row of each group (course)
    project_row <- course_enrls[nrow(course_enrls),]
    project_row$TERM <- as.character(target_term)
    project_row$enrolled <- 0
    
    course_enrls <- course_enrls %>% ungroup() %>% add_row(project_row) %>% group_by(TERM,SUBJ_CRSE)
  }
  
  # compute difference between terms
  # pct diff is the percent difference b/w previous term (each row in the DF) and the previous year.
  course_enrls$pct_diff <- lag(course_enrls$enrolled,n=1) / lag(course_enrls$enrolled,n=3) 
  course_enrls$forecast <-  course_enrls$pct_diff * lag(course_enrls$enrolled,n=2)
  course_enrls$accuracy <-   course_enrls$forecast / course_enrls$enrolled
  
  # print
  message("enrollment projections:") 
  course_enrls %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)

    
  # output from this method includes all terms,
  # but until i can automatically get other terms in other functions, 
  # just take the target_term
  course_enrls <- course_enrls %>%  filter(TERM == target_term)
  new_forecast_row <- course_enrls %>% select (TERM,SUBJ_CRSE,forecast)
  new_forecast_row$method = "fall-to-spring"

  # add to forecast table 
  add_to_forecast_table(new_forecast_row)
    
} # end basic_forecast function


