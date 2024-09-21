# In addition to terminal output, saves a CSV file that shows the aggregated wait list across sections of a course
# Students are already registered are NOT included
# Terminal display shows waiting and registered AND waiting and not registered (by major and by classification), 

# No required params, but this is more useful to use filtering params (usually by course)

inspect_waitlist <- function (students,opt) {

  #### for studio testing
  # opt <- list()
  # opt$course <- "MATH 1430"
  # opt$term <- 202410
  # opt$pt <- "2H"
  
  message("welcome to inspect_waitlist! (in waitlist.R)")
  
  message("filtering students from params...")
  filtered_students <- filter_class_list(students,opt) 
  
  filtered_students <- filtered_students %>% 
    select (`Academic Period Code`,SUBJ_CRSE,CRSE_TITLE,`Student ID`,`Student Name`,`Student Classification`,`Student College`, Major, `Registration Status`,`Schedule Attribute Code`) %>% 
    distinct(`Student ID`, `Registration Status`, .keep_all = T)
  
  message("getting students with registration status as 'Wait Listed'...")
  waiting <- filtered_students %>% filter (`Registration Status` == "Wait Listed")
  waitingids <- waiting$`Student ID`
  message("All waitlisted students:")
  waiting %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  message("number of waitlisted students: ",nrow(waiting))
  
  # using IDs from waitlists, get list of students registered for same course (across all sections)
  waiting_and_registered <- filtered_students %>% filter (`Student ID` %in% waitingids & `Registration Status` %in% c("Student Registered", "Registered" ))  
  war_ids <- waiting_and_registered$`Student ID`
  waiting_and_registered <- add_acad_year(waiting_and_registered,"Academic Period Code")
  
  message("students waiting AND registered:")
  waiting_and_registered %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  message("number of waitlisted AND registered students: ",nrow(waiting_and_registered))
  
  message("summary of students waiting AND registered:")
  war_summary <- waiting_and_registered %>% group_by(`Academic Period Code`,`Student Classification`,Major,`Schedule Attribute Code`) %>% 
    summarize(total = n()) 
  war_summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  message("students waiting AND NOT registered:")
  just_waiting <- filtered_students %>% filter (`Registration Status` == "Wait Listed" & !(`Student ID` %in% war_ids))
  just_waiting %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  message("number of waitlisted AND NOT registered students: ",nrow(just_waiting))

  message("summary of students waitlisted and NOT registered:")  
  jm_summary <- just_waiting %>% group_by(`Academic Period Code`,`Student Classification`,Major,`Schedule Attribute Code`) %>% 
    summarize(total = n()) 
  jm_summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  message("number of waitlisted students NOT registered: ",nrow(waiting))

  message("summary of students waiting and NOT registered:")
  jm_summary <- just_waiting %>% group_by(`Academic Period Code`,Major,`Schedule Attribute Code`) %>% summarize(total = n()) 
  jm_summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  message("number of waitlisted students by MAJOR and NOT registered: ",nrow(waiting))
  
  message("summary of students waiting and NOT registered:")
  jm_summary <- just_waiting %>% group_by(`Academic Period Code`,`Student Classification`,`Schedule Attribute Code`) %>% summarize(total = n()) 
  jm_summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  message("number of waitlisted students by CLASSIFICATION and NOT registered: ",nrow(waiting))
  
  message("saving students waiting and NOT registered as waitlist_demand.csv...")
  write.csv(just_waiting, paste0(cedar_output_dir,"waitlist/waitlist_demand.csv"), row.names=FALSE)
  
  message("all done inspecting waitlist!")
  
  return(just_waiting)
}
