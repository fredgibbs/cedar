# check for expected data files and report number of records per term and last updated date

get_data_status <- function (students = NULL, 
                             courses = NULL, 
                             academic_studies = NULL, 
                             degrees = NULL, 
                             fac_by_term = NULL) {
  
  message("Welcome to get_data_status!")
  
  status_list <- list()
  
  message("getting class list status...")
  if (!is.null(students)) {
    # students <- load_students()
    class_list_status <- students %>% group_by(`Academic Period Code`,as_of_date) %>% summarize (rows = n(), .groups="keep")
    status_list[["class_list"]] <- class_list_status
  }

  message("getting DESRs status...")
  if (!is.null(courses)) {
    # courses <- load_courses()
    DESR_status <- courses %>% group_by(`TERM`,as_of_date) %>% summarize (rows = n(), .groups="keep")
    status_list[["DESR_status"]] <- DESR_status
  }

  
  message("getting academic study status...")
  if (!is.null(academic_studies)) {
    # academic_studies <- load_academic_studies()
    academic_study_status <- academic_studies %>% group_by(term_code ,as_of_date) %>% summarize (rows = n(), .groups="keep")
    status_list[["academic_study_status"]] <- academic_study_status
  }
  
  
  message("getting degrees status...")
  if (!is.null(degrees)) {
    # degrees <- load_degrees()
    degrees_status <- degrees %>% group_by(`Academic Period Code`,as_of_date) %>% summarize (rows = n(), .groups="keep")
    status_list[["degrees_status"]] <- degrees_status
  }
  
  
  message("getting HRReport status:")
  if (!is.null(fac_by_term)) {
    # fac_by_term <- load_hr_data()
    fac_by_term_status <- fac_by_term %>% group_by(as_of_date) %>% summarize (rows = n())
    status_list[["fac_by_term_status"]] <- fac_by_term_status
  }
  
  return(status_list)
   
}
