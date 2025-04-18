# check for expected data files and report number of records per term and last updated date

get_data_status <- function (students = NULL, 
                             courses = NULL, 
                             academic_studies = NULL, 
                             degrees = NULL, 
                             fac_by_term = NULL) {
  
  message("Welcome to get_data_status!")
  
  data_status <- tibble(
    MyReport = character(),
    Term = character(),
    Last_Updated = character(),
    Num_Rows = numeric()
  )
  
  
  normalize_summary <- function(list_name, term_col, summary, data_status) {
    summary <- summary %>% 
      rename(`Term` = !!term_col, 
             `Last_Updated` = as_of_date, 
             `Num_Rows` = rows)
    
    summary <- summary %>% add_column(MyReport = list_name, .before = "Term")
    
    data_status <- rbind(data_status, summary)
    
    return(data_status)
  }
  
  
  message("getting class list status...")
  if (!is.null(students)) {
    # students <- load_students()
    summary_status <- students %>% group_by(`Academic Period Code`,as_of_date) %>% summarize (rows = n(), .groups="keep")
    
    data_status <- normalize_summary("class_list", "Academic Period Code", summary_status, data_status)
  }

  message("getting DESRs status...")
  if (!is.null(courses)) {
    # courses <- load_courses()
    summary_status <- courses %>% group_by(`TERM`,as_of_date) %>% summarize (rows = n(), .groups="keep")
    
    data_status <- normalize_summary("DESR", "TERM", summary_status, data_status)
  }

  
  message("getting academic study status...")
  if (!is.null(academic_studies)) {
    # academic_studies <- load_academic_studies()
    summary_status <- academic_studies %>% group_by(term_code ,as_of_date) %>% summarize (rows = n(), .groups="keep")
    data_status <- normalize_summary("academic_study", "term_code", summary_status, data_status)
  }
  
  
  message("getting degrees status...")
  if (!is.null(degrees)) {
    # degrees <- load_degrees()
    summary_status <- degrees %>% group_by(`Academic Period Code`,as_of_date) %>% summarize (rows = n(), .groups="keep")
    data_status <- normalize_summary("degree", "Academic Period Code", summary_status, data_status)
  }
  
  
  message("getting HRReport status:")
  if (!is.null(fac_by_term)) {
    # fac_by_term <- load_hr_data()
    
    summary_status <- fac_by_term %>% group_by(as_of_date) %>% summarize (rows = n())
    summary_status <- summary_status %>% add_column(`Academic Period Code` = "202510", .before = "as_of_date")
    
    data_status <- normalize_summary("hr_data", "Academic Period Code", summary_status, data_status)
    
  }
  
  return(data_status)
}
