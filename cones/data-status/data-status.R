
get_data_status <- function (opt) {
  message("Welcome to get_data_status!")
  
  # check for expected data files and report number of records per term and last updated date
  message("class list status:")
  class_list <- load_students(opt)
  class_list_status <- class_list %>% group_by(`Academic Period Code`,as_of_date) %>% summarize (rows = n())
  class_list_status %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  message("DESRs status:")
  DESRs <- load_courses(opt)
  DESRs_status <- DESRs %>% group_by(`TERM`,as_of_date) %>% summarize (rows = n())
  DESRs_status %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  message("academic study status:")
  academic_study <- load_academic_study()
  academic_study_status <- academic_study %>% group_by(term_code ,as_of_date) %>% summarize (rows = n())
  academic_study_status %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  # TODO: need to add as_of_date to degree-parser.R and degrees.Rda
  message("degrees status:")
  load(paste0(cedar_data_dir,"processed/degrees.Rda")) # load degrees DF
  degrees_status <- degrees %>% group_by(`Academic Period Code`,as_of_date) %>% summarize (rows = n())
  degrees_status %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  # TODO: need to add as_of_date to parse-HRreport.R and fac_by_term.Rda
  message("HRReport status:")
  load(paste0(cedar_data_dir,"processed/fac_by_term.Rda")) # load fac_by_term DF
  fac_by_term_status <- fac_by_term %>% group_by(as_of_date) %>% summarize (rows = n())
  fac_by_term_status %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
}