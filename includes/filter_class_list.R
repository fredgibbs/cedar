
# "students" DF has same fields as Guided Class List Adhoc report
filter_class_list <- function(students, opt) {
  
  message("welcome to filter_class_list!")
  message("filtering class list with supplied options...")
  
  # filter by campus
  if (!is.null(opt$campus)) {
    message("filtering by campus...")
    students <- filter_by_col(students,"Course Campus Code",opt[["campus"]])
  
  } else {
    # if (opt$verbose) print("using campus defaults: ABQ or EA")
    # students <- students %>% filter (`Course Campus Code`=="ABQ" | `Course Campus Code`=="EA") 
  }
  
  # filter by student college
  if (!is.null(opt$studentcollege)) {
    message("filtering by student college...")
    students <- filter_by_col(students,"Course College Code",opt[["studentcollege"]])
  }
  
  # filter by course college
  if (!is.null(opt$coursecollege)) {
    message("filtering by course college...")
    students <- students %>% filter (`Course College Code` == opt$coursecollege) 
  }
  
  # filter by student classification
  # use "Freshman" opt param as shortcut for both 1st and 2nd semester categories
  if (!is.null(opt$classification)) {
    message("filtering by student classification...")
    if (opt$classification == "Freshman") {
      message ("using Freshman to mean both 1st and 2nd semester...")
      query_str <- "`Student Classification` == 'Freshman, 1st Yr, 1st Sem' | `Student Classification` == 'Freshman, 1st Yr, 2nd Sem'" 
      students <- students %>% filter (!!rlang::parse_expr(query_str)) 
    }
    else {
      students <- students %>% filter (`Student Classification` == opt$classification) 
    }
  }
  
  # filter by registration status
  if (!is.null(opt$registration_status_code)) {
    message("filtering by registration status code: ", opt$registration_status_code)
    param_to_list <- convert_param_to_list(opt[["registration_status_code"]])
    students <- students %>% filter (`Registration Status Code` %in% param_to_list)
  }
  
  
  # filter by instruction method
  # this is the best way to identify/filter AOP courses
  if (!is.null(opt$im)) {
    students <- students %>% filter (`Instruction Delivery Mode Code` %in% as.list(strsplit(opt$im, ",")[[1]]))
  }
  
  
  # filter by dept
  # NOTE: this depends on DEPT being assigned from the data parsers that processes Excel files from MyReports...
  if (!is.null(opt[["dept"]])) {
    message("filtering by department...")
    param_to_list <- convert_param_to_list(opt[["dept"]])
    students <- students %>% filter (DEPT %in% param_to_list)
  }
  
  # Subject Code filter
  if (!is.null(opt$subj)) {
    message("filtering by subject code...")
    students <- students %>% filter (`Subject Code`==opt$subj) 
  }
  
  # CRN filter
  if (!is.null(opt$number)) {
    message("filtering by course number ")
    students <- students %>% filter (`Course Number`==opt$number) 
  }
  
  
  # COURSE filter
  if (!is.null(opt[["course"]])) {
    message("filtering by course...")
    students <- filter_by_col(students,"SUBJ_CRSE",opt[["course"]])
  }  
  
  # MAJOR filter
  message("figuring out major filtering...")
  
  if (!is.null(opt[["major"]])) {
    if (grepl(",", opt[["major"]])) {
      message("filtering from comma separated string (",opt[["major"]],")...")
      students <- students %>% filter (Major %in% as.list(strsplit(opt[["major"]], ",")[[1]])) 
    }
    else {
      major_str <- paste("`Major`=='",opt$major,"'",sep="")
      message("filtering by single major: ", major_str)
      students <- students %>% filter (!!rlang::parse_expr(major_str))
    }
  }
  
  
  # PT (part of term) filter
  if (!is.null(opt$pt)) {
    pt_str <- paste("`Sub-Academic Period Code`=='",opt$pt,"'",sep="")
    print(paste("filtering by part of term: ",pt_str))
    students <- students %>% filter (!!rlang::parse_expr(pt_str))
  }
  
  
  # INSTRUCTOR filter
  if (!is.null(opt[["inst"]])) {
    message("filtering by instructor...")
    param_to_list <- convert_param_to_list(opt[["inst"]])
    students <- students %>% filter (`Primary Instructor Last Name` %in% param_to_list)
  }
  
  
  # filter by level
  if (!is.null(opt$level)) {
    message("filtering by level...")
    students <- students %>% filter (level == opt$level)
  } 
  else {
    message("no level specified.")
  }
  
  
  # # summer filtering
  # if (!is.null(opt$summer) && opt$summer == FALSE) {
  #   message("filtering out summer...")
  #   students <- students %>% filter (!str_detect(as.character(`Academic Period Code`), "60"))
  # }
  # 
  # term filter
  if (length(opt[["term"]]) > 0 || !is.null(opt[["term"]])) { 
    message("processing term param: ", opt[["term"]])
    students <- filter_by_term(students,opt[["term"]],"Academic Period Code")
  } 
  

  # filter out courses on excluded courses list; see excluded-courses.R
  if (!is.null(opt$uel)) {
    message("using excluded courses list...")
    students <- students %>% subset( !(SUBJ_CRSE %in% excluded_courses) )
  }
  
  
  message("done filtering. returning class list with ",nrow(students)," rows.")
  
  #students <- students %>% select (`Academic Period Code`,`Student ID`, `Student Classification`, `Program Classification`,`Student College`,Major, `Course Reference Number`,`Subject Code`,`Short Course Title`,SUBJ_CRSE,`Sub-Academic Period`,level,`Course Section Number`,`Instruction Delivery Mode Code`,`Primary Instructor Last Name`,crse_base) %>% arrange(`Academic Period Code`,SUBJ_CRSE)
  
  
  return(students)
  
}
