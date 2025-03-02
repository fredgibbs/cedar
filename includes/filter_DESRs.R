
check_num_rows <- function(courses) {
  message("current courses data has ",nrow(courses)," rows.")
}


filter_by_col <- function(courses, col, val) {
  message("filtering by ",col, "=", val)
  
  param_to_list <- convert_param_to_list(val)
  message(param_to_list)
  
  ## use get instead of {{ }} because cal is passed in as a string, rather than a variable
  courses <- courses %>% filter (get(col) %in% param_to_list)
  
  check_num_rows(courses)
  
  return(courses)
}



filter_DESRs <- function(courses, opt) {
  message("Filtering DESRs with supplied options...")
  check_num_rows(courses)
  
  if (!length(opt)){
    print_help(opt_parser)
    stop("Error in filter_DESRs: Please supply at least one filter parameter", call.=FALSE)
  }
  
  # filter by campus
  if (!is.null(opt$campus)) {
    courses <- filter_by_col(courses,"CAMP",opt[["campus"]])
  }
  
  # filter by college
  if (!is.null(opt$college)) {
    courses <- filter_by_col(courses,"COLLEGE",opt[["college"]])
  }
  
  # filter by DEPT (department code)
  # NOTE: the DEPT field is created when DESRs are parsed from MyReports.
  # See subj_to_dept_map in mappings.R
  if (!is.null(opt[["dept"]])) {
    courses <- filter_by_col(courses,"DEPT",opt[["dept"]])
  }
  
  # filter by SUBJ (subject code)
  if (!is.null(opt[["subj"]])) {
    courses <- filter_by_col(courses,"SUBJ",opt[["subj"]])
  } 
  
  # filter by CRN
  if (!is.null(opt$crn)) {
    courses <- filter_by_col(courses,"CRN",opt[["crn"]])
  }

  # filter by SUBJ_CRSE (by course)
  # NOTE: the SUBJ_CRSE field is created when DESRs are parsed from MyReports.
  if (!is.null(opt[["course"]])) {
    courses <- filter_by_col(courses,"SUBJ_CRSE",opt[["course"]])
  }
  
  # term filter
  if (length(opt[["term"]]) > 0 || !is.null(opt[["term"]])) { 
    courses <- filter_by_term(courses,opt[["term"]],"TERM")
    check_num_rows(courses)  
  } 
  
  # filter by part of term (PT)
  if (!is.null(opt$pt)) {
    courses <- filter_by_col(courses,"PT",opt[["pt"]])
  }
  
  # filter by instructor
  if (!is.null(opt[["inst"]])) {
    courses <- filter_by_col(courses,"PRIM_INST_LAST",opt[["inst"]])
  }
  
  # filter by gen ed
  if (!is.null(opt[["gen_ed"]])) {
    courses <- filter_by_col(courses,"gen_ed_area",opt[["gen_ed"]])
  }  
  
  # filter by level
  if (!is.null(opt[["level"]]) && opt$level != "all") {
    courses <- filter_by_col(courses,"level",opt[["level"]])
  }
  
  # filter by course status
  if (!is.null(opt$status)) {
    message("filtering by status:",opt$status)
    courses <-courses  %>% filter (STATUS==opt$status)
    check_num_rows(courses)
  }
  
  
  # filter by instructional method
  # this is the best way to flag aop (mops) courses
  if (!is.null(opt$im)) {
    
    # provide common shortcut, since f2f courses could have either IM
    if(opt$im == "f2f") {
      opt$im <- "ENH,0,HYB"
    }
    courses <- courses %>% filter (INST_METHOD %in% as.list(strsplit(opt$im, ",")[[1]]))
    check_num_rows(courses)
  }
  
  
  # # check summer filtering flag
  # if (!is.null(opt$summer) && opt$summer == FALSE) {
  #   message("filtering out summer...")
  #   courses <- courses %>% filter (!str_detect(as.character(TERM), "60"))
  #   check_num_rows(courses)
  # }
  # 

  
  # CROSSLIST FILTER
  .xlist_filter <- function(df,action) {
    message("Welcome to xlist.R!")
    
    if (action == "exclude") {
      print("excluding cross-listed courses (by XL_CRN = 0)...")
      df <- df %>% filter(XL_CRN == "0")
      return(df)
    }
    
  # crosslist processing
  if (!is.null(opt$crosslist)) {
    courses <- .xlist_filter(courses,opt$crosslist)
    check_num_rows(courses)
  }
  
  # enrollment min
  if (!is.null(opt$enrl_min)) {
    message("using enrollment minimum of ",opt$enrl_min,  "...")
    courses <- courses %>% filter (total_enrl >= as.integer(opt$enrl_min)) 
    check_num_rows(courses)
  }
  
  # enrollment max
  if (!is.null(opt$enrl_max)) {
    message("using enrollment maximum of ",opt$enrl_max,  "...")
    courses <- courses %>% filter (total_enrl <= as.integer(opt$enrl_max))
    check_num_rows(courses)
  }
  
  # filter out courses on excluded courses list; see excluded-courses.R
  if (!is.null(opt$uel)) {
    message("using excluded courses list...")
    courses <- courses %>% subset( !(SUBJ_CRSE %in% excluded_courses) )
    check_num_rows(courses)
  }

  # job category from parsing HR Report
  if (!is.null(opt$job_cat)) {
    param_to_list <- convert_param_to_list(opt[["job_cat"]])
    courses <- courses %>% filter  (job_cat %in% param_to_list)
    check_num_rows(courses)
  }
  
  # set default groupings for output
  courses <- courses %>% group_by(TERM,SUBJ_CRSE,CRSE_TITLE,PT,INST_METHOD,level,INST_NAME)
  
  # dedupe based on CRN
  courses <- courses %>% distinct(CRN,.keep_all = TRUE)
    
  message("done filtering DESRs. returning ",nrow(courses)," courses...\n")
  
  return(courses)
  
}
