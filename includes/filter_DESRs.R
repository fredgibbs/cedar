
check_num_rows <- function(courses) {
  message("current courses data has ",nrow(courses)," rows.")
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
    message("filtering by campus...")
    courses <- courses %>% filter (CAMP==opt$campus) 
    check_num_rows(courses)
  } else {
    message("filtering with campus defaults: ABQ or EA...")
    check_num_rows(courses)
  }
  
  # filter by college
  if (!is.null(opt$college)) {
    message("filtering by college: ",opt$college,"...")
    courses <- courses %>% filter (COLLEGE == opt$college)
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
  
  
  # filter by dept
  # NOTE: the DEPT field is created when DESRs are parsed from MyReports.
  # See subj_to_dept_map in mappings.R
  if (!is.null(opt[["dept"]])) {
    message("filtering by department: ",opt[["dept"]],"...")
    param_to_list <- convert_param_to_list(opt[["dept"]])
    courses <- courses %>% filter (DEPT %in% param_to_list)
    check_num_rows(courses)
  }
  
  # filter by subject code
  if (!is.null(opt[["subj"]])) {
    message("filtering by subject code...")
    param_to_list <- convert_param_to_list(opt[["subj"]])
    courses <- courses %>% filter (SUBJ %in% param_to_list)
    check_num_rows(courses)
  } 
  
  # filter by CRN
  if (!is.null(opt$crn)) {
    message("filtering by CRN...")
    courses <- courses %>% filter (CRN==opt$crn)
    check_num_rows(courses)
  }
  
  # filter by course
  if (!is.null(opt[["course"]])) {
    message("filtering by course...")
    param_to_list <- convert_param_to_list(opt[["course"]])
    #message(param_to_list)
    courses <- courses %>% filter (SUBJ_CRSE %in% param_to_list)
    check_num_rows(courses)
  }
  
  # filter by part of term (PT)
  if (!is.null(opt$pt)) {
    pt_str <- paste("PT=='",opt$pt,"'",sep="")
    print(paste("filtering by part of term: ",pt_str))
    courses <- courses %>% filter (!!rlang::parse_expr(pt_str))
    check_num_rows(courses)
  }
  
  # filter by instructor
  if (!is.null(opt[["inst"]])) {
    message("filtering by instructor...")
    param_to_list <- convert_param_to_list(opt[["inst"]])
    courses <- courses %>% filter (PRIM_INST_LAST %in% param_to_list)
    check_num_rows(courses)
  }
  
  # filter by anti-instructor
  if (!is.null(opt$notinst)) {
    message("filtering by NOT instructor: ",opt$notinst,"...")
    
    # check for multiple values
    if (grepl(",",opt$notinst)) {
      message("using instructor string...")
      courses <- courses %>% filter (!PRIM_INST_LAST %in% as.list(strsplit(opt$notinst, ",")[[1]]))
    }
    else {
      message("using single instructor value...")
      courses <- courses %>% filter (PRIM_INST_LAST != opt$notinst) 
    }
    check_num_rows(courses)
  }
  
  # filter by anti-list name 
  if (!is.null(opt[["not_inst_list"]])) {
    message("filtering by NOT instructor via named list: ",opt[["not_inst_list"]],"...")
    faculty <- as.list(names(get(opt[["not_inst_list"]])))
    message(faculty)
    courses <- courses %>% filter (!INST_NAME %in% faculty)
    check_num_rows(courses)
  }
  
  # filter by gen ed
  if (!is.null(opt[["gen_ed"]])) {
    message("filtering by gen ed: ", opt[["gen_ed"]])
    param_to_list <- convert_param_to_list(opt[["gen_ed"]])
    courses <- courses %>% filter  (gen_ed_area %in% param_to_list)
    check_num_rows(courses)
  }  
  
  
  # filter by level
  # course_type is set in in parse-DESRs when loading excel files from MyReports
  course_type <- opt$level
  
  filter_string <- case_when(
    course_type == "undergrad" ~ "crse_base > 1000 | crse_base < 500",
    course_type == "grad" ~ "crse_base >= 500 & crse_base < 700",
    course_type == "upper" ~ "crse_base >= 300 & crse_base < 500",
    course_type == "lower" ~ "crse_base >= 1000"
  )
  
  if (!is.null(opt$level) && opt$level != "all") {
    message("filtering by level: ",opt$level,"...")
    courses <- courses %>% filter (!!rlang::parse_expr(filter_string))
    check_num_rows(courses)
  }
  
  
  # filter by course status
  if (!is.null(opt$status)) {
    message("filtering by status:",opt$status)
    courses <-courses  %>% filter (STATUS==opt$status)
    check_num_rows(courses)
  }
  
  # # check summer filtering flag
  # if (!is.null(opt$summer) && opt$summer == FALSE) {
  #   message("filtering out summer...")
  #   courses <- courses %>% filter (!str_detect(as.character(TERM), "60"))
  #   check_num_rows(courses)
  # }
  # 

  # crosslist processing
  if (!is.null(opt$crosslist)) {
    source("includes/xlist.R")
    courses <- xlist_filter(courses,opt$crosslist)
    check_num_rows(courses)
  }
  

  # term filter
  if (length(opt[["term"]]) > 0 || !is.null(opt[["term"]])) { 
    courses <- filter_by_term(courses,opt[["term"]],"TERM")
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

  # filter academic title
  if (!is.null(opt$title)) {
    message("using academic title...")
    courses <- courses %>% filter( (`Academic Title` == opt$title) )
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
  
  #courses <- courses %>% select (TERM,CRN,DEPT,SUBJ,CRSE,SUBJ_CRSE,level,SECT,CRSE_TITLE,INST_METHOD,PT,PRIM_INST_LAST,INST_NAME, `Academic Title`,job_cat,`Home Organization Desc`,ENROLLED,total_enrl,TOTAL_HOURS,XL_SUBJ,XL_CRSE,XL_CRN, XL_ENRL,XL_CODE,WAIT_COUNT,SEATS_AVAIL,crse_base) %>% arrange(TERM,SUBJ,crse_base, SECT, INST_NAME)
  
  return(courses)
  
}
