# set_payload prepares the d_params object for the Rmd file  
set_payload <- function (dept_code, prog_focus) {
  
  # set program codes
  if (!is.na(prog_focus)) {
    # if program focus specified
    prog_codes <- prog_focus
  } else {
    # get all program codes associated with the dept; use prgm_to_dept_map
    prog_codes <- names(prgm_to_dept_map[which(prgm_to_dept_map == dept_code)])
  }
  
  
  # to get program name variants that appear in MyReports
  # TODO: any reason to use prog_names rather than prog_codes to filter MyReports data?
  prog_names <- names(major_to_program_map[which(major_to_program_map %in% prog_codes)])
  
  # payload
  d_params <- list("dept_code" = dept_code,
                   "dept_name" = dept_code_to_name[dept_code],
                   "subj_codes" = names(subj_to_dept_map[which(subj_to_dept_map == dept_code)]),
                   "prog_focus" = prog_focus,
                   "prog_names" = prog_names,
                   "prog_codes" = prog_codes,
                   "tables" = list(),
                   "plots" = list(),
                   "term_start" = cedar_report_start_term,
                   "term_end"  = cedar_report_end_term,
                   "palette" = cedar_report_palette
  )
  
  return (d_params)
}


################ main function ################
dept_report <- function (students,courses,opt) {
  message("Welcome to dept_report!")
  
  # convert dept param to dept_list for processing
  dept_list <- convert_param_to_list(opt[["dept"]]) 
  
  # set processed data dir
  data_processed_dir <- paste0(cedar_data_dir,"processed/")
  
  # for studio testing...
  #opt <- list()
  #opt$output_format <- "aspx" # either aspx or html. use aspx for easier loading if moving to onedrive
  #opt$dept <- "LCL"
  #opt$prog <- ""
  
  # just for line-by-line testing w/ single unit; comment these two out for regular use 
  #message("setting unit to opt$dept for testing...")
  #unit <- "AMST"
  
  # loop through each unit in dept list
  for (unit in dept_list) {
    message("looking at unit ", unit)
    message("length:", length(unit))
    
    dept_code <- ""
    prog_focus <- NA
    
    if (length(unit) == 2) {
      dept_code <- unlist(unit)[1]
      prog_focus <- unlist(unit)[2]
    } else {
      dept_code <- unit
      prog_focus <- NA
    }
    
    message("setting payload with dept_code ",dept_code," and prog_focus ",prog_focus,"...")
    d_params <- set_payload (dept_code,prog_focus)
    
    
    ############ 
    # run scripts to prepare data and images
    ############
    
    ####### HEADCOUNT
    if (file.exists(paste0(data_processed_dir,"academic-study.feather"))) {
      d_params <- get_headcount_data_for_dept_report(d_params)
      # produces: headcount-by-year.png, headcount_filtered_by_level_dept.Rda (hc_filtered)
    } else {
      stop("headcount.Rda not found in ",data_processed_dir,". You must first run data/parsers/parse-academic-study.R.")
    }
    
    ####### DEGREES
    if (file.exists(paste0(data_processed_dir,"degrees.Rda"))) {
      #source("degrees/degrees-for-rmd.R") 
      d_params <- get_degrees_for_dept_report(d_params)
      # produces: degrees-for-program-by-year.png; degrees-for-program-by-year.Rda (degrees_for_program)
    } else {
      stop("degrees.Rda not found in ",data_processed_dir,". You must first run data/parsers/parse-degrees.R.")
    }
    
    ####### CREDIT HOURS  
    # this uses just class list data, so no file checks necessary
    d_params <- get_credit_hours_for_dept_report(students, d_params)
    
    ####### CREDIT HOURS BY MAJOR
    d_params <- credit_hours_by_major(students,d_params)
    
    ####### CREDIT HOURS BY FACULTY
    d_params <- credit_hours_by_fac(students,d_params)
    
    ####### GRADES
    d_params <- get_grades_for_dept_report(students,opt,d_params)
    
    ####### ENROLLMENT
    d_params <- get_enrl_for_dept_report(courses, opt, d_params)
    
    ####### SFRs
    d_params <- get_sfr_data_for_dept_report(d_params)

    # set output data
    if (!is.na(d_params$prog_focus)) {
      output_filename <- paste0(d_params$dept_code, "-", d_params$prog_focus)
    } else {
      output_filename <- d_params$dept_code
    }
    
    d_params$output_filename <- output_filename
    d_params$rmd_file <- "cones/dept-report/dept-report.Rmd"
    d_params$output_dir_base <- paste0(cedar_output_dir,"dept-reports/")
    
    # create report (defined in misc_funcs.R)
    create_report(opt, d_params)
    
  }
  
  
}
