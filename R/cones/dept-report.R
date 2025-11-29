# set_payload prepares the d_params object for the Rmd file  
set_payload <- function (dept_code, prog_focus = NULL) {
  message("[dept-report.R] Welcome to set_payload!")

  
  # set program codes
  message("[dept-report.R] setting program codes and names...")

  if (!is.null(prog_focus)) {
    # if program focus specified
    prog_codes <- prog_focus
  } else {
    # get all program codes associated with the dept; use prgm_to_dept_map
    prog_codes <- names(prgm_to_dept_map[which(prgm_to_dept_map == dept_code)])
    message("[dept-report.R] prog_codes: ", paste(prog_codes, collapse=", "))
  }
  
  
  # to get program name variants that appear in MyReports
  # TODO: any reason to use prog_names rather than prog_codes to filter MyReports data?
  prog_names <- names(major_to_program_map[which(major_to_program_map %in% prog_codes)])
  message("[dept-report.R] prog_names: ", paste(prog_names, collapse=", "))

  # set d_params object
  message("[dept-report.R] setting d_params object...")
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
  
  message("[dept-report.R] returning d_params object...")
  return (d_params)
}


# create_dept_report_data prepares the d_params object with all the data needed for the report
# It calls various functions to populate the d_params object with data for each section of the report
create_dept_report_data <- function(data_objects, opt) {
  message("[dept-report.R] Welcome to create_dept_report_data!")
  
  # Check what's available in data_objects
  message("[dept-report.R] data_objects keys: ", paste(names(data_objects), collapse=", "))
  message("[dept-report.R] hr_data exists: ", "hr_data" %in% names(data_objects))
  if ("hr_data" %in% names(data_objects)) {
    message("[dept-report.R] hr_data is null: ", is.null(data_objects[["hr_data"]]))
    if (!is.null(data_objects[["hr_data"]])) {
      message("[dept-report.R] hr_data rows: ", nrow(data_objects[["hr_data"]]))
    }
  }

  # set dept_code and prog_focus
  dept_code <- opt[["dept"]]
  prog_focus <- opt[["prog"]]

  message("[dept-report.R] setting payload with dept_code ",dept_code," and prog_focus ",prog_focus,"...")
  d_params <- set_payload (dept_code,prog_focus)
    
  
  ####### HEADCOUNT
  message("[dept-report.R] About to call get_headcount_data_for_dept_report...")
  d_params <- get_headcount_data_for_dept_report(data_objects[["academic_studies"]], d_params)
  message("[dept-report.R] Completed headcount data processing")

  ####### DEGREES
  message("[dept-report.R] About to call get_degrees_for_dept_report...")
  d_params <- get_degrees_for_dept_report(data_objects[["degrees"]], d_params)
  message("[dept-report.R] Completed degrees data processing")

  ####### CREDIT HOURS
  message("[dept-report.R] About to filter class_lists by dept_code (with DEPT col)...")
  filtered_cl_by_dept <- data_objects[["class_lists"]] %>%
    filter(DEPT == dept_code)
  
  message("[dept-report.R] About to call get_credit_hours_for_dept_report...")
  d_params <- get_credit_hours_for_dept_report(filtered_cl_by_dept, d_params)
  message("[dept-report.R] Completed credit hours data processing")

  ####### CREDIT HOURS BY MAJOR
  message("[dept-report.R] About to call credit_hours_by_major...")
  d_params <- credit_hours_by_major(filtered_cl_by_dept, d_params)
  message("[dept-report.R] Completed credit_hours_by_major processing")

  ####### CREDIT HOURS BY FACULTY
  message("[dept-report.R] About to call credit_hours_by_fac...")
  d_params <- credit_hours_by_fac(data_objects, d_params)
  message("[dept-report.R] Completed credit_hours_by_fac processing")

  ####### GRADES
  message("[dept-report.R] About to call get_grades_for_dept_report...")
  d_params <- get_grades_for_dept_report(filtered_cl_by_dept, data_objects[["hr_data"]], opt, d_params)
  message("[dept-report.R] Completed grades data processing")

  ####### ENROLLMENT
  message("[dept-report.R] About to call get_enrl_for_dept_report...")
  d_params <- get_enrl_for_dept_report(data_objects[["DESRs"]], d_params)
  message("[dept-report.R] Completed enrollment data processing")

  ####### SFRs
  message("[dept-report.R] About to call get_sfr_data_for_dept_report...")
  d_params <- get_sfr_data_for_dept_report(data_objects, d_params)
  message("[dept-report.R] Completed SFR data processing")
  
  message("[dept-report.R] About to return d_params from create_dept_report_data")
  return(d_params)
}



create_dept_report <- function (data_objects,opt) {
  
  message("[dept-report.R] Welcome to create_dept_report!")

  gc()  # clean up memory before starting

  # for studio testing...
  #opt <- list()
  #opt$output <- "html"
  #opt$dept <- "LCL"
  #opt$prog <- ""
  
# convert dept param to dept_list for processing
  dept_list <- convert_param_to_list(opt[["dept"]]) 
  
  # loop through each unit in dept list
  for (dept in dept_list) {
    # for studio testing a single dept
    #dept <- "AMST"
    message("[dept-report.R] looking at dept: ", dept)
    message("[dept-report.R] length: ", length(dept))

    dept_code <- ""
    prog_focus <- NULL
    
    if (length(dept) == 2) {
      dept_code <- unlist(dept)[1]
      prog_focus <- unlist(dept)[2]
    } else {
      dept_code <- dept
      prog_focus <- NULL
    }
  
    # set dept_code and prog_focus
    opt[["dept"]] <- dept_code
    opt[["prog"]] <- prog_focus

    # get dept report data
    message("[dept-report.R] about to call create_dept_report_data...")
    d_params <- create_dept_report_data(data_objects, opt)

    # set output_filename
    message("[dept-report.R] setting output filename...")
    if (!is.null(d_params$prog_focus) && !is.na(d_params$prog_focus)) {
      output_filename <- paste0(d_params$dept_code, "-", d_params$prog_focus)
    } else {
      output_filename <- d_params$dept_code
    }
    message("[dept-report.R] output_filename: ", output_filename)
    
    d_params$output_filename <- output_filename
    d_params$rmd_file <-  paste0(cedar_base_dir,"Rmd/dept-report.Rmd")
    d_params$output_dir_base <- paste0(cedar_output_dir,"dept-reports/")
    
    # create report (defined in misc_funcs.R)
    create_report(opt, d_params)
  
  } # end of dept loop
  message("[dept-report.R] Completed create_dept_report for all departments!")
  return("dept-report success!")
}

