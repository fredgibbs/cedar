

get_courses_diff <- function (f_courses,opt,d_params) {
  
  f_courses <- f_courses %>% distinct(TERM,CRN, .keep_all = TRUE)
  
  # aggregate enrollments across courses (f_courses is already aggregated, so sections here are total courses)
  summary <- f_courses %>% group_by(TERM,SUBJ_CRSE,PT,method,level,gen_ed_area) %>% 
    summarize(.groups="keep", sections=n(),avg_size=round(mean(ENROLLED),digits=1),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT))
  
  # select only gen ed courses and relevant fields
  course_names <- f_courses %>% ungroup() %>% 
    filter (!is.na(gen_ed_area)) %>%
    select(TERM, SUBJ_CRSE, CRSE_TITLE, gen_ed_area)
  
  
  
  # parse opt param for first and second term
  prev_term_courses <- course_names %>% filter (TERM == opt[["term_start"]])
  cur_term_courses <- course_names %>% filter (TERM == opt[["term_end"]])
  
  # need to subtract out the TERM col for the intersection and setdiffs
  message ("getting first and second term courses...")
  prev_term_courses <- prev_term_courses %>% ungroup() %>% select (-TERM)  %>% arrange(SUBJ_CRSE,CRSE_TITLE)
  cur_term_courses <- cur_term_courses %>% ungroup() %>% select (-TERM) %>% arrange(SUBJ_CRSE,CRSE_TITLE)
  
  # find intersection of both terms and restore TERM col
  message ("finding courses common to both terms...")
  both_terms <- intersect(prev_term_courses, cur_term_courses)
  both_w_enrl <- merge(both_terms, summary, by = c("SUBJ_CRSE","gen_ed_area"))
  
  message ("finding difference between the terms...")
  prev_offered <- setdiff(prev_term_courses, cur_term_courses) %>% arrange(SUBJ_CRSE,CRSE_TITLE)
  prev_w_enrl <- merge(prev_offered, summary, by = c("SUBJ_CRSE","gen_ed_area"))
  
  newly_offered <- setdiff(cur_term_courses, prev_term_courses) %>% arrange(SUBJ_CRSE,CRSE_TITLE)
  newly_w_enrl <- merge(newly_offered, summary, by = c("SUBJ_CRSE","gen_ed_area"))
  
  # compute difference between terms
  both_w_enrl <- both_w_enrl %>% group_by(SUBJ_CRSE) %>% arrange(SUBJ_CRSE,TERM) %>% 
    mutate ( enrl_diff_from_last_year = enrolled - lag(enrolled))
  
  # filter for just the target term and remove non gen-ed courses
  both_w_enrl <- both_w_enrl %>% 
    filter (TERM != opt[["term_start"]] & !is.na(gen_ed_area) ) %>% 
    arrange(gen_ed_area,method,enrl_diff_from_last_year)
  
  # payload
  d_params$tables[["both_terms"]] <- both_w_enrl 
  d_params$tables[["prev_offered"]] <- prev_w_enrl
  d_params$tables[["newly_offered"]] <- newly_w_enrl
  
  return(d_params)
}


normalize_inst_method <- function (f_courses) {
  f_courses$method <- f_courses$INST_METHOD
  f_courses$method[f_courses$INST_METHOD == "0"] <- "f2f"
  f_courses$method[f_courses$INST_METHOD == "ENH"] <- "f2f"
  f_courses$method[f_courses$INST_METHOD == "HYB"] <- "f2f"
  
  return(f_courses)
}



get_course_type_summary <- function (f_courses,opt) {
  # aggregate enrollments across courses (f_courses is already aggregated across sections)
  summary <- f_courses %>% group_by(PT,method,level,gen_ed_area,TERM) %>% 
    summarize(.groups="keep", courses=n(),avg_size=median(ENROLLED),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT))
  
  # find enrl diff compared to last year
  summary <- summary %>% 
    arrange(gen_ed_area,method) %>% 
    group_by(gen_ed_area,method) %>% 
    mutate ( avail_diff = avail - lag(avail))
  
  # keep only end term rows and gen ed courses
  summary <- summary %>% filter (TERM == opt[["term_end"]] & !is.na(gen_ed_area) )
  
  return (summary)
}


###################################
create_seatfinder_report <- function (students,courses,opt) {  
  
  ########## for studio testing
  # opt <- list()
  # opt$term <- "202480"
  # opt$pt <- "INT"

  # courses <- load_courses(opt)
  
  message("\n","welcome to seatfinder!")
  
  # set opt 
  message ("seatfinder always uses the exclude list, excludes AOP courses, includes only active courses, and aggregates section enrollments by course_type...")
  opt$uel <- TRUE
  opt$status <- "A"
  opt$aggregate <- "course_type"
  
  # standard behavior is to use specified term param and subtract one year for comparison
  # if term param has two terms separated by comma compare those
  term <- opt[["term"]]
  
  # extract start and end codes
  if (grepl(",", term)) {
    opt[["term_start"]] <- substring(term, 1,6)
    opt[["term_end"]] <- substring(term, 8,13)
  } else {
    opt[["term_end"]] <- term
    opt[["term_start"]] <- as.character(as.numeric(term) - 100) # default to one year previous to specified term
    
    # adjust term param for course filtering
    opt[["term"]] <- paste0(opt[["term_start"]],",",opt[["term_end"]])
  }
  
  # list specified and implied options
  print(opt)
  
  # set payload for Rmd file
  d_params <- list("term"  = opt[["term"]],
                   "opt" = opt,
                   "tables" = list()
  )
  
  # filter courses according to options
  # ALWAYS WITHOUT AOP sections (the twin section is fine)
  f_courses <- filter_DESRs(courses,opt) %>% filter (INST_METHOD != "MOPS")
  
  #normalize instructor method--replace ENH,0,HYB with f2f
  f_courses <- normalize_inst_method(f_courses)
  
  # add mean DFW rate for course
  myopt <- opt
  myopt$course <- as.list(f_courses$SUBJ_CRSE)
  myopt$aggregate <- "course_avg"
  myopt$term <- NULL # remove term param to get dfw rates across all terms, not just seatfinder terms
  f_courses_grades <- get_grades(students,myopt)
  f_courses_grades <- f_courses_grades %>% select(SUBJ_CRSE,`DFW %`)
  f_courses <- merge(f_courses,f_courses_grades)
  
  
  # get course_type_summary
  d_params$tables[["course_type_summary"]] <- get_course_type_summary(f_courses,opt)
  
  # get difference in courses between terms
  d_params <- get_courses_diff(f_courses,opt,d_params) 
  
  # get enrollments
  summary <- get_enrl(f_courses,opt)
  
  # filter out everything but specified current term
  summary <- summary %>% filter (TERM == opt[["term_end"]])
  
  summary <- merge(summary,f_courses_grades)
  

  # filter out non gen ed
  non_gen_ed_summary <- summary %>% filter(is.na(gen_ed_area))
  
  # summarize gen ed courses
  gen_ed_summary <- summary %>% group_by(TERM,SUBJ_CRSE,CRSE_TITLE,INST_METHOD,PT) %>% 
    filter (!is.na(gen_ed_area)) %>% 
    filter (avail > 0) %>% 
    arrange(gen_ed_area,desc(avail),SUBJ_CRSE,INST_METHOD)
  
  # find courses that are active but likely capped at 0 for now
  gen_ed_likely <- summary %>% group_by(TERM,SUBJ_CRSE,CRSE_TITLE,INST_METHOD,PT) %>% 
    filter (!is.na(gen_ed_area)) %>% 
    filter (avail == 0 & enrolled == 0) %>% 
    arrange(gen_ed_area,SUBJ_CRSE,INST_METHOD)
  
  # store in d_params to send to Rmd
  d_params$tables[["gen_ed_summary"]] <- gen_ed_summary
  d_params$tables[["gen_ed_likely"]] <- gen_ed_likely
  
  # set output data
  d_params$output_filename <- paste0("seatfinder-",opt[["term"]],"-",opt[["pt"]])
  d_params$rmd_file <- "cones/seatfinder-report/seatfinder-report.Rmd"
  d_params$output_dir_base <- paste0(cedar_output_dir,"seatfinder-reports/")
  
  # generate report
  create_report(opt,d_params)
  
  
} # end seatfinder_report

