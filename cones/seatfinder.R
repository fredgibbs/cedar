

get_courses_common <- function (term_courses, enrl_summary) {
  
  # find intersection of both terms and restore TERM col
  message ("finding courses common to both terms...")
  courses_intersect <- intersect(term_courses[["start"]], term_courses[["end"]])
  courses_intersect <- merge(courses_intersect, enrl_summary, by = c("CAMP","COLLEGE","SUBJ_CRSE","gen_ed_area"))
  
  # compute enrollment difference between terms
  courses_intersect <- courses_intersect %>% group_by(SUBJ_CRSE) %>% arrange(CAMP,COLLEGE,TERM,SUBJ_CRSE) %>% 
    mutate ( enrl_diff_from_last_year = enrolled - lag(enrolled))
  
  return (courses_intersect)
}



get_courses_diff <- function (term_courses) {
  
  message ("finding difference between the terms...")
  previously_offered <- setdiff(term_courses[["start"]], term_courses[["end"]]) # %>% arrange(CAMP,COLLEGE,SUBJ_CRSE,CRSE_TITLE)
  newly_offered <- setdiff(term_courses[["end"]], term_courses[["start"]]) # %>% arrange(CAMP,COLLEGE,SUBJ_CRSE,CRSE_TITLE)
  
  courses_diff <- list()
  courses_diff[["prev"]] <- previously_offered
  courses_diff[["new"]] <- newly_offered
  
  return(courses_diff)
}



# TODO: move to misc_functions?
# TODO: also change NA to 0 (or vice versa)
normalize_inst_method <- function (courses) {
  courses$method <- courses$INST_METHOD
  courses$method[courses$INST_METHOD == "0"] <- "f2f"
  courses$method[courses$INST_METHOD == "ENH"] <- "f2f"
  courses$method[courses$INST_METHOD == "HYB"] <- "f2f"
  
  return(courses)
}


# this is the main function that calls the calc functions
# need to take in incoming params from shiny (maybe opt is fine?)
# takes set of students (for calling)
seatfinder <- function (students, courses, opt) {
  
  ########## for studio testing
  # opt <- list()
  # opt$term <- "202510"
  # opt$pt <- "2H"
  # courses <- load_courses()
  # students <- load_students()
  
  message("\n","Welcome to seatfinder!")
  
  # set opt 
  message ("seatfinder always uses the exclude list, excludes AOP courses, includes only active courses, and aggregates section enrollments by course_type...")
  opt$uel <- TRUE
  opt$status <- "A"
  
  # standard behavior is to use specified term param and subtract one year for comparison
  # if term param has two terms separated by comma, compare those instead
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
  
  # filter courses according to options
  f_courses <- filter_DESRs(courses,opt) 
  
  # ALWAYS WITHOUT AOP sections (the twin section is fine)
  f_courses <- f_courses %>% filter (INST_METHOD != "MOPS")
  
  # normalize instructor method--replace ENH,0,HYB with f2f
  f_courses <- normalize_inst_method(f_courses)
  
  # ensure distinct rows/courses
  f_courses <- f_courses %>% distinct(CAMP,COLLEGE,TERM,CRN, .keep_all = TRUE)
  
  # aggregate enrollments across courses (f_courses is already aggregated, so sections here is number of courses)
  # enrollment summaries get merged to course lists
  opt[["group_cols"]] <- c("CAMP", "COLLEGE", "TERM","SUBJ_CRSE","PT","INST_METHOD","level","gen_ed_area")
  enrl_summary <- get_enrl(f_courses,opt)
  
  # filter out everything but specified current term
  # enrl_summary <- enrl_summary %>% filter (TERM == opt[["term_end"]])
  
  # add mean DFW rate for course; make sure grades match course params (pt, method, etc)
  # TODO: merge grades with enrl_summary or f_courses?
  # set params for get_grades
  myopt <- opt
  myopt$course <- as.list(f_courses$SUBJ_CRSE)
  myopt$aggregate <- "course_avg"
  myopt$term <- NULL # remove term param to get dfw rates across all terms, not just seatfinder terms
  
  #TODO: document what get_grades is doing & getting
  grades <- get_grades(students,myopt)[["course_avg"]]
  grades <- grades %>% select(`Course Campus Code`, `Course College Code`, SUBJ_CRSE, `DFW %`)
  
  # merge grade data with enrl data
  enrl_summary <- merge(enrl_summary, grades, by.x=c("CAMP","COLLEGE","SUBJ_CRSE"), by.y=c("Course Campus Code","Course College Code","SUBJ_CRSE"), all.x = TRUE )
  
  # get only course names to ignore diff and intersect comparison of extra data
  course_names <- f_courses %>% ungroup() %>% 
    # filter (!is.na(gen_ed_area)) %>%
    select(CAMP, COLLEGE, TERM, SUBJ_CRSE, CRSE_TITLE, gen_ed_area)
  
  # create separate DFs for start and end terms
  start_term_courses <- course_names %>% filter (TERM == opt[["term_start"]])
  end_term_courses <- course_names %>% filter (TERM == opt[["term_end"]])
  
  # prep new container lists
  term_courses <- list()
  courses_list <- list()
  
  # need to subtract out the TERM col for the intersection and setdiffs
  message ("getting first and second term courses...")
  term_courses[["start"]] <- start_term_courses %>% ungroup() %>% select (-TERM)  %>% arrange(CAMP,COLLEGE,SUBJ_CRSE,CRSE_TITLE)
  term_courses[["end"]] <- end_term_courses %>% ungroup() %>% select (-TERM) %>% arrange(CAMP,COLLEGE,SUBJ_CRSE,CRSE_TITLE)
  
  
  # aggregate enrollments across COURSE TYPES (don't get specific course data, just PT,method, etc) 
  opt[["group_cols"]] <- c("CAMP","COLLEGE", "TERM","PT","INST_METHOD","level","gen_ed_area")
  enrl_type_summary <- get_enrl(f_courses,opt)
  
  # find enrollment differences compared to last year across course types
  course_type_summary <- enrl_type_summary %>% 
    arrange(gen_ed_area,INST_METHOD) %>% 
    group_by(gen_ed_area,INST_METHOD) %>% 
    mutate ( avail_diff = avail - lag(avail))
  
  # we only need the end term
  course_type_summary <- course_type_summary %>% filter (TERM == opt[["term_end"]])  %>% 
    arrange(CAMP,COLLEGE,gen_ed_area)
  courses_list[["type_summary"]] <- course_type_summary
  
  # find common courses between two terms
  courses_common <- get_courses_common(term_courses,enrl_summary)
  
  # to clean up list, filter for just the target term
  courses_common <- courses_common %>% 
    filter (TERM == opt[["term_end"]]) %>% 
    arrange(CAMP, COLLEGE, gen_ed_area, SUBJ_CRSE, INST_METHOD, enrl_diff_from_last_year)
  
  courses_list[["courses_common"]] <- courses_common
  
  
  # find difference between terms (courses offered previously, and courses offered now)
  courses_diff <- get_courses_diff(term_courses)
  courses_list[["courses_prev"]] <- merge(courses_diff[["prev"]], enrl_summary, by = c("CAMP","COLLEGE", "SUBJ_CRSE","gen_ed_area"))
  courses_list[["courses_new"]] <- merge(courses_diff[["new"]], enrl_summary, by = c("CAMP","COLLEGE","SUBJ_CRSE","gen_ed_area"))
  
  
  # summarize gen ed courses
  gen_ed_summary <- enrl_summary %>% group_by(CAMP, COLLEGE, TERM,SUBJ_CRSE,INST_METHOD,PT) %>% 
    filter (!is.na(gen_ed_area)) %>% 
    filter (avail > 0) %>% 
    arrange(gen_ed_area,desc(avail),CAMP,COLLEGE,SUBJ_CRSE,INST_METHOD)
  
  # find courses that are active but likely capped at 0 for now
  gen_ed_likely <- enrl_summary %>% group_by(CAMP,COLLEGE,TERM,SUBJ_CRSE,INST_METHOD,PT) %>% 
    filter (!is.na(gen_ed_area)) %>% 
    filter (avail == 0 & enrolled == 0) %>% 
    arrange(gen_ed_area,CAMP,COLLEGE,SUBJ_CRSE,INST_METHOD)
  
  courses_list[["gen_ed_summary"]] <- gen_ed_summary
  courses_list[["gen_ed_likely"]] <- gen_ed_likely
  
  message("all done in seatfinder; returning course_list...")
  return (courses_list)
}




###################################
create_seatfinder_report <- function (students, courses, opt) {  
  
  # begin setting payload for Rmd file
  d_params <- list("term"  = opt[["term"]],
                   "opt" = opt,
                   "tables" = list()
  )
  
  # call basic seatfinder
  courses_list <- seatfinder(students, courses, opt)
  
  d_params$tables[["type_summary"]] <- courses_list[["type_summary"]]
  d_params$tables[["courses_common"]] <- courses_list[["courses_common"]]
  d_params$tables[["courses_prev"]] <- courses_list[["prev"]]
  d_params$tables[["courses_new"]] <- courses_list[["new"]]
  
  # store in d_params to send to Rmd
  d_params$tables[["gen_ed_summary"]] <- courses_list[["gen_ed_summary"]]
  d_params$tables[["gen_ed_likely"]] <- courses_list[["gen_ed_likely"]]
  
  # set output data
  d_params$output_filename <- paste0("seatfinder-",opt[["term"]],"-",opt[["pt"]])
  d_params$rmd_file <- "Rmd/seatfinder-report.Rmd"
  d_params$output_dir_base <- paste0(cedar_output_dir,"seatfinder-reports/")
  
  # generate report
  create_report(opt,d_params)
  
} # end seatfinder_report

