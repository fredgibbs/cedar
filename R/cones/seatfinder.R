

get_courses_common <- function (term_courses, enrl_summary) {
  
  message ("[seatfinder.R] Welcome to get_courses_common! Finding courses common to both terms...")
  courses_intersect <- intersect(term_courses[["start"]], term_courses[["end"]])
  courses_intersect <- merge(courses_intersect, enrl_summary, by = c("CAMP","COLLEGE","SUBJ_CRSE","gen_ed_area"))
  
  message("[seatfinder.R] Computing enrollment difference between terms...")
  courses_intersect <- courses_intersect %>% group_by(SUBJ_CRSE) %>% arrange(CAMP,COLLEGE,TERM,SUBJ_CRSE) %>% 
    mutate ( enrl_diff_from_last_year = enrolled - lag(enrolled))
  
  return (courses_intersect)
}



get_courses_diff <- function (term_courses) {
  
  message ("[seatfinder.R] Welcome to get_courses_diff! Finding differences between the terms...")
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
  # opt$dept <- "HIST"
  # courses <- load_courses()
  # students <- load_students()
  
  message("[seatfinder.R] Welcome to seatfinder!")
  
  # set opt 
  message ("[seatfinder.R] Seatfinder always uses the exclude list, excludes AOP courses, includes only active courses, and aggregates section enrollments by course_type...")
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
  
  # get enrollment summary (which does opt filtering)
  message("[seatfinder.R] Getting enrollment summary...")
  # if no grouping specified, aggregate by course/method/PT (not individual sections)
  if (is.null(opt[["group_cols"]]) || length(opt[["group_cols"]]) == 0) {
    opt[["group_cols"]] <- c("CAMP","COLLEGE","TERM","SUBJ_CRSE","PT","level","gen_ed_area")
  } else {
    # ensure required columns for downstream merging are always included
    required_cols <- c("CAMP", "COLLEGE", "TERM", "SUBJ_CRSE", "gen_ed_area")
    opt[["group_cols"]] <- unique(c(required_cols, opt[["group_cols"]]))
  }
  message("[seatfinder.R] Using group_cols: ", paste(opt[["group_cols"]], collapse = ", "))
  enrl_summary <- get_enrl(courses,opt)
  
  
  # add mean DFW rate for course
  # make sure grades match course params (pt, method, etc)
  # set params for get_grades
  myopt <- opt
  myopt$course <- as.list(enrl_summary$SUBJ_CRSE)
  myopt$term <- NULL # remove term param to get dfw rates across all terms, not just seatfinder terms
  
  #TODO: document what get_grades is doing & getting
  message("[seatfinder.R] Getting grades data for courses in enrollment summary...")
  grades <- get_grades(students, hr_data, myopt)[["course_avg"]]
  
  message("[seatfinder.R] Grades data has rows: ", nrow(grades))
  message("[seatfinder.R] Grades data columns: ", paste(colnames(grades), collapse = ", "))
  message("[seatfinder.R] Sample of grades data:")
  message(head(grades))


  cols <- c("Course Campus Code", "Course College Code", "SUBJ_CRSE", "DFW %")
  message("[seatfinder.R] Selecting needed columns from grades data: ", paste(cols, collapse = ", "))
  grades <- grades %>% select (all_of(cols))
  
  # merge grade data with enrl data
  message("[seatfinder.R] Merging grade data with enrollment summary...")
  message("[seatfinder.R] enrl_summary columns: ", paste(colnames(enrl_summary), collapse = ", "))
  message("[seatfinder.R] grades columns: ", paste(colnames(grades), collapse = ", "))
  enrl_summary <- merge(enrl_summary, grades, by.x=c("CAMP","COLLEGE","SUBJ_CRSE"), by.y=c("Course Campus Code","Course College Code","SUBJ_CRSE"), all.x = TRUE )
  
  # Clean up columns before creating output dataframes
  message("[seatfinder.R] Removing unnecessary columns from enrollment summary...")
  enrl_summary <- enrl_summary %>% select(-any_of(c("xl_sections", "reg_sections", "INST_METHOD")))

  # get only core course data for diff and intersect comparison
  cols <- c("CAMP", "COLLEGE", "TERM", "SUBJ_CRSE", "gen_ed_area")
  message("[seatfinder.R] Selecting needed columns from enrollment summary for course comparisons: ", paste(cols, collapse = ", "))
  course_names <- enrl_summary %>% 
    ungroup() %>% 
    select(all_of(cols))
  
  # create separate DFs for start and end terms
  start_term_courses <- course_names %>% filter (TERM == opt[["term_start"]])
  end_term_courses <- course_names %>% filter (TERM == opt[["term_end"]])
  
  # prep new container lists
  term_courses <- list()
  courses_list <- list()
  
  # need to subtract out the TERM col for the intersection and setdiffs
  message ("[seatfinder.R] Getting first and second term courses...")
  term_courses[["start"]] <- start_term_courses %>% ungroup() %>% select (-TERM)  %>% arrange(CAMP,COLLEGE,SUBJ_CRSE)
  term_courses[["end"]] <- end_term_courses %>% ungroup() %>% select (-TERM) %>% arrange(CAMP,COLLEGE,SUBJ_CRSE)
  
  
  # find enrollment differences compared to last year across course types
  message("[seatfinder.R] Computing course type summary with enrollment differences...")
  
  # pivot to compare start vs end term availability
  course_type_summary <- enrl_summary %>% 
    select(CAMP, COLLEGE, TERM, PT, SUBJ_CRSE, gen_ed_area, avail, `DFW %`) %>%
    pivot_wider(names_from = TERM, values_from = avail, names_prefix = "avail_") %>%
    mutate(
      avail_diff = .data[[paste0("avail_", opt[["term_end"]])]] - .data[[paste0("avail_", opt[["term_start"]])]]
    ) %>%
    # merge back with full end term data to get enrolled count and current avail
    left_join(
      enrl_summary %>% filter(TERM == opt[["term_end"]]) %>% select(CAMP, COLLEGE, PT, SUBJ_CRSE, gen_ed_area, enrolled, avail, TERM),
      by = c("CAMP", "COLLEGE", "SUBJ_CRSE", "gen_ed_area", "PT")
    ) %>%
    
    # keep only courses that exist in end term (have non-NA TERM after join)
    filter(!is.na(TERM)) %>%
    select(-starts_with("avail_202")) %>%  # remove the pivoted columns
    ungroup() %>%
    select(CAMP, COL = COLLEGE, TERM, PT, SUBJ_CRSE, avail, `DFW %`, avail_diff, ENRL = enrolled, GE = gen_ed_area) %>%
    arrange(CAMP, COL, TERM, PT, SUBJ_CRSE) %>%
    filter (avail > 0)


  # add to output list
  courses_list[["type_summary"]] <- course_type_summary
  
  # find common courses between two terms
  courses_common <- get_courses_common(term_courses, enrl_summary)
  
  # to clean up list, filter for just the target term
  courses_common <- courses_common %>% 
    filter (TERM == opt[["term_end"]]) %>% 
    arrange(CAMP, COLLEGE, SUBJ_CRSE, enrl_diff_from_last_year)
  
  courses_list[["courses_common"]] <- courses_common
  
  
  # find difference between terms (courses offered previously, and courses offered now)
  courses_diff <- get_courses_diff(term_courses)
  courses_list[["courses_prev"]] <- merge(courses_diff[["prev"]], enrl_summary, by = c("CAMP","COLLEGE", "SUBJ_CRSE","gen_ed_area"))
  courses_list[["courses_new"]] <- merge(courses_diff[["new"]], enrl_summary, by = c("CAMP","COLLEGE","SUBJ_CRSE","gen_ed_area"))
  
  
  # make list of only gen ed courses
  gen_ed_summary <- enrl_summary %>% group_by(CAMP, COLLEGE, TERM, SUBJ_CRSE, PT) %>% 
    filter (!is.na(gen_ed_area)) %>% 
    filter (avail > 0) %>% 
    filter (TERM == opt[["term_end"]]) %>% 
    arrange(gen_ed_area, desc(avail), CAMP,COLLEGE,SUBJ_CRSE)
  
  courses_list[["gen_ed_summary"]] <- gen_ed_summary

  # find courses that are active but likely capped at 0 for now
  gen_ed_likely <- enrl_summary %>% group_by(CAMP,COLLEGE,TERM,SUBJ_CRSE,PT) %>% 
    filter (!is.na(gen_ed_area)) %>% 
    filter (TERM == opt[["term_end"]]) %>% 
    filter (avail == 0 & enrolled == 0) %>% 
    arrange(gen_ed_area,CAMP,COLLEGE,SUBJ_CRSE)
  
  courses_list[["gen_ed_likely"]] <- gen_ed_likely
  
  message("[seatfinder.R] All done in seatfinder! Returning course_list...")
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

