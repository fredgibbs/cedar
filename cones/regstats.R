# use rollcall to find popular fall courses among sophomores for potential summer offerings
get_high_fall_sophs <- function (students,courses,opt) {
  
  message("getting fall courses with 100+ sophomores for potential summer offerings...")
  myopt <- list()
  myopt[["group_cols"]] <- c("Course Campus Code", "Course College Code","Academic Period Code", "term_type", "Student Classification", "SUBJ_CRSE","Short Course Title","level")
  myopt[["classification"]] <- "Sophomore, 2nd Yr"
  myopt[["term"]] <- "fall"
  rollcall_out <- rollcall(students,myopt)
  
  # 100 is a bit arbitrary; not sure how to calc would what be a better threshold
  rollcall_out <- rollcall_out %>% filter(mean > 100)
  
  # grab just SUBJ_CRSE col
  high_fall_sophs <- tibble(SUBJ_CRSE = unique(rollcall_out$SUBJ_CRSE))
  
  message("all done getting high fall sophs!")
  
  return(as_tibble(high_fall_sophs))
}


# single out bumps to forecast for where_to courses 
get_after_bumps <- function (bumps, students, courses, opt) {
  
  bumps <- bumps$SUBJ_CRSE
  after_bumps <- c()
  
  # reset temp opt params
  myopt <- opt
  
  # loop through bumps to see what courses students take next, and add those to the list
  for (course in bumps) {
    
    # for studio testing...
    #course <- bumps[1]
    #message("now processing: ",course,"...")
    
    myopt[["course"]] <- course
    
    # get top 5 courses where students go after a bump course (than than normal enrollment)
    where_tos <- where_to(students,myopt) %>% arrange (desc(avg_contrib))
    next_courses <- head(where_tos,n=5)
    after_bumps <- c(after_bumps, next_courses$SUBJ_CRSE)
    
  } # end loop through bumps to find next courses
  
  after_bumps <- unique(tibble(SUBJ_CRSE = after_bumps))
  
  message("done assembling, and returning after bumps...")
  
  return(after_bumps)
}



####### main function 
get_reg_stats <- function(students,courses,opt) {
  message("\nWelcome to get_reg_stats!")
  
  # uncomment for studio testing
  # opt <- list()
  # opt[["term"]] <- "202510"
  #opt[["course"]] <- "HIST 1160"
  
  
  # grab default thresholds from config.R
  message("setting default thresholds (from config.R...")
  thresholds <- cedar_regstats_thresholds
  
  # check for opt thresholds, esp from shiny
  if (!is.null(opt[["thresholds"]])) {
    message("overriding with opt$thresholds...")
    thresholds <- opt[["thresholds"]]
    print(thresholds)
  }
  
  # process course param
  if (!is.null(opt$course)) {
    message("processing opt$course...")
    course_list <- convert_param_to_list(opt[["course"]])
    
    # do course filtering early, but not term
    message("filtering COURSES by course_list...")
    filtered_courses <- courses %>% filter (SUBJ_CRSE %in% course_list)
    
    message("filtering STUDENTS by course_list...")
    filtered_students <- students %>% filter (SUBJ_CRSE %in% course_list)
    message("left with ",nrow(filtered_students)," students.")
  } else {
    filtered_students <- students
  }
  
  # filter by term LATER so calcs below can get mean values across terms
  
  # get registration and enrollment stats  
  regstats <- calc_cl_enrls(filtered_students)
  
  # find potential anomalies
  # use biased SD calc, since we're not really sampling from a population
  message("finding courses of interest...")
  flagged <- list()
  std_fields <- c("Course Campus Code", "Course College Code","SUBJ_CRSE","Academic Period Code","term_type","registered")
  std_group_cols <- c("Course Campus Code", "Course College Code","SUBJ_CRSE","term_type")
  #std_arrange_cols <- c("Course Campus Code","Academic Period Code","impacted")
  std_arrange_cols <- c("Course Campus Code", "Course College Code")
  
  ##### EARLY DROPS
  message("finding early drops...")
  drops <- regstats %>% select (all_of(std_fields), drop_early=dr_early, dr_early_mean)
  drops <- drops %>% group_by_at(all_of(std_group_cols))
  drops <- drops %>% mutate (sd = round(sd(drop_early) * thresholds[["pct_sd"]] /(sqrt(n()-1/n())),digits=2), impacted = round(drop_early-(dr_early_mean + sd),digits=2))
  drops <- drops %>% filter (impacted > thresholds[["min_impacted"]])
  drops <-  drops %>% arrange(across(std_arrange_cols))
  flagged[["early_drops"]] <- drops
  
  
  ##### LATE DROPS
  message("finding late drops...")
  late_drops <- regstats %>% select (all_of(std_fields), drop_late=dr_late, dr_late_mean)
  late_drops <- late_drops %>% group_by_at(all_of(std_group_cols))
  late_drops <- late_drops %>% mutate (sd = round(sd(drop_late) * thresholds[["pct_sd"]] /(sqrt(n()-1/n())),digits=2), impacted = round(drop_late-(dr_late_mean + sd),digits=2))
  late_drops <- late_drops %>% filter (impacted > thresholds[["min_impacted"]])
  flagged[["late_drops"]]  <-  late_drops %>% arrange(across(std_arrange_cols))
  
  
  ##### DIPS
  message("finding dips...")
  dips <- regstats %>% select (all_of(std_fields), registered, registered_mean)
  dips <- dips %>% group_by_at(all_of(std_group_cols))
  dips <- dips %>% mutate (sd = round(sd(registered) * thresholds[["pct_sd"]] / (sqrt(n()-1/n())),digits=2), impacted = round((registered_mean - sd) - registered, digits=2))
  dips <- dips %>% filter (impacted > thresholds[["min_impacted"]])
  flagged[["dips"]] <-  dips %>% arrange (across(std_arrange_cols))
  
  
  ##### BUMPS
  message("finding bumps...")
  bumps <- regstats %>% select (all_of(std_fields), registered, registered_mean)
  bumps <- bumps %>% group_by_at(all_of(std_group_cols))
  bumps <- bumps %>% mutate (sd = round(sd(registered) * thresholds[["pct_sd"]] / (sqrt(n()-1/n())),digits=2), impacted = round(registered - (registered_mean + sd), digits=2))
  bumps <- bumps %>% filter (impacted > thresholds[["min_impacted"]])
  flagged[["bumps"]] <-  bumps %>% arrange (across(std_arrange_cols))
  
  
  ##### WAITS
  message("finding waits...")
  myopt <- opt
  myopt[["uel"]] <- TRUE
  myopt[["group_cols"]] <- c("CAMP","COLLEGE","TERM", "SUBJ_CRSE", "gen_ed_area")
  enrls <- get_enrl(courses, myopt)
  waits <-  enrls %>% filter (waiting > thresholds[["min_wait"]]) %>% arrange (desc(waiting))
  waits <- waits %>% rename (`Academic Period Code` = TERM)
  flagged[["waits"]] <- waits
  
  
  ##### SQUEEZES
  message("finding squeezes...")
  squeezes <- merge(enrls,regstats,
                    by.x=c("CAMP","COLLEGE","TERM","SUBJ_CRSE"),
                    by.y=c("Course Campus Code","Course College Code","Academic Period Code","SUBJ_CRSE"),all.x=TRUE )
  squeezes <- squeezes %>% mutate(squeeze = round(avail/dr_all_mean,digits=2))
  squeezes <- squeezes %>% 
    filter (enrolled >= thresholds[["min_impacted"]]) %>%  
    filter (squeeze < thresholds[["min_squeeze"]]) %>% 
    arrange(term_type,TERM,squeeze)  
  
  squeezes <- squeezes %>% rename (`Academic Period Code` = TERM)
  
  flagged[["squeezes"]] <- squeezes
  
  
  # filter for supplied term
  if (!is.null(opt[["term"]])) {
    message("filtering for term...")
    message(names(flagged))
    flagged <- lapply(flagged, function(x) filter_by_term(x, opt[["term"]], "Academic Period Code"))
  }
  
  #print(flagged)
  
  ##### COURSES AFTER BUMPS (if not from shiny)
  message("finding courses students take after bumps...")
  if (as.logical(Sys.getenv("shiny")) == FALSE) {
    # disabling until we have a better way of using these...
    #flagged[["courses_after_bumps"]] <- get_after_bumps(flagged[["bumps"]], students, courses, opt)
  }
  
  # gather SUBJ_CRSE col into separate list  
  message("gathering flagged courses...")
  flagged_courses <- c()
  for (flag in flagged) {
    if (!is.null(flag$SUBJ_CRSE)) {
      flagged_courses <- c(flagged_courses, as.character(flag$SUBJ_CRSE))  
    }
  }
  
  # convert to tibble with unique values
  # message("converting to unique SUBJ_CRSE values to tibble...")
  # flagged_courses <- tibble(SUBJ_CRSE = unique(as.character(flagged_courses)))
  message("filtered flagged_courses has ", nrow(flagged_courses), " rows:")
  
  flagged[["all_flagged_courses"]] <- sort(unique(flagged_courses))
  
  # save thresholds for adding to report
  flagged[["thresholds"]] <- thresholds 
  
  # keep separate from flagged courses since we don't need to forecast for this all the time
  if (as.logical(Sys.getenv("shiny")) == FALSE) {
    flagged[["high_fall_sophs"]] <- get_high_fall_sophs(students, courses, myopt)
  }
  
  message("returning flagged courses...")
  return(flagged)
}



create_regstat_report <- function(students,courses,opt) {
  message("\nWelcome to create_regstat_report!")
  
  # get flagged courses
  flagged <- get_reg_stats(students,courses,opt)
  
  # if arrange param set, use it
  if (!is.null(opt[["arrange"]])) {
    arrange_col <- opt[["arrange"]]
    flagged <- flagged %>%  arrange(get({{arrange_col}}) )
  }
  
  # payload
  d_params <- list("opt" = opt,
                   "tables" = list(
                     "flagged" = flagged
                   )
  )
  
  message("rendering regstats report...")
  
  good_opts <- c("college","course","term","pt","im")
  message("setting filename based on params...")
  output_filename <- "regstats"
  
  for (i in 1:length(opt)) {
    message(names(opt[i]))
    if (names(opt[i]) %in% good_opts) {
      output_filename <- paste0(output_filename,"-",opt[i])    
    }
  }
  
  # set output data
  d_params$output_filename <- output_filename
  d_params$rmd_file <- paste0(cedar_base_dir,"Rmd/regstats-report.Rmd")
  d_params$output_dir_base <- paste0(cedar_output_dir,"regstats-reports/")
  
  create_report(opt,d_params)
  
  message("all done creating regstats report!")
}
