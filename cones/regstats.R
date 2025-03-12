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
get_after_bumps <- function (bumps, students, courses) {
  
  bumps <- bumps$SUBJ_CRSE
  after_bumps <- c()
  
  # reset temp opt params
  myopt <- list()
  
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
  
  message("done assembling after bumps:")
  print(after_bumps)
  
  return(after_bumps)
}



####### main function 
get_reg_stats <- function(students,courses,opt) {
  message("\nWelcome to get_reg_stats!")
  
  # uncomment for studio testing
  #opt <- list()
  #opt[["course"]] <- "HIST 1160"
  #opt[["term"]] <- "202510"
  
  # grab default thresholds from config.R
  message("setting default thresholds...")
  thresholds <- cedar_regstats_thresholds
  
  if (!is.null(opt[["thresholds"]])) {
    message("overriding with opt$thresholds...")
    thresholds <- opt[["thresholds"]]
    print(thresholds)
  }
  
  myopt <- opt
  
  # if no course specified, use all lower division AS courses
  if (is.null(opt$course)) {
    message("no course specified. defaulting to lower-division courses.")
    myopt[["level"]] <- "lower"
    course_list <- as.list(get_course_list(courses,myopt))
  } 
  else {
    message("processing opt$course...")
    course_list <- convert_param_to_list(opt[["course"]])
  }
  
  
  # do course filtering early, but not term
  message("filtering COURSES by course_list...")
  filtered_courses <- courses %>% filter (SUBJ_CRSE %in% course_list)
  
  message("filtering STUDENTS by course_list...")
  filtered_students <- students %>% filter (SUBJ_CRSE %in% course_list)
  message("left with ",nrow(filtered_students)," students.")
  
  # filter by term AFTER getting regstats, so calcs below can get mean values across terms
  
  # get registration and enrollment stats  
  # regstats <- get_reg_data(filtered_students,myopt)
  regstats <- calc_cl_enrls(filtered_students)
  
  
  # find potential anomalies
  # use biased SD calc, since we're not really sampling from a population
  message("finding courses of interest...")
  flagged <- list()
  std_fields <- c("Course Campus Code", "Course College Code","SUBJ_CRSE","Academic Period Code","term_type","registered")
  std_group_cols <- c("Course Campus Code", "Course College Code","SUBJ_CRSE","term_type")
  
  ##### EARLY DROPS
  message("finding early drops...")
  drops <- regstats %>% select (all_of(std_fields), drop_early=dr_early, de_mean)
  drops <- drops %>% group_by_at(all_of(std_group_cols))
  drops <- drops %>% mutate (sd = round(sd(drop_early)/(sqrt(n()-1/n())),digits=2), impacted = round(drop_early-(de_mean+sd),digits=2))
  drops <- drops %>% filter (impacted > thresholds[["min_impacted"]])
  drops <-  drops %>% arrange (desc(impacted))
  flagged[["early_drops"]] <- drops
  
  
  ##### LATE DROPS
  message("finding late drops...")
  late_drops <- regstats %>% select (all_of(std_fields), drop_late=dr_late, dl_mean)
  late_drops <- late_drops %>% group_by_at(all_of(std_group_cols))
  late_drops <- late_drops %>% mutate (sd = round(sd(drop_late)/(sqrt(n()-1/n())),digits=2), impacted = round(drop_late-(dl_mean+sd),digits=2))
  late_drops <- late_drops %>% filter (impacted > thresholds[["min_impacted"]])
  flagged[["late_drops"]]  <-  late_drops %>% arrange (desc(impacted))
  
  
  ##### DIPS
  message("finding dips...")
  dips <- regstats %>% select (all_of(std_fields), registered, reg_mean)
  dips <- dips %>% group_by_at(all_of(std_group_cols))
  dips <- dips %>% mutate (sd = round(sd(registered)/(sqrt(n()-1/n())),digits=2), impacted = round((reg_mean-sd)-registered,digits=2))
  dips <- dips %>% filter (impacted > thresholds[["min_impacted"]])
  flagged[["dips"]] <-  dips %>% arrange (desc(impacted))
  
  
  ##### BUMPS
  message("finding bumps...")
  bumps <- regstats %>% select (all_of(std_fields), registered, reg_mean)
  bumps <- bumps %>% group_by_at(all_of(std_group_cols))
  bumps <- bumps %>% mutate (sd = round(sd(registered)/(sqrt(n()-1/n())),digits=2), impacted = round(registered-(reg_mean+sd),digits=2))
  bumps <- bumps %>% filter (impacted > thresholds[["min_impacted"]])
  flagged[["bumps"]] <-  bumps %>% arrange (desc(impacted))
  
  
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
  squeezes <- merge(enrls,regstats,by.x=c("CAMP","COLLEGE","TERM","SUBJ_CRSE"),by.y=c("Course Campus Code","Course College Code","Academic Period Code","SUBJ_CRSE"),all.x=TRUE )
  squeezes <- squeezes %>% mutate(squeeze = round(avail/da_mean,digits=2))
  squeezes <- squeezes %>% 
    filter (enrolled >= thresholds[["min_impacted"]]) %>%  
    filter (squeeze < thresholds[["min_squeeze"]]) %>% 
    arrange(term_type,TERM,squeeze)  
  
  squeezes <- squeezes %>% rename (`Academic Period Code` = TERM)
  
  flagged[["squeezes"]] <- squeezes
  
  
  # filter for supplied term
  if (!is.null(opt$term)) {
    message("filtering for term...")
    message(names(flagged))
    flagged <- lapply(flagged, function(x) filter_by_term(x ,opt$term,"Academic Period Code"))
  }
  
  print(flagged)
  
  ##### COURSES AFTER BUMPS
  message("finding courses students take after bumps...")
  flagged[["courses_after_bumps"]] <- get_after_bumps(flagged[["bumps"]], students, courses)
  
  
  # gather SUBJ_CRSE col into separate list  
  message("gathering flagged courses...")
  flagged_courses <- list()
  for (flag in flagged) {
    #print(flag)
    flagged_courses <- append(flagged_courses, flag$SUBJ_CRSE)  
  }
  
  # for now, filter for A&S
  # message("filtering for lower A&S courses...")
  # myopt[["level"]] <- "lower"
  # myopt[["college"]] <- "AS"
  # myopt[["course"]] <- NULL
  # flagged_courses <- filter_course_list(courses, flagged_courses, myopt)
  
  # convert to tibble with unique values
  message("converting to unique SUBJ_CRSE values to tibble...")
  flagged_courses <- tibble(SUBJ_CRSE = unique(flagged_courses))
  message("filtered flagged_courses has ",nrow(flagged_courses)," rows:")
  
  flagged[["all_flagged_courses"]] <- flagged_courses 
  
  # save thresholds for adding to report
  flagged[["thresholds"]] <- thresholds 
  
  # keep separate since we don't need to forecast for this all the time
  flagged[["high_fall_sophs"]] <- get_high_fall_sophs(students, courses, myopt)
  
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
