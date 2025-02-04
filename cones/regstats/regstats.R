
# filtered_students should be term agnostic, but limited to opt params or defaults for courses
calc_squeezes <- function(filtered_students,filtered_courses,opt) {
  message("finding enrollment squeezes...")
  
  # get final enrollments; need to use get_enrl to get final number of available seats
  myopt <- opt
  myopt[["aggregate"]] <- "course"
  myopt[["uel"]] <- "true"
  myopt[["term"]] <- NULL # need enrollments across all terms for mean calcs
  
  enrls <- get_enrl(filtered_courses,myopt)
  message("enrls from CALC_SQUEEZES")
  print(enrls)
  
  # get registration status
  cl_drops <- calc_cl_enrls(filtered_students)

  # merge drop data w course data
  squeezes <- merge(enrls,cl_drops,by.x=c("TERM","SUBJ_CRSE"),by.y=c("Academic Period Code","SUBJ_CRSE"),all.x=TRUE )
  
  # add term_type
  squeezes <- add_term_type_col(squeezes,"TERM")
  
  # remove NAs if we're getting data for future terms with NA drops
  squeezes <- na.omit(squeezes)
  
  # find mean drops across term types
  # squeezes <- squeezes %>% group_by (SUBJ_CRSE,term_type) %>% mutate(mean_drops = mean(drops))

  # find "squeeze" amount -- the ratio of available seats to mean drops (all drops, not just late)
  squeezes <- squeezes %>% mutate(squeeze = round(avail/da_mean,digits=2))
  message("all done in calc_squeezes.")
  return(squeezes)
}


filter_squeezes <- function(squeezes,thresholds,opt) {
  
    # filter only for those courses where we have less open spots at end of term than the mean attrition
  squeezes <- squeezes %>%
    filter (enrolled >= thresholds[["min_count"]]) %>%  
    filter (squeeze < thresholds[["min_squeeze"]]) 
  
  # keep only data for specified term now that course stats calcs are done
  if (!is.null(opt[["term"]])) {
    squeezes <- filter_by_term(squeezes,opt[["term"]],"TERM") 
  }
  
  squeezes <- squeezes %>% arrange(term_type,TERM,squeeze)
  
  message("all done calculating squeezes!")
  
  return(squeezes)
}


set_defaults <- function(opt) {
  # if no term specified, use next term
  if (is.null(opt[["term"]])) {
    # TODO: handle summer, both from --summer opt param and default
    next_term <- add_term(cedar_current_term,opt[["summer"]])
    message("no opt$term param found. setting opt$term to: ",next_term)
    opt[["term"]] <- next_term
  }  
  
  if (is.null(opt[["level"]])) {
    message("no opt$level param found. setting opt$level to: lower")
    opt[["level"]] <- "lower"
  }  
  
  if (is.null(opt[["college"]])) {
    message("no opt$college param found. setting opt$term to: AS")
    opt[["college"]] <- "AS"
  }  
  
  return(opt)
}


# use rollcall to find popular fall courses among sophomores for potential summer offerings
# rollcall -a course_classification_avg --classification 'Sophomore, 2nd Yr' --arrange mean --coursecollege AS -t fall
get_high_fall_sophs <- function (students,courses,opt) {
  
  message("getting fall courses with 100+ sophomores for potential summer offerings...")
  myopt <- list()
  myopt[["aggregate"]] <- "course_classification_avg"
  myopt[["classification"]] <- "Sophomore, 2nd Yr"
  myopt[["coursecollege"]] <- "AS"
  myopt[["term"]] <- "fall"
  rollcall_out <- rollcall(students,myopt)
  
  print(rollcall_out)
  
  # 100 is a bit arbitrary; not sure how to calc would what be a better threshold
  rollcall_out <- rollcall_out %>% filter(mean > 100)
  
  
  # grab just SUBJ_CRSE col
  high_fall_sophs <- tibble(SUBJ_CRSE = unique(rollcall_out$SUBJ_CRSE))
  
  message("all done getting high fall sophs!")
  
  return(as_tibble(high_fall_sophs))
}


# single out bumps to forecast for where_to courses 
get_after_bumps <- function (bumps,courses,opt) {
  
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
    
    where_tos <- where_to(students,myopt) %>% arrange (desc(avg_contrib))
    next_courses <- head(where_tos,n=5)
    
    after_bumps <- c(after_bumps, next_courses$SUBJ_CRSE)
    message("after_bumps now looks like:",after_bumps)
  } # end loop through bumps to find next courses
  
  after_bumps <- unique(tibble(SUBJ_CRSE = after_bumps))
  
  message("done assembling after bumps:")
  print(after_bumps)
  return(after_bumps)
}


# get a summary of registration status via class lists
# course and term agnostic
get_reg_data <- function(filtered_students,opt) {
  message("welcome to get_reg_data!")
  
  # uncomment for testing
  # students <- load_students(opt)
  # filtered_students <- students %>% filter (SUBJ_CRSE %in% ...)
  
  # group and summarize class_lists
  message("computing summary stats...")
  filtered_students <- filtered_students %>% group_by(`Academic Period Code`,SUBJ_CRSE,`Registration Status Code`,term_type)
  summary_by_term <- filtered_students %>% summarize(count = n(), .groups="keep")
  summary_by_term <- summary_by_term %>% group_by(SUBJ_CRSE,term_type,`Registration Status Code`)
  summary <- summary_by_term %>% summarize (mean = round(mean(count),digits=2), sd = round(sd(count),digits=2), .groups="keep")
  
  regstats <- merge(summary_by_term,summary, by=c("SUBJ_CRSE","Registration Status Code","term_type"))
  
  message("get_reg_data now returning regstats...")
  return (regstats)
}


# this function identifies courses of concern for dips and bumps,
# in the sense that some enrollment stats are outside thresholds set in config.R
get_dimp_concerns <- function(regstats,thresholds,sign) {
  #print(regstats)
  if (sign=="plus") {
    concerns <- regstats %>%  mutate(pct_sd = round((count - mean) / sd,digits=2), impacted =  round((count - (mean + sd)),digits=2)) %>% 
      filter (impacted > thresholds[["min_impacted"]])  %>% 
      filter (count > thresholds[["min_count"]]) %>% 
      filter (pct_sd > thresholds[["min_pct_sd"]])
  } else if (sign =="minus") {
    concerns <- regstats %>%  mutate(pct_sd = round((mean - count) / sd,digits=2), impacted =  round((mean - sd) - count,digits=2)) %>% 
      filter (impacted > thresholds[["min_impacted"]])  %>% 
      filter (count > thresholds[["min_count"]]) %>% 
      filter (pct_sd > thresholds[["min_pct_sd"]])
  }
  
  return(concerns)
}


get_dimp_bumps <- function(regstats,thresholds) {
  bumps <- regstats %>% filter (substring(`Registration Status Code`,1,1) == "R") 
  bumps <- get_dimp_concerns(bumps,thresholds,"plus")
  bumps <-  bumps %>% arrange (desc(impacted))
  
  return (bumps)
}


get_dimp_dips <- function(regstats,thresholds) {
  dips <- regstats %>% filter (substring(`Registration Status Code`,1,1) == "R") 
  message("DIPS")
  print(dips)
  dips <- get_dimp_concerns(dips,thresholds,"minus")
  dips <-  dips %>% arrange (impacted)
  
  return (dips)
}


get_dimp_drops <- function(regstats,thresholds) {
  drops <- regstats %>% filter (substring(`Registration Status Code`,1,1) == "D") 
  drops <- get_dimp_concerns(drops,thresholds,"plus")
  drops <-  drops %>% arrange (desc(impacted))
  
  return(drops)
}


# find high wait list courses from courses list (not actually regstats)
get_dimp_waits <- function(students, opt, thresholds) {
  myopt <- opt
  # if no course specified, defaults already set to AS lower-division
  myopt[["uel"]] <- TRUE
  myopt[["aggregate"]] <- "course"
  
  waits <- get_enrl(students, myopt)
  waits <-  waits %>% filter (waiting > thresholds[["min_wait"]]) %>% arrange (desc(waiting))
  
  return(waits)
}


# main function called by cedar
get_reg_stats <- function(students,courses,opt) {
  message("\n welcome to get_reg_stats!")
  
  # grab default thresholds from config.R
  message("setting default thresholds...")
  thresholds <- cedar_regstats_thresholds
  
  myopt <- opt
  
  # if no course specified, use all lower division AS courses
  if (is.null(opt$course)) {
    # message("no course specified. defaulting to AS lower-division courses.")
    # myopt[["course"]] <- NULL
    # myopt[["level"]] <- "lower"
    # myopt[["college"]] <- "AS"
    course_list <- as.list(get_course_list(courses,myopt))
  } 
  else {
    message("processing opt$course...")
    course_list <- convert_param_to_list(opt[["course"]])
    
    # TODO: use lower thresholds for specific course reporting?
  }
  
  
  # do course filtering early, but not term
  message("filtering COURSES by course_list...")
  filtered_courses <- courses %>% filter (SUBJ_CRSE %in% course_list)
  
  message("filtering STUDENTS by course_list...")
  filtered_students <- students %>% filter (SUBJ_CRSE %in% course_list)
  message("left with ",nrow(filtered_students)," students.")
  
  # get registration and enrollment stats  
  regstats <- get_reg_data(filtered_students,myopt)
  
  # filter by term AFTER getting regstats, so get_reg_data can get mean values across terms
  if (!is.null(opt$term)) {
    message("filtering for term...")
    regstats <- filter_by_term(regstats,opt$term,"Academic Period Code")
  }
  
  # find potential anomalies
  message("finding courses of interest...")
  flagged <- list()
  flagged[["drops"]] <- get_dimp_drops(regstats, thresholds)
  flagged[["dips"]] <- get_dimp_dips(regstats, thresholds)
  flagged[["bumps"]] <- get_dimp_bumps(regstats, thresholds)
  flagged[["after_bumps"]] <- get_after_bumps(flagged[["bumps"]], thresholds)
  flagged[["waits"]] <- get_dimp_waits(courses, myopt, thresholds)
  
  squeezes <- calc_squeezes(filtered_students, filtered_courses, myopt)
  flagged[["squeezes"]] <- filter_squeezes(squeezes, thresholds, opt)
  
  # gather SUBJ_CRSE col into separate list  
  message("gathering flagged courses...")
  flagged_courses <- list()
  for (flag in flagged) {
    print(flag)
    flagged_courses <- append(flagged_courses, flag$SUBJ_CRSE)  
  }

  # for now, filter for A&S
  message("filtering for lower A&S courses...")
  myopt[["level"]] <- "lower"
  myopt[["college"]] <- "AS"
  myopt[["course"]] <- NULL
  flagged_courses <- filter_course_list(courses, flagged_courses, myopt)
  
  # convert to tibble with unique values
  message("converting to unique SUBJ_CRSE values to tibble...")
  flagged_courses <- tibble(SUBJ_CRSE = unique(flagged_courses))
  message("filtered flagged_courses has ",nrow(flagged_courses)," rows:")

  flagged[["all_flagged_courses"]] <- flagged_courses 
  
  # save thresholds for adding to report
  flagged[["thresholds"]] <- thresholds 
  
  # keep separate since we don't need to forecast for this all the time
  flagged[["high_fall_sophs"]] <- get_high_fall_sophs(students, courses, myopt)

  
  # output to terminal 
  message("courses with enrollment dips:")
  flagged[["dips"]] %>% tibble::as_tibble() %>% print(n=nrow(.), width=Inf)  
  
  message("courses with enrollment bumps:")
  flagged[["bumps"]] %>% tibble::as_tibble() %>% print(n=nrow(.), width=Inf)  
  
  message("courses with higher than usual drops:")
  flagged[["drops"]] %>% tibble::as_tibble() %>% print(n=nrow(.), width=Inf)  
  
  message("courses with long wait lists:")
  flagged[["waits"]] %>% tibble::as_tibble() %>% print(n=nrow(.), width=Inf)  
  
  message("courses with enrollment squeezes:")
  flagged[["squeezes"]] %>% tibble::as_tibble() %>% print(n=nrow(.), width=Inf)  
  
  return(flagged)
}


create_regstat_report <- function(flagged, opt) {
  
  # flagged <- get_reg_stats(students,courses,opt)
  
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
  d_params$rmd_file <- "cones/regstats/regstats-report.Rmd"
  d_params$output_dir_base <- paste0(cedar_output_dir,"regstats-reports/")
  
  create_report(opt,d_params)
  
  message("all done creating regstats report!")
}
