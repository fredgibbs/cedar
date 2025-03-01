# This file provides a few functions to report on NSO registrations. it shows:
# - difference in number of majors compared to previous year
# - expected number of freshman in a course given last year's course enrollment and difference in number of majors compared to last year
# - where NSO students are registered for the fall, and comparison to last year
# expected params are -t for next fall term 

# TODO: use prior year averages instead of just last year


# load NSO data from MyReports 
load_NSO_data <- function (term) {
  # studio testing
  # setwd("/Users/fwgibbs/Dropbox/college/cedar/beehive")

  message("loading NSO Data...")
  
  # unfortunate to load data separately, but reports are different
  # TODO: more robust data loading for different kinds of reports
  load(paste0(cedar_data_dir,"/processed/NSO-data-23.Rda")) # loads NSO_registrants
  NSO_registrants_23 <- NSO_registrants
  NSO_registrants_23$nso_year <- "2023"
  NSO_registrants_23 <- NSO_registrants_23 %>%  mutate (NSO_date = mdy(str_sub(SESSION_DATE,1,10)))
  
  load(paste0(cedar_data_dir,"/processed/NSO-data-24.Rda")) # loads NSO_registrants
  NSO_registrants_24 <- NSO_registrants
  NSO_registrants_24$nso_year <- "2024"
  NSO_registrants_24 <- NSO_registrants_24 %>%  mutate (NSO_date = ymd(paste0(str_sub(SESSION_DATE,1,4),"/",str_sub(SESSION_DATE,5,6),"/",str_sub(SESSION_DATE,7,8))))
  
  message("combining 2023 and 2024 data...")
  NSO_registrants <- rbind(NSO_registrants_23, NSO_registrants_24, fill=TRUE)

  # if function param TERM specified, filter for it; otherwise return both years
  if (!missing(term)) {
    message("filtering by ",term,"...")
    NSO_registrants <- NSO_registrants %>% filter(NSO_date == term)
  } 
  
  return (NSO_registrants)
}


# determine course placement based on test scores
# for each course, whittle away from top down
compute_placement <- function(NSO_registrants) {
  
  NSO_placed <- NSO_registrants %>%
    mutate(engl_placement = case_when(
      LOBO_CRSE_PLACEMENT_ENGLISH >= 40 | ACT_ENGLISH >= 29 | SAT_EVIDENCE_READ_WRITE >= 700  ~ "out",
      LOBO_CRSE_PLACEMENT_ENGLISH == 30 | (ACT_ENGLISH >= 26 & ACT_ENGLISH <= 28) | (SAT_EVIDENCE_READ_WRITE >= 660 & SAT_EVIDENCE_READ_WRITE <= 669) ~ "ENGL 1120",
      LOBO_CRSE_PLACEMENT_ENGLISH == 20 | (ACT_ENGLISH >= 16 & ACT_ENGLISH <= 25) | (SAT_EVIDENCE_READ_WRITE >= 450 & SAT_EVIDENCE_READ_WRITE <= 659) ~ "ENGL 1110",
      LOBO_CRSE_PLACEMENT_ENGLISH == 10 | (ACT_ENGLISH < 16 ) | (SAT_EVIDENCE_READ_WRITE < 450) ~ "ENGL 1110X",
      TRUE ~ "unknown"
    ))
  
  
  NSO_placed <- NSO_placed %>%
    mutate(math_placement = case_when(
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 70 | ACT_MATH >= 28 | SAT_MATH >= 640  ~ "MATH 1512",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 60 | ACT_MATH >= 25 | SAT_MATH >= 590  ~ "MATH 1230",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 50 | ACT_MATH >= 25 | SAT_MATH >= 590  ~ "MATH 1240",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 50 | ACT_MATH >= 25 | SAT_MATH >= 590  ~ "MATH 1250",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 40 | ACT_MATH >= 26 | SAT_MATH >= 620  ~ "MATH 1430",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 30 | ACT_MATH >= 22 | SAT_MATH >= 540  ~ "MATH 1130",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 30 | ACT_MATH >= 22 | SAT_MATH >= 540  ~ "MATH 1220",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 30 | ACT_MATH >= 22 | SAT_MATH >= 540  ~ "MATH 1300",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 30 | ACT_MATH >= 22 | SAT_MATH >= 540  ~ "MATH 1350",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 20 | ACT_MATH >= 19 | SAT_MATH >= 480  ~ "MATH 1118",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 20 | ACT_MATH >= 17 | SAT_MATH >= 460  ~ "MATH 1215X",
      LOBO_CRSE_PLACEMENT_MATH_STATS >= 10 | ACT_MATH <= 16 | SAT_MATH <= 459  ~ "FYEX 1010",
      TRUE ~ "unknown"
    ))
  
  #TODO: add accu placer
  #temp <- NSO_registrants %>% filter(ACCU_NEXT_GEN_WRITING >= 279)
  
  return(NSO_placed)
}


# create table of how number of majors have changed compared to last year
get_NSOers_diff <- function(NSOers,opt) {
  
  message("looking at how majors among Freshman have changed among NSO registrants from last year...")
  
  # summarize by majors
  # TODO: this DF has pre majors, so it might be useful to combine pre and regular.
  # for general forecasting, everything gets summed anyway
  NSOers_majors_summary <- NSOers %>%
    filter (str_detect(STUDENT_CLASSIFICATION_DESC, "Freshman")) %>% 
    group_by(MAJOR_DESC,nso_year) %>% 
    summarize (count = n()) %>% 
    arrange (MAJOR_DESC)
  
  # calculate percent difference in reported majors from previous fall
  NSOers_majors_summary <- NSOers_majors_summary %>% mutate(diff = count/lag(count)*100)
  
  # filter for target term (usually next fall)
  NSOers_diff <- NSOers_majors_summary %>% filter (nso_year == substr(opt$term,1,4)) %>% 
    arrange(desc(diff))
  
  # need to remove NAs for missing combinations of classification and year from comparison and target semesters
  NSOers_diff <- NSOers_diff %>% drop_na(diff)
  
  return(NSOers_diff)
}


# return list of courses and how many NSO registrants are registered for it
# this helps monitor conversation rate from NSOs sessions to registrations
# and tells us if there is unmet demand
get_NSOers_in_courses <- function (NSOers,students,opt) {
  
  message("getting NSOers already in courses...")
  
  # "merge" classlist data with NSO data
  # this now 5-6x size of NSO_placed b/c it has all course data for NSO registrants
  #NSO_regs_w_courses <- merge (NSO_placed, filtered_students, by=c("ID" = "Student ID"), keep=TRUE )
  #NSOers_w_courses <- merge (NSOers, filtered_students, by.x ="ID", by.y="Student ID", all.x=TRUE )
  
  # filter courses by opt params
  # opt should have target term; we want previous fall also for comparison
  # make sure classification is NULL?
  filtered_students <- students %>% filter_class_list(opt)
  
  # if opt param has a course so that students are filtered, don't keep all NSOers, keep just the filtered
  NSOers_w_courses <- merge (NSOers, filtered_students, by.x ="ID", by.y="Student ID")
  
  # limit to just "First-Year" in session title or "Freshman" at beginning for STUDENT_CLASSIFICATION_DESC
  #NSOers_w_courses <- NSOers_w_courses %>% filter(str_detect(STUDENT_CLASSIFICATION_DESC,"Freshman"))
  
  #NSOers_w_courses <- NSOers_w_courses %>% filter(SUBJ_CRSE == opt$course & nso_year == str_sub(`Academic Period`,start= -4))
  
  # count number of NSO registrants who registered for given courses (or all of them)
  NSOers_courses_summary <- NSOers_w_courses %>% group_by(`Academic Period`, SUBJ_CRSE) %>% 
    summarize (count = n()) %>%  arrange (SUBJ_CRSE)
  
  return (NSOers_courses_summary)
  }


# compare last two Falls (2023 and 2024) NSO data on reported majors for freshman; 
# apply diff to each critical course enrollment from last year
forecast_enrl_from_majors <- function (NSOers,students,opt) {

  message("forecasting course enrollment from NSO majors...")
  NSOers_diff <- get_NSOers_diff(NSOers,opt)
  
  # filter class lists for specified courses and terms
  #message("filtering students based on opt params...")
  #filtered_students <- students %>% filter_class_list(opt)
  
  ###### find out who were in courses last fall
  # set term to previous fall
  message("looking up who were in courses year prior to target term...")
  myopt <- opt
  myopt$term <- subtract_term(myopt$term) %>% subtract_term
  
  # call rollout; students get filtered by opt params there
  # this will return number of students in given course for each major and classification combination
  rollcall_out <- rollcall(students,myopt)
  message("rollcall_out:")
  print(rollcall_out)
  
  # as long as get_NSOers_diff focuses on freshman, filter here also
  rollcall_out <- rollcall_out %>%
    filter (str_detect(`Student Classification`, "Freshman"))
    
  # merge rollcall data (numbers of majors/classifications in each course) with NSO data
  # TODO: be more explicit about year labeling in the tables
  message("merging rollcall data with NSO data...")
  merged <- merge(rollcall_out,NSOers_diff,by.x="Major",by.y="MAJOR_DESC")
  merged <- merged %>% mutate (proj_enrl = count.x * (diff/100))
  merged$proj_enrl <- round(merged$proj_enrl,digits=0)
  print(merged)
  
  # summarize by course to see NSO contribution to enrollments across all majors
  freshman_enrls <- merged %>% 
    group_by(SUBJ_CRSE,`Academic Period Code`,`Short Course Title`) %>% 
    summarize (fresh_proj = sum(proj_enrl))
  
  return(freshman_enrls)
}



# this main body function should provide a basic report on NSO stats
# calls the above functions to generate specific tables and output/save them.
# opt should supply the Fall term in question
# opt$course_list will normally be 'critical_courses' but other lists can be used.
nosedive <- function (courses,students,opt) {
  message("Welcome to Nosedive!")

  # for studio testing
  opt <- list()
  opt$aggregate <- TRUE
  opt$term <- "202480"
  #opt$course <- "BIOL 2305"
  opt$course_list <- "critical_courses"
  opt$rollcall_agg_by <- "classification_course_major"
  opt$classification <- "Freshman"
  
  
  # load data
  NSO_registrants <- load_NSO_data()

  # add column to orig NSO data to show what courses students placed into
  NSO_placed <- compute_placement(NSO_registrants)
  
  # calc diff between majors across terms
  NSOers_diff <- get_NSOers_diff(NSO_registrants,opt)
  NSOers_diff %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  
  # forecast enrollment for specified courses based on NSO reported majors
  enrl_from_NSO_majors <- forecast_enrl_from_majors(NSO_registrants,students,opt)
  enrl_from_NSO_majors %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  

  # use forecast to get current estimate of non-freshman and add this projection
  # assuming opt$term has term we are forecasting for.
  forecast_data <- get_forecast_data(students,courses,opt) 
  
  # filter for specified term and print
  forecast_data <- forecast_data %>% filter (TERM %in% opt[["term"]])
  forecast_data %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  # merge NSO freshman projections and forecast data (remember to re-forecast w/o freshman)
  # TODO: need to do some math to calc need
  merged <- merge(enrl_from_NSO_majors,forecast_data,by="SUBJ_CRSE")
  merged %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  
  # see what courses NSOers actually registered for
  NSOers_in_courses <- get_NSOers_in_courses(NSO_registrants,students,opt)
  NSOers_in_courses %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
}
