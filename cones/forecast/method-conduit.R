###################### CONDUIT METHOD ##########################
# For a given course: 
# 1) Identify what courses students are coming from before taking the given course. These are the "conduit" courses.
# 2) Identify how enrollments in the conduit courses changed from a year prior to target course.
# 3) Apply that pct diff to student demographics in target course

# TODO: see if where_from output table can be used here. 
# TODO: does this count the same student multiple times in different conduit courses?
# TODO: this uses only last year's data; it should use an average of past years


# create a get_avg_enrl func in enrl.R that returns avg enrollment for set of courses and term type
# 
# use  weighted avg of conduit courses
# get enrl diff from each course to get average diff, but weight the conduit courses by contribution to target to get a composite enrollment diff
# then apply diff to total enrollment from prior year


conduit_forecast <- function(students,courses,opt) {
  message("forecasting via conduit...")
  
  # for studio testing...
  # students <- load_students(opt)
  # courses <- load_courses(opt)
  # opt <- list()
  # opt[["course"]] <- "HIST 491"
  # opt[["term"]] <- "202480"
  # 
  opt[["summer"]] <- FALSE

  target_course <- opt[["course"]]
  target_term <- opt[["term"]]
  
  # subtract term defaults to skipping summer terms
  # TODO: send the opt$summer param to subtract term
  conduit_term <- subtract_term(target_term)
  prev_target_term <- subtract_term(conduit_term)
  prev_conduit_term <- subtract_term(prev_target_term)
  
  # get unique ids from students in prev target course (1 year previous to target course/term)
  # in other words: who was in the this course a year prior to when we're forecasting?
  # this is the "baseline" enrollment to which we will apply adjustments
  conduits_contributions <- where_from (students,opt)
  #  SUBJ_CRSE term_type avg_contrib to_crse
  # ECON 2110 fall              4   HIST 412  
  
  
  # how did conduit enrollments change from one year to the next?
  message("finding conduit enrollments...")
  
  # get enrollments from prev_conduit and conduit and use lag to see diff
  myopt <- opt
  myopt[["course"]] <- as.list(conduits_contributions$SUBJ_CRSE)
  myopt[["term"]] <- paste0(conduit_term,",",prev_conduit_term)
  myopt[["aggregate"]] <- "course"
  
  conduit_enrls <- get_enrl(courses,myopt) %>% 
    group_by(SUBJ_CRSE,TERM) %>% 
    select(SUBJ_CRSE, TERM, enrolled) %>%  
    arrange(SUBJ_CRSE)
  
  # make sure all terms and courses are present
  conduit_enrls <- conduit_enrls %>% ungroup() %>% complete(SUBJ_CRSE, TERM, fill=list(enrolled=0))
  
  # calc enrollment differences between conduit terms
  enrl_diffs <- conduit_enrls %>% group_by(SUBJ_CRSE) %>% 
    mutate(conduit_enrl_diff = enrolled/lag(enrolled))

  # filter out meaningless diffs  
  enrl_diffs <- enrl_diffs %>% filter (TERM == conduit_term & is.finite(conduit_enrl_diff) & !is.na(conduit_enrl_diff) & enrolled > 0)

  # merge conduit contributions with enrl_diffs
  merged <- merge(conduits_contributions, enrl_diffs)

  # get mean enrollments from previous target courses
  myopt <- opt
  myopt[["course"]] <- target_course
  myopt[["term"]] <- NULL
  myopt[["aggregate"]] <- "course"
  
  target_enrls <- get_enrl(courses,myopt) %>% 
    ungroup() %>% 
    mutate (mean_enrl = mean(enrolled)) %>% 
    select (TERM,SUBJ_CRSE,mean_enrl) %>% 
    tail(n=1)
  
  merged$target_mean_enrl <- target_enrls$mean_enrl
  
  # calculate the percent of students in target course coming from conduit
  merged <- merged %>% mutate(pct_from_conduit = avg_contrib / target_mean_enrl)   
  
  # we want to know how much the conduit courses have changed in enrollment compared to last year
  # but they don't contribute equally to the target course, so use a weighted mean
  
  # for fun, see what the standard mean is:
  mean_std <- mean(merged$conduit_enrl_diff)
  
  mean_weighted <- weighted.mean(merged$conduit_enrl_diff, w = merged$pct_from_conduit)
  
  proj <-  target_enrls$mean_enrl * mean_weighted
  
  new_summary<-data.frame(target_term, target_course, "conduit", proj )
  names(new_summary)<-c("TERM","SUBJ_CRSE","method","forecast")
  
  
  # add to forecast table 
  add_to_forecast_table(new_summary)
  
} 
