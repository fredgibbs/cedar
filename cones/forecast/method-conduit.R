###################### CONDUIT METHOD ##########################
# For a given "target" course and term: 
# 0) Get Student IDs for students in previous (1 year ago) target course 
# 1) Identify what courses students are coming from before taking the given course. These are the "conduit" courses.
# 2) Identify how enrollments in the conduit courses changed from a year prior to target course.
# 3) Apply that pct diff to student demographics in target course

# TODO: does this count the same student multiple times in different conduit courses?
# TODO: this uses only last year's data; it should use an average of past years


# create a get_avg_enrl func in enrl.R that returns avg enrollment for set of courses and term type
# 
# use  weighted avg of conduit courses
# get enrl diff from each course to get average diff, but weight the conduit courses by contribution to target to get a composite enrollment diff
# then apply diff to total enrollment from prior year
# REQUIRES: course and term

conduit_forecast <- function(students,courses,opt) {
  message("\n FORECASTING VIA CONDUIT...")
  
  #for studio testing...
  # students <- load_students()
  # courses <- load_courses()
  # opt <- list()
  # opt[["course"]] <- "ENGL 2993"
  # opt[["term"]] <- "202560"
  # opt[["custom_conduit"]]  <- "202480"
  # opt[["conduit_for_term"]] <- "202560"
  
  target_course <- opt[["course"]]
  target_term <- opt[["term"]]

  message("target course: ",target_course,"; target_term: ",target_term,".")
    
  # see if user has supplied a conduit term besides default of target-1. (for projecting summer from fall...)
  # note: subtract_term defaults to ignoring summer, which is almost always want to do
  if (!is.null(opt[["custom_conduit"]]) && target_term == opt[["conduit_for_term"]]) {
    message("target term matches conduit_for_term; using custom_conduit.")
    conduit_term <- opt[["custom_conduit"]]  
  } else {
    # no special conduits; business as usual
    conduit_term <- subtract_term(target_term)   
  }
  
  message("conduit term set to ",conduit_term,".")
  
  message("subtracting year from target and conduit terms for enrollment refs...")
  prev_target_term <- as.integer(target_term) - 100
  prev_conduit_term <- as.integer(conduit_term) - 100
  
  # get unique ids from students in prev target course (1 year previous to target course/term)
  # in other words: who was in the this course a year prior to when we're forecasting?
  # this is the "baseline" enrollment to which we will apply adjustments
  
  # be careful with summer flag. if accidentally on, what's returned from where_from might not have any rows one of the conduit semesters
  # note: where_from is term agnostic, so it gets mean of contributing enrollments
  
  #print(opt)
  conduits_contributions <- where_from (students,opt)
  #print(conduits_contributions)
  
  #  SUBJ_CRSE term_type avg_contrib to_crse
  # ECON 2110 fall              4   HIST 412  
  
  # keep only term_type of conduit course
  message("filtering conduit contributions by conduit term type...")
  conduits_contributions <- conduits_contributions %>% filter (term_type == get_term_type(conduit_term))
  #print(conduits_contributions)
  
  # determine how conduit enrollments changed from one year to the next
  message("finding conduit enrollments...")
  
  # get enrollments from prev_conduit and conduit and use lag to see diff
  myopt <- opt
  myopt[["course"]] <- as.list(conduits_contributions$SUBJ_CRSE)
  myopt[["term"]] <- paste0(conduit_term,",",prev_conduit_term)
  myopt[["aggregate"]] <- "course"
  
  # the summarize here combines enrolled col for same SUBJ_CRSE (like topics courses)
  # note that we can't forecast via conduits if we don't have prior semester enrollments
  # ie we can't skip a term 
  conduit_enrls <- get_enrl(courses,myopt) %>% 
    group_by(SUBJ_CRSE,TERM) %>% 
    summarize (enrolled = sum(enrolled)) %>% 
    select(SUBJ_CRSE, TERM, enrolled) %>%  
    arrange(SUBJ_CRSE)
  
  #if no conduit data, (ie we're forecasting for summer in the fall), make the conduits be one earlier semester
  if (nrow( conduit_enrls %>% filter (TERM == conduit_term)) == 0) {
    message("\n can't find any enrollment data for true conduit term. Trying one term back from original conduit term...")
    conduit_term <- subtract_term(conduit_term)
    prev_target_term <- as.integer(target_term) - 100
    prev_conduit_term <- subtract_term(prev_conduit_term)
    
    myopt[["term"]] <- paste0(conduit_term,",",prev_conduit_term)
    
    conduit_enrls <- get_enrl(courses,myopt) %>% 
      group_by(SUBJ_CRSE,TERM) %>% 
      summarize (enrolled = sum(enrolled)) %>% 
      select(SUBJ_CRSE, TERM, enrolled) %>%  
      arrange(SUBJ_CRSE)
  }
  
  if (nrow(conduit_enrls > 0)) {
  
  # make sure each course has an enrollment (even if 0) for both terms
  conduit_enrls <- conduit_enrls %>% ungroup() %>% complete(SUBJ_CRSE, TERM, fill=list(enrolled=0))
  
  # calc enrollment differences between conduit terms
  message("calcuating differences between conduit terms...")
  enrl_diffs <- conduit_enrls %>% group_by(SUBJ_CRSE) %>% 
    mutate(conduit_enrl_diff = enrolled/lag(enrolled))

  #print(enrl_diffs)
  
  
  # filter out meaningless diffs (NAs and Infs)
  # these result from one of the semesters having 0 enrollment; not much we can learn if a course wasn't offered 
  enrl_diffs <- enrl_diffs %>% filter (TERM == conduit_term & is.finite(conduit_enrl_diff) & !is.na(conduit_enrl_diff) & enrolled > 0)

  # merge conduit contributions with enrl_diffs
  message("merging conduit contributions with enrl_diffs")
  merged <- merge(conduits_contributions, enrl_diffs)

  #print(merged)
  
  # get mean enrollments from previous TARGET courses **with same term type**
  myopt <- opt
  myopt[["course"]] <- target_course
  myopt[["term"]] <- get_term_type(target_term)
  myopt[["aggregate"]] <- "course"
  
  # TODO: provide mean or last term options
  message("getting mean enrollment of target course in same term type of target term...")
  
  target_enrls <- get_enrl(courses,myopt) %>%
    ungroup() %>%
    # mutate (mean_enrl = mean(enrolled)) %>%
    mutate (mean_enrl = enrolled) %>%
    select (TERM,SUBJ_CRSE,mean_enrl)

    
  # check to make sure we have some rows
  message("checking for prior enrollments...")
  if (nrow(target_enrls) > 0) {
    message("prior enrollments found.")
    target_enrls <- target_enrls %>% tail(n=1)
    merged$target_mean_enrl <- target_enrls$mean_enrl
  } else {
    message("no enrollments found.")
    merged$target_mean_enrl <- 0
    message("target_mean_enrl (in target term type) set to 0.")
  }
  
  
  # calculate the percent of students in target course coming from conduit
  message("calculating the percent of students from conduit course...")
  merged <- merged %>% mutate(pct_from_conduit = avg_contrib / target_mean_enrl)   
  
  # we want to know how much the conduit courses have changed in enrollment compared to last year
  # but they don't contribute equally to the target course, so use a weighted mean
  mean_weighted <- weighted.mean(merged$conduit_enrl_diff, w = merged$pct_from_conduit)
  print(paste0("mean weighted: ",mean_weighted))
  
  message("forecasting target enrollment from previous semester and weighted mean of contributions.")
  proj <-  target_enrls$mean_enrl * mean_weighted
  
  message("forecast: ", class(proj))
  print(paste0("proj: ",proj))
  
  if (is.nan(mean_weighted)) {
    message("no projection! probably no prior enrollments.")
    proj <- 0
  } 
  
  } else { # could not find conduit enrollments
    message("no projection! could not find conduit enrollments.")
    proj <- 0
  }
  
  # continuning and incoming columns used by major method, so need to be consistent with columns
  new_summary <- data.frame(target_term, target_course, "conduit", round(proj,digits=0), 0, 0)
  names(new_summary)<-c("TERM","SUBJ_CRSE","method","forecast","continuing_forecast","incoming_forecast")
  
  # add to forecast table 
  add_to_forecast_table(new_summary)
  
    
} 
