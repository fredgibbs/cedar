# output several vignettes of seeing what people are teaching

workload_func <- function (courses,opt) {
  message("Welcome to workload!")

  # TODO: add academic year?
  
  #TODO: not sure if i should check for a file like this or assume this is a one off
  #AFST_faculty <- read_xlsx("workload/AFST_faculty.xlsx",col_type="text")
  
  # two different tables seem useful here
  # 1) all courses in specified unit (with subj codes in dept)
  # 2) courses by all faculty in unit

  # get list of all courses by dept faculty across units
  # filtering by inst (esp w name strings) is best done via filter_DESRs.
  # minor filtering or subsetting is easily done here
  optcopy <- opt
  optcopy$dept <- NULL

  #TODO: verify there is some inst string (either opt param maybe look up from hr table based on opt$dept)
  dept_fac_all_courses <- filter_DESRs(courses,optcopy) %>% ungroup()
  
  # remove duplicates
  dept_fac_all_courses <- dept_fac_all_courses %>% distinct()
  
  # flag gen ed courses
  dept_fac_all_courses <- dept_fac_all_courses %>% mutate (gen_ed = ifelse(SUBJ_CRSE %in% gen_ed_all,T,F))
  
  # highlight xl enrollment
  dept_fac_all_courses <- dept_fac_all_courses %>% mutate (xl_enrl = total_enrl-ENROLLED)
  
  filename <- paste0("workload/workload_",opt$dept,"_fac_all_courses.csv")
  message("saving workload_",opt$dept,"_fac_all_courses.csv...")
  write.csv(dept_fac_all_courses,file=filename)
  
  message("grouping by inst...")
  inst_courses <- dept_fac_all_courses %>% group_by(TERM,INST_NAME,`Academic Title`,job_cat,`Home Organization Desc`) %>% 
    arrange(INST_NAME,TERM) %>% 
    summarize (courses_this_term = n()) 
  
  message("creating wide version...")
  inst_courses_w <- inst_courses %>% pivot_wider(names_from=TERM,values_from = courses_this_term ) %>% 
    arrange(INST_NAME)
  names <- names(inst_courses_w)
  newnames <- lapply(names,term_code_to_str)
  colnames(inst_courses_w) <- c(newnames)
  
  
  dept_fac_all_courses <- dept_fac_all_courses %>% select(TERM,DEPT,SUBJ_CRSE,CRSE_TITLE,INST_METHOD,INST_NAME,`Academic Title`,`Home Organization Desc`,ENROLLED,total_enrl,xl_enrl,gen_ed)
  dept_fac_all_courses <- dept_fac_all_courses %>% rename(TITLE = `Academic Title`,DEPARTMENT = `Home Organization Desc`, DEPT_ENRL=ENROLLED,TOTAL_ENRL=total_enrl,XLIST_ENRL=xl_enrl,GEN_ED=gen_ed)
  
  
  # filter according to orig params,
  # but don't filter by INST to get instructors from all unites teaching courses in a unit
  # if no dept is set (getting workloads for chairs, say), skip?
  optcopy <- opt
  optcopy$inst <- NULL
  optcopy$inst_list <- NULL
  optcopy$uel <- TRUE
  
  dept_courses <- filter_DESRs(courses,optcopy) %>% ungroup()
  
  # add col to indicate if gen ed course
  dept_courses <- dept_courses %>% mutate (gen_ed = ifelse(SUBJ_CRSE %in% gen_ed_all,T,F))
  
  # add col for just xl enrollment
  dept_courses <- dept_courses %>% mutate (xl_enrl = total_enrl-ENROLLED)
  
  # TODO: add col if required for a unit program; don't have great way to indicate prog_reqs
  # dept_courses <- dept_courses %>% rowwise() %>% mutate (reqs = paste((unlist(prog_reqs[SUBJ_CRSE])),collapse=", ")) %>% ungroup()

  # commented out b/c this filtering now is in workload.Rmd
  #quick table just of GEN ED
  #gen_ed <- dept_courses %>% filter (gen_ed == TRUE) %>%  select(TERM,SUBJ,CRSE_TITLE,INST_METHOD,INST_NAME,`Academic Title`,ENROLLED,total_enrl,xl_enrl,reqs)
  
  #quick table just of required courses
  #reqs <- dept_courses %>% filter (reqs != "") %>%  select(TERM,SUBJ,CRSE_TITLE,INST_METHOD,INST_NAME,`Academic Title`,ENROLLED,total_enrl,xl_enrl,reqs)
  
  
  # before output, remove extraneous cols
  courses_brief <- dept_courses %>%  select(TERM,SUBJ_CRSE,CRSE_TITLE,INST_METHOD,INST_NAME,`Academic Title`,`Home Organization Desc`,ENROLLED,total_enrl,xl_enrl,gen_ed)
  courses_brief <- courses_brief %>% rename(TITLE = `Academic Title`,DEPARTMENT = `Home Organization Desc`, DEPT_ENRL=ENROLLED,TOTAL_ENRL=total_enrl,XLIST_ENRL=xl_enrl,GEN_ED=gen_ed)

  
  #message("courses_brief: ")
  #courses_brief %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  filename <- paste0("workload/workload_",opt$dept,"_courses.csv")
  message("saving courses_brief as: ",filename,"...")
  write.csv(courses_brief,file=filename)
  
  # group by instructor and create wide view for easy counts per term (or acad year)
  message("grouping by inst...")
  workload <- dept_courses %>% group_by(TERM,INST_NAME,`Academic Title`,job_cat,`Home Organization Desc`) %>% 
    arrange(INST_NAME,TERM) %>% 
    summarize (courses_this_term = n()) 
  #workload %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  
  # convert columns with term codes to term strings
  # TODO: maybe do this before moving to wide, since it avoids col name issues and is basic data transformation
  message("creating wide version...")
  workload_w <- workload %>% pivot_wider(names_from=TERM,values_from = courses_this_term ) %>% 
    arrange(INST_NAME)
  names <- names(workload_w)
  newnames <- lapply(names,term_code_to_str)
  colnames(workload_w) <- c(newnames)

  #workload_w %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)

  # commenting out since these are easily derived in workload.Rmd
  # workload_avg <- courses %>% group_by(`job_cat`) %>% summarize(count=n(),mean=count/length(unique(courses$TERM)))
  # workload_avg %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  # 
  # ggplot(workload_avg, aes(x="", y=mean, fill=job_cat)) +
  #   geom_bar(stat="identity", width=1, color="white") +
  #   coord_polar("y", start=0) +
  #   theme_void() # remove background, grid, numeric labels
  # 
  # 
  # student_pct <- courses %>% group_by(`job_cat`) %>% summarize(students=sum(total_enrl),mean=students/length(unique(courses$TERM)))
  # student_pct %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  # 
  # ggplot(student_pct, aes(x="", y=mean, fill=job_cat)) +
  #   geom_bar(stat="identity", width=1, color="white") +
  #   coord_polar("y", start=0) +
  #   theme_void() # remove background, grid, numeric labels
  
  # create basic enrollment over time plot
  optcopy <- opt
  optcopy$term <- "201780-202410"
  enrl_by_year <- rollout(optcopy,courses)
  enrl_summary <- enrl_by_year %>% group_by(TERM) %>% summarise(ENRL = sum(total_enrl), AVG = mean(total_enrl), NUM = n())
  
  ggplot(enrl_summary, aes(x=TERM)) + 
          geom_line(aes(y=ENRL),group=1) +
          theme(axis.text.x = element_text(angle = -45, hjust=0.05))
  
  ggplot(enrl_summary, aes(x=TERM)) + 
    geom_point(aes(y=AVG),group=2) + geom_line(aes(y=AVG),group=2) +
    geom_point(aes(y=NUM),group=3,color="red") + geom_line(aes(y=NUM),group=3,color="red") +
    theme(axis.text.x = element_text(angle = -45, hjust=0.05))
  
  
  
  # if no fac_list defined, set to null
  if (exists(paste0(opt$dept,"_fac"))) {
    fac_list <- as.list(names(get(paste0(opt$dept,"_fac"))))
    message("fac_list set to: ", paste0(opt$dept,"_fac"))
  } else {
    fac_list <- NULL
    message("no fac_list defined") 
  }
    
  
  # payload
  d_params <- list("dept" = opt$dept,
                   "term"  = opt$term,
                   "fac_list" = fac_list,
                   #"faculty" = AFST_faculty, # just for loading special faculty table
                   "tables" = list("dept_courses"=dept_courses, 
                                   "dept_fac_all_courses"=dept_fac_all_courses, 
                                   "workload_w"=workload_w,
                                   "courses_brief"=courses_brief,
                                   "enrl_summary"=enrl_summary)
  )
  
  filename <- sub(" ", "_", opt$dept)
  output_file <- paste0("./",filename,'-workload-report.html')
  
  xfun::Rscript_call(
    rmarkdown::render,
    list(input = 'workload/workload.Rmd', output_format = 'html_document',output_file = output_file,params = d_params)
  )
  
  message("done rendering.")
  
  return(workload_w)
} 

