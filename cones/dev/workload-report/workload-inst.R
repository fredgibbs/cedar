# output several vignettes of seeing what people are teaching

workload_inst_func <- function (all_courses,opt) {
  message("Welcome to workload_inst!")

  # TODO: add academic year?
  
  optcopy <- opt
  optcopy$dept <- NULL

  #TODO: verify there is some inst string (either opt param maybe look up from hr table based on opt$dept)
  dept_fac_all_courses <- filter_DESRs(all_courses,optcopy) %>% ungroup()
  
  #not sure why there are dupes (as_of date again?)
  dept_fac_all_courses <- dept_fac_all_courses %>% group_by(TERM,SUBJ_CRSE) %>% distinct(TERM,SUBJ_CRSE,.keep_all = T)

  #Instead of just de-duping this way, get summary row of enrollments for same SUBJ_CRSE listings and rbind to DF
  
  # better data display...
  dept_fac_all_courses <- dept_fac_all_courses %>% mutate (gen_ed = ifelse(SUBJ_CRSE %in% gen_ed_all,T,F))
  dept_fac_all_courses <- dept_fac_all_courses %>% mutate (xl_enrl = total_enrl-ENROLLED)
  # dept_fac_all_courses <- dept_fac_all_courses %>% select(TERM,DEPT,SUBJ_CRSE,CRSE_TITLE,INST_METHOD,INST_NAME,`Academic Title`,`Home Organization Desc`,ENROLLED,total_enrl,xl_enrl,gen_ed)
  # dept_fac_all_courses <- dept_fac_all_courses %>% rename(TITLE = `Academic Title`,DEPARTMENT = `Home Organization Desc`, DEPT_ENRL=ENROLLED,TOTAL_ENRL=total_enrl,XLIST_ENRL=xl_enrl,GEN_ED=gen_ed)
  

  
   
  filename <- paste0("workload/workload_",opt$dept,"_fac_all_courses.csv")
  message("saving dept_fac_all_courses as: ",filename,"...")
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
  
  
  # # create basic enrollment over time plot
  # courses <- load_courses(opt)
  # optcopy <- opt
  # optcopy$term <- "201780-202410"
  # enrl_by_year <- rollout(optcopy,courses)
  # enrl_summary <- enrl_by_year %>% group_by(TERM) %>% summarise(ENRL = sum(total_enrl), AVG = mean(total_enrl), NUM = n())
  # 
  # ggplot(enrl_summary, aes(x=TERM)) + 
  #         geom_line(aes(y=ENRL),group=1) +
  #         theme(axis.text.x = element_text(angle = -45, hjust=0.05))
  # 
  # ggplot(enrl_summary, aes(x=TERM)) + 
  #   geom_point(aes(y=AVG),group=2) + geom_line(aes(y=AVG),group=2) +
  #   geom_point(aes(y=NUM),group=3,color="red") + geom_line(aes(y=NUM),group=3,color="red") +
  #   theme(axis.text.x = element_text(angle = -45, hjust=0.05))
  
  
  # if no fac_list defined, set to null
  if (exists(paste0(opt$dept,"_fac"))) {
    fac_list <- as.list(names(get(paste0(opt$dept,"_fac"))))
    message("fac_list set to: ", paste0(opt$dept,"_fac"))
  } else {
    fac_list <- NULL
    message("no fac_list defined") 
  }
    fac_list <- as.list(names(get(paste0("chairs_fac"))))
  
  # payload
  d_params <- list("dept" = opt$dept,
                   "term"  = opt$term,
                   "fac_list" = fac_list,
                   #"faculty" = AFST_faculty, # just for loading special faculty table
                   "tables" = list(
                                   "dept_fac_all_courses"=dept_fac_all_courses, 
                                   "inst_courses_w"=inst_courses_w
                   )
  )
  
  filename <- "chairs"
  output_file <- paste0("./",filename,'-workload-report.html')
  
  xfun::Rscript_call(
    rmarkdown::render,
    list(input = 'workload/workload-inst.Rmd', output_format = 'html_document',output_file = output_file,params = d_params)
  )
  
  message("done rendering.")
  
  return(workload_w)
} 

