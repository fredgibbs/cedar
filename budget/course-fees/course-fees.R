# this function provides lists of fees by course, dept_subj, and dept
# no params required, but more useful if filtered by dept or a set of courses

fee_report <- function (courses,opt) {
  message("Welcome to Fee Report!")

  #opt <- list()
  #opt$dept <- "AFST"
  #opt$term <- "202380"
  #opt$college <- "AS"
  
  filtered_courses <- filter_DESRs(courses, opt)

  print <- filtered_courses %>% select (TERM,CRN,SUBJ,CRSE,level,SECT,CRSE_TITLE,INST_METHOD,PT,INST_NAME, `Academic Title`,ENROLLED,total_enrl,TOTAL_HOURS,FEE_CODE,FEE_DESC,FEE_TYPE, FEE_AMOUNT) 
  print <- print %>% arrange(TERM,SUBJ, CRSE_TITLE, SECT, FEE_AMOUNT)  %>%  distinct()
  
  # preserve total courses and fee courses
  fee_list_by_course <-print %>% group_by (TERM,SUBJ,CRSE_TITLE) %>% filter(FEE_AMOUNT > 0) %>% 
    reframe(sections=n(),students=sum(ENROLLED),fee_amt=FEE_AMOUNT,total_amt=fee_amt*students) %>%  
    distinct()

  fee_list_by_course %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)

  fee_list_by_dept <- fee_list_by_course %>% filter(fee_amt > 0) %>%  group_by (TERM,SUBJ) %>% reframe(courses=n(),students=sum(students),total_amt=sum(total_amt))
  #fee_list_by_dept %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)

  no_fee_list_by_course <-print %>% group_by (TERM,SUBJ,CRSE_TITLE) %>% reframe(sections=n(),students=sum(ENROLLED),fee_amt=FEE_AMOUNT,total_amt=(students*fee_amt)) %>%  distinct() 
  
  no_fee_depts <- no_fee_list_by_course %>% group_by (TERM,SUBJ) %>% summarise(courses=n(),students=sum(students),total_amt=sum(total_amt)) %>%  
    distinct() %>% filter(total_amt == 0)
  #no_fee_depts %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  fee_list_by_dept <- rbind(fee_list_by_dept, no_fee_depts)
  
  fee_list_by_dept$DEPT <- subj_to_dept_map[fee_list_by_dept$SUBJ]
  fee_list_by_dept$DEPT <-  ifelse(is.na(fee_list_by_dept$DEPT), fee_list_by_dept$SUBJ, fee_list_by_dept$DEPT)
  
  fee_list_by_dept <- fee_list_by_dept[, c(1,6,2,3,4,5)]
  fee_list_by_dept <- fee_list_by_dept %>% arrange (TERM,DEPT,SUBJ,total_amt) %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  # group and summarize
  fee_list_real_dept <- fee_list_by_dept %>%  group_by (TERM,DEPT) %>% summarize(courses=sum(courses),students=sum(students),total_amt=sum(total_amt))
  
  # print
  fee_list_real_dept %>% arrange (TERM,DEPT,total_amt) %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  # save for external use
  write.csv(fee_list_by_course, paste0(output_dir,"course-fees/fee_list_by_course.csv"), row.names=FALSE)
  write.csv(fee_list_by_dept, paste0(output_dir,"course-fees/fee_list_by_dept_subj.csv"), row.names=FALSE)
  write.csv(fee_list_real_dept, paste0(output_dir,"course-fees/fee_list_by_dept.csv"), row.names=FALSE)
}
