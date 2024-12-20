
# this function is course agnostic, expecting caller will merge with existing DF
calc_cl_enrls <- function(students,reg_status="registered") {
  
  message("calculating enrollments from class lists (calc_cl_enrls)...")
  
  # get distinct students (use CRN?)
  cl_enrls <- students %>%
    group_by(`Academic Period Code`, `SUBJ_CRSE`) %>% 
    distinct(`Student ID`, .keep_all = TRUE)
    
  # process reg_status
  if (is.character(reg_status)) {
    if (reg_status == "registered") {
      cl_enrls <- cl_enrls %>% filter (`Registration Status`== "Registered" | `Registration Status`== "Student Registered")
    }
    else if (reg_status == "dropped") {
      cl_enrls <- cl_enrls %>% filter (`Registration Status Code` %in% c("DR","DG","DW","DD"))
    }
  }
  else if (is.list(reg_status)) {
    cl_enrls <- cl_enrls %>% filter (`Registration Status Code` %in% reg_status)
  }
  
  # count students  
  cl_enrls <- cl_enrls %>% summarize(count = n(), .groups="keep")
  
  message("calc_cl_enrls returning ",nrow(cl_enrls)," rows.")
  
  return (cl_enrls)
}




# basic summary summarizes different sections into a single row
# maintains PT and INST_METHOD
agg_by_course_type <- function(courses,opt) {
  message("agg_by_course_type: basic course summary (summarize all course sections into single row):")
  summary <- courses %>% group_by(TERM,SUBJ,SUBJ_CRSE,CRSE_TITLE,PT,INST_METHOD,level,gen_ed_area) %>% 
    summarize(.groups="keep", sections=n(),avg_size=median(ENROLLED),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    select (TERM,SUBJ_CRSE,SUBJ,CRSE_TITLE,PT,INST_METHOD,enrolled,sections,avg_size,avail,waiting,level,gen_ed_area)
  
    #summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)

  return(summary)
}


# basic summary summarizes different sections and variants (method and pt) into a single row
# by default, compress AOP sections?
agg_by_course <- function(courses,opt) { 
  message("agg_by_course: basic course summary (summarize all types of course sections into single row):")
  summary <- courses %>% group_by(TERM,SUBJ,SUBJ_CRSE,CRSE_TITLE,level,gen_ed_area) %>% 
    summarize(.groups="keep", sections=n(),avg_size=mean(ENROLLED),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    select (TERM,SUBJ_CRSE,SUBJ,CRSE_TITLE,enrolled,sections,avg_size,avail,waiting,level,gen_ed_area)
  #summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(summary)
}


agg_by_course_term <- function(courses,opt) { 
  message("agg_by_course_term:")
  summary <- courses %>% group_by(SUBJ,SUBJ_CRSE,CRSE_TITLE,level,gen_ed_area) %>% 
    summarize(.groups="keep", sections=n(),avg_size=median(ENROLLED),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    select (SUBJ_CRSE,SUBJ,CRSE_TITLE,enrolled,sections,avg_size,avail,waiting,level,gen_ed_area)
  #summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(summary)
}


# agg_by_dept <- function(courses,opt) { 
#   message("agg_by_dept:")
#   summary <- courses %>% group_by(acad_year,TERM,DEPT,level) %>% 
#     summarize(sections=n(),avg_size=median(ENROLLED),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT))
#   #summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
#   
#   return(summary)
# }


agg_by_dept_level <- function(courses,opt) { 
  message("summarizing across DEPT and LEVEL:")
  summary <- courses %>% group_by(acad_year,TERM,DEPT,level) %>% 
    summarize(sections=n(),avg_size=median(ENROLLED),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    arrange(acad_year,DEPT,factor(level,levels=c("lower","upper","grad","total")),enrolled)
  
  summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(summary)
}


# basic summary for college
agg_by_college_level <- function(courses,opt) { 
  # group only by academic year to get college totals
  summary <- courses %>% group_by(TERM,`COLLEGE_DESC`,level) %>% 
    summarize(sections=n(),avg_size=median(ENROLLED),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    arrange(TERM,factor(level,levels=c("lower","upper","grad","total")),enrolled)
  
  summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(summary)
}



############# aggregate function (for enrollment summaries)
aggregate_courses <- function(courses,opt) {

  agg_by <- opt$aggregate
  
  if (agg_by == "course") {
    summary <- agg_by_course(courses,opt)
  }
  else if (agg_by == "course_type") {
    summary <- agg_by_course_type(courses,opt) 
  }
  else if (agg_by == "course_term") {
    summary <- agg_by_course_term(courses,opt) 
  }
  else if (agg_by == "dept") {
    summary <- agg_by_dept(courses,opt) 
  }
  else if (agg_by == "dept_level") {
    summary <- agg_by_dept_level(courses,opt) 
  }
  else if (agg_by == "college_level") {
    summary <- agg_by_college_level(courses,opt) 
  }
  else {
    message("not sure how to aggregate! aggregate param ",agg_by, " not found.")
  }  
  
  # return the summary DF
  message("done aggregating sections.")  
  return(summary)    

} # end aggregate_courses



get_enrl_for_dept_report <- function(courses, opt, d_params) {
  
  message("welcome to get_enrl_for_dept_report!")  
  
  myopt <- opt
  myopt$aggregate <- "course_term"
  myopt$x <- "compress"
  myopt$uel <- TRUE 
  
  #TODO: filter out AOP sections so it doesn't bring down averages?
  
  message("getting enrollment data via get_enrl...")
  summary_across_terms <- get_enrl(courses,myopt)  # filter, aggregate, etc
  
  # for inspection, rank by avg size across terms or total enrolled
  highest_total_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(enrolled)) %>% slice_head(n=10)    
  highest_mean_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(avg_size)) %>% slice_head(n=10)    
  
  highest_total_enrl_plot <- highest_total_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, enrolled)) %>%
    ggplot(aes(y=CRSE_TITLE, x=enrolled)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    ylab("Course") + xlab("Total Enrollment (since 2019)") 
  
  highest_mean_enrl_plot <- highest_mean_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, avg_size)) %>%
    ggplot(aes(y=CRSE_TITLE, x=avg_size)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    ylab("Course") + xlab("Mean Enrollment (since 2019)") 
  
  
  # histogram of avg class sizes
  highest_mean_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(avg_size))  
  
  highest_mean_histo_plot <- highest_mean_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, avg_size)) %>%
    ggplot(aes(x=avg_size)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_histogram(aes(fill=level),bins = 30) +
    scale_fill_brewer(palette=d_params$palette) +
    ylab("Number of courses") + xlab("# of students") 
  
  highest_mean_histo_plot <- ggplotly(highest_mean_histo_plot) %>% 
    layout(legend = list(orientation = 'h', x = 0.3, y = -.3),
           xaxis = list(standoff = -1))
  
  
  # load d_params w/ enrollment plots
  d_params$plots[["highest_total_enrl_plot"]] <- highest_total_enrl_plot
  d_params$plots[["highest_mean_enrl_plot"]] <- highest_mean_enrl_plot
  d_params$plots[["highest_mean_histo_plot"]] <- highest_mean_histo_plot
  
  return(d_params)
}



###################################
get_enrl <- function (courses,opt) {

  ########## for studio testing
  # opt <- list()
  # opt$college <- "AS"
  # opt$aggregate <- "course"
  # opt$course <- "ENGL 1110"
  
  message("\n","welcome to get_enrl!")
  #print(opt)
  
  # disable output by default, in case called from another function
  if (is.null(opt$csv)) {
    opt$csv <- FALSE
  }
  
  # default status should be A
  if (is.null(opt$status)) {
    opt$status <- "A"
  }
  
  # filter courses according to options
  courses <- filter_DESRs(courses, opt)
  courses <- courses %>% group_by(TERM,SUBJ,CRSE_TITLE,PT,INST_METHOD,level)

  # makes sure we have distinct courses after filtering
  courses <- courses %>%  distinct(TERM,CRN,.keep_all=TRUE)
  
  # add academic year field
  courses <- add_acad_year (courses, "TERM")
  
  ### AOP COMPRESSION
  message("figuring out AOP compression...")
  if (!is.null(opt$aop) && opt$aop == "compress") {
    courses <- compress_aop_pairs(courses,opt) # defined in misc_funcs
    
    #courses <- courses %>% select(TERM,CRN,SUBJ_CRSE,level,CRSE_TITLE,INST_METHOD,PT,INST_NAME,total_enrl,sect_enrl,pair_enrl)
    courses <- courses %>% select(TERM,CRN,SUBJ,SUBJ_CRSE,SECT,level,CRSE_TITLE,INST_METHOD,PT,INST_NAME,`Academic Title`,ENROLLED,total_enrl,XL_SUBJ,SEATS_AVAIL,WAIT_COUNT,sect_enrl,pair_enrl,gen_ed_area)
    
  }
  else {
    message("leaving AOP pairs alone...")
    
    # remove extraneous cols
    courses <- courses %>% select(TERM,CRN,SUBJ,SUBJ_CRSE,SECT,level,CRSE_TITLE,INST_METHOD,PT,INST_NAME,`Academic Title`,ENROLLED,total_enrl,XL_SUBJ,SEATS_AVAIL,WAIT_COUNT,gen_ed_area)
  }
  
  # courses get listed multiple times b/c of crosslisting (inc aop, but also general)
  # also, a course can also be listed multiple times depending on the lecture/recitation model (b/c of XL_CRSE column)
  #courses <- courses %>% ungroup() %>% subset(select = -c(XL_CRSE,XL_CRN,XL_CODE))
  
  # remove dupes since we have final columns
  courses <- courses %>% distinct() %>% 
    arrange(TERM,SUBJ_CRSE,CRSE_TITLE,INST_METHOD)
  
  if(!is.null(opt$aggregate)) {
    courses <- aggregate_courses(courses,opt)
  } 

  message("all done in get_enrl! returning data from get_enrl with ", nrow(courses) ," rows...\n")
  return(courses)
  
} # end get_enrl function

