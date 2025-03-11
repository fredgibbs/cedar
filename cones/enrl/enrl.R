
# this function is course agnostic, expecting caller will merge with existing DF
# return a row for each course in students list, 
# with a count and mean for each registration status code

calc_cl_enrls <- function(students,reg_status=NULL) {
  
  # students <- load_students()
  # #reg_status <- c("DR")
  # reg_status <- NULL
  
  reg_stats_summary <- tibble()
  
  message("calculating enrollments from class lists (calc_cl_enrls)...")
  
  # get distinct rows within courses
  # using SUBJ_CRSE to lump all sections topics courses together
  cl_enrls <- students %>%
    group_by(`Course Campus Code`,`Course College Code`,`Academic Period Code`, `SUBJ_CRSE`) %>% 
    distinct(`Student ID`, .keep_all = TRUE)
  
  # count students in each term by reg status code
  cl_enrls <- cl_enrls %>% group_by (`Course Campus Code`,`Course College Code`,SUBJ_CRSE,`Registration Status Code`,`Academic Period Code`, term_type) %>% 
    summarize(count = n(), .groups="keep") 
  
  # calc mean reg codes per course and term type
  cl_enrls <- cl_enrls %>% group_by (`Course Campus Code`,`Course College Code`,SUBJ_CRSE,term_type,`Registration Status Code`) %>% 
    mutate(mean = round(mean(count),digits=1))
  
  
  if (is.null(reg_status)) {
    
    cl_enrls <- cl_enrls %>% group_by(`Course Campus Code`,`Course College Code`,SUBJ_CRSE,`Academic Period Code`, term_type)
    
    reg_stats_summary <- cl_enrls %>% filter(`Registration Status Code`== "RE" | `Registration Status Code`== "RS")  %>%
      summarize(registered = sum(count), .groups="keep")
    
    de <- cl_enrls %>% filter (`Registration Status Code` %in% c("DR")) %>%
      summarize(dr_early = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, de, all=T)
    
    
    dl <- cl_enrls %>% filter (`Registration Status Code` %in% c("DG","DW","DD")) %>%
      summarize(dr_late = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, dl, all=T)
    
    
    da <- cl_enrls %>% filter (`Registration Status Code` %in% c("DR","DG","DW","DD")) %>%
      summarize(dr_all = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, da, all=T)
    

    cl_total <- cl_enrls %>% 
      summarize(cl_total = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, cl_total, all=T)
    
        
    # remove NAs from merging
    reg_stats_summary[is.na(reg_stats_summary)] <- 0
    
    
    # regroup without APC to calc means
    reg_stats_summary <- reg_stats_summary %>% group_by(`Course Campus Code`,`Course College Code`,SUBJ_CRSE, term_type)
    
    # get means across term_types
    reg_stats_summary <- reg_stats_summary %>% mutate(de_mean = round(mean(dr_early),digits=2))
    reg_stats_summary <- reg_stats_summary %>% mutate(dl_mean = round(mean(dr_late),digits=2))
    reg_stats_summary <- reg_stats_summary %>% mutate(da_mean = round(mean(dr_all),digits=2))
    reg_stats_summary <- reg_stats_summary %>% mutate(cl_mean = round(mean(cl_total),digits=2))
    reg_stats_summary <- reg_stats_summary %>% mutate(reg_mean = round(mean(registered),digits=2))
    
  }
  # if given list of reg codes, filter for those
  else if (!is.null(reg_status) && is.list(reg_status)) {
    reg_stats_summary <- cl_enrls %>% filter (`Registration Status Code` %in% reg_status)
  }
  
  message("calc_cl_enrls returning ",nrow(reg_stats_summary)," rows.")
  
  return (reg_stats_summary)
}

  
compress_aop_pairs <- function (courses,opt) {
  message("compressing AOP courses into single row...")
  
  # for testing...
  # courses <- load_courses()
  # opt <- list()
  # #opt[["course"]] <- "BIOL 2305"
  # opt[["term"]] <- "202210"
  # courses <-  filter_DESRs(courses,opt)
  
  # for clarity, combine aop and twin courses into single entry
  # test to see if we're filtering by dept
  courses <- courses %>%  group_by(TERM, XL_CODE)
  
  # get just AOP courses
  courses_aop <- courses %>% filter (INST_METHOD == "MOPS")
  
  # AOP sections don't necessarily have a partner, so remove those without one
  # TODO: handle case of AOP course having partner, but not being crosslisted
  # might be able to check on course title
  courses_aop <- courses_aop %>% filter (XL_CODE != "0")
  
  # get pairs of aop and twin section
  aop_pairs <- courses_aop %>% filter (XL_CODE %in% courses_aop$XL_CODE) %>% 
    distinct(CRN, .keep_all = TRUE) %>% 
    group_by(TERM,XL_CODE)
  
  # to collapse the aop and online section into one row, get each section's enrollment
  aop_pairs <- aop_pairs %>% mutate (sect_enrl = ENROLLED, pair_enrl = total_enrl - ENROLLED)
  
  # arrange by inst_method, and take first row of group
  aop_single <- aop_pairs %>% arrange(INST_METHOD) %>% filter (row_number() == 1)
  # message("aop sections:")
  # print(aop_single)
  
  # since compressing two sections into one, change ENROLLED to mimic total_enrl
  # otherwise, compressing effectively deletes the non-aop section enrollment
  aop_single <- aop_single %>% mutate (ENROLLED = total_enrl)
  
  # remove all pairs from orig course list
  courses <- courses %>% filter (!(XL_CODE %in% courses_aop$XL_CODE)) %>% distinct(CRN, .keep_all = TRUE) %>% 
    group_by(TERM,XL_CODE)
  
  # add all single rows
  courses <- rbind(courses,aop_single)
  
  message("returning compressed aop rows...")
  
  return(courses)
} # end compress_aop_pairs




# generic summary function based on group_cols
# replaces the many variants of aggregate
summarize_courses <- function (courses, opt) {
  message("summarizing courses with group_cols...")
  
  # set default group_cols
  if (is.null(opt[["group_cols"]])) {
    group_cols <- c("CAMP","COLLEGE","TERM", "term_type", "SUBJ","SUBJ_CRSE","CRSE_TITLE","level","gen_ed_area")
  }
  else {
    group_cols <- opt[["group_cols"]]
    group_cols <- convert_param_to_list(group_cols)
    print(str(group_cols))
    group_cols <- as.character(group_cols)
    #group_cols <- c("SUBJ_CRSE","PT")
    print(str(group_cols))
  }
  
  summary <- courses %>% ungroup() %>% group_by_at(group_cols) %>% 
    summarize(.groups="keep", sections=n(),avg_size=round(mean(ENROLLED),digits=1),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT))
  
  return(summary)
}


# basic summary summarizes different sections into a single row

# maintains PT and INST_METHOD
agg_by_course_type <- function(courses,opt) {
  message("agg_by_course_type: basic course summary (summarize all course sections into single row):")
  summary <- courses %>% group_by(TERM,SUBJ,SUBJ_CRSE,CRSE_TITLE,PT,INST_METHOD,level,gen_ed_area) %>% 
    summarize(.groups="keep", sections=n(),avg_size=round(mean(ENROLLED),digits=1),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    select (TERM,SUBJ_CRSE,SUBJ,CRSE_TITLE,PT,INST_METHOD,enrolled,sections,avg_size,avail,waiting,level,gen_ed_area)
  
    #summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)

  return(summary)
}


# only INST_METHOD
agg_by_method <- function(courses,opt) {
  summary <- courses %>% group_by(TERM,SUBJ,INST_METHOD,level,gen_ed_area) %>% 
    summarize(.groups="keep", sections=n(),avg_size=round(mean(ENROLLED),digits=1),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    select (TERM,SUBJ,INST_METHOD,enrolled,sections,avg_size,avail,waiting,level,gen_ed_area)
  
  #summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(summary)
}


# basic summary summarizes different sections and variants (method and pt) into a single row
# by default, compress AOP sections?
agg_by_course <- function(courses,opt) { 
  message("agg_by_course: basic course summary (summarize all types of course sections into single row):")
  summary <- courses %>% group_by(TERM,SUBJ,SUBJ_CRSE,CRSE_TITLE,level,gen_ed_area) %>% 
    summarize(.groups="keep", sections=n(),avg_size=round(mean(ENROLLED),digits=1),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    select (TERM,SUBJ_CRSE,SUBJ,CRSE_TITLE,enrolled,sections,avg_size,avail,waiting,level,gen_ed_area)
  #summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(summary)
}

agg_by_subj_crse <- function(courses,opt) { 
  message("agg_by_subj_course: summarize all types of course sections into single row based on SUBJ_CRSE (not course title)):")
  summary <- courses %>% group_by(TERM,SUBJ,SUBJ_CRSE,level,gen_ed_area) %>% 
    summarize(.groups="keep",sections=n(),avg_size=round(mean(ENROLLED),digits=1),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT))
    
  return(summary)
}


agg_by_course_term <- function(courses,opt) { 
  message("agg_by_course_term:")
  summary <- courses %>% group_by(SUBJ,SUBJ_CRSE,CRSE_TITLE,level,gen_ed_area) %>% 
    summarize(.groups="keep", sections=n(),avg_size=round(mean(ENROLLED),digits=1),enrolled=sum(ENROLLED),avail=sum(SEATS_AVAIL),waiting=sum(WAIT_COUNT)) %>% 
    select (SUBJ_CRSE,SUBJ,CRSE_TITLE,enrolled,sections,avg_size,avail,waiting,level,gen_ed_area)
  #summary %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  return(summary)
}


############# aggregate function (for enrollment summaries)
aggregate_courses <- function(courses, opt) {

  agg_by <- opt$aggregate
  
  if (!is.null(opt[["group_cols"]])) {
    message("group_cols is not null...")
    summary <- summarize_courses(courses,opt)
  }
  else if (agg_by == "course") {
    stop("course is not implemented yet!")
    summary <- agg_by_course(courses,opt)
  }
  else if (agg_by == "course_type") {
    stop("course_type is not implemented yet!")
    summary <- agg_by_course_type(courses,opt)
  }
  else if (agg_by == "subj_crse") {
    stop("subj_crse is not implemented yet!")
    summary <- agg_by_subj_crse(courses,opt)
  }
  else if (agg_by == "course_term") {
    stop("course_term is not implemented yet!")
    summary <- agg_by_course_term(courses,opt) 
  }
  else {
    stop("not sure how to aggregate! aggregate param ",agg_by, " not found.")
  }  
  
  # return the summary DF
  message("done aggregating sections.")  
  return(summary)    

} # end aggregate_courses



get_enrl_for_dept_report <- function(courses, d_params) {
  
  message("welcome to get_enrl_for_dept_report!")  
  
  myopt <- list()
  myopt$dept <- d_params[["dept_code"]]
  myopt$group_by <- c("SUBJ","SUBJ_CRSE","CRSE_TITLE","level","gen_ed_area")
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
get_enrl <- function (courses,opt,group_cols=NULL) {

  ########## for studio testing
  # opt <- list()
  # opt$college <- "AS"
  # opt$aggregate <- "course"
  # opt$course <- "ENGL 1110"
  
  message("\n","welcome to get_enrl!")
  
  # default status to A for active courses
  if (is.null(opt$status)) {
    opt$status <- "A"
  }

  # default to use exclude list
  if (is.null(opt$uel)) {
    opt$uel <- TRUE
  }
  
  # default to use ABQ campus (EA is ABQ online courses)
  if (is.null(opt$campus)) {
    #opt$campus <- c("ABQ","EA")
  }
  
  # filter courses according to options
  courses <- filter_DESRs(courses, opt)

  # add academic year field
  # TODO: move into load_courses or parser?
  courses <- add_acad_year (courses, "TERM")
  
  select_cols <- c("CAMP","COLLEGE","TERM","term_type","CRN","SUBJ","SUBJ_CRSE","SECT","level","CRSE_TITLE","INST_METHOD","PT","INST_NAME","ENROLLED","total_enrl","XL_SUBJ","SEATS_AVAIL","WAIT_COUNT","gen_ed_area")
  
  ### AOP COMPRESSION
  if (!is.null(opt$aop) && opt$aop == "compress") {
    message("compressing AOP pairs...")
    courses <- compress_aop_pairs(courses,opt) 
    select_cols <- c(select_cols, "sect_enrl","pair_enrl")
    courses <- courses %>% select(all_of(select_cols))
  }
  else {
    message("leaving AOP pairs alone...")
    courses <- courses %>% select(all_of(select_cols))
  }
  
  # courses get listed multiple times b/c of crosslisting (inc aop, but also general)
  # also, a course can also be listed multiple times depending on the lecture/recitation model (b/c of XL_CRSE column)
  #courses <- courses %>% ungroup() %>% subset(select = -c(XL_CRSE,XL_CRN,XL_CODE))
  
  # remove dupes since we have final columns
  courses <- courses %>% distinct() %>% 
    arrange(TERM,SUBJ_CRSE,CRSE_TITLE,INST_METHOD)
  
  # check if aggregating
  if(!is.null(opt$aggregate) || !is.null(opt$group_cols)) {
    courses <- aggregate_courses(courses, opt)
  } else {
    message("no aggregating!")
  }

  message("all done in get_enrl! returning data from get_enrl with ", nrow(courses) ," rows...\n")
  return(courses)
  
} # end get_enrl function

