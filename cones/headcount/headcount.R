# get counts of majors across Colleges to see change over time
# useful for trying to understanding relationship b/w changing majors and enrollments
count_majors <- function(opt) {
  acad_study <- load_academic_study()
  acad_study <- acad_study %>% distinct(`Academic Period`,ID, .keep_all = TRUE)
  acad_study <- acad_study %>% filter (`Student Level` == "Undergraduate")
  acad_study <- acad_study %>% group_by(`Academic Period`,Major)
  majors <- acad_study %>% summarize (count = n())
  
  return(majors)
}




# count_heads_in_college reports the number of majors and minors
# it also adds DEPT and PRGM field codes (via map_to_subj_codes) for easy filtering

# it counts pre-majors along with majors, since they are combined in the parser (but preserved with a 'pre' column)
# may be worth preserving that distinction here for reporting

# TODO: specify college as input param so A&S isn't locked in

count_heads_in_college <- function(opt) {

  headcount <- load_academic_study()

  # don't filter by College field, since it represents only First Major,
  # and would ignore students with Second Majors or minors in a different College.
  # instead, filter the Major (and similar) fields by names of majors in mappings.R
  message("filtering by A&S majors and minors...")
  college_programs <- as.list(names(major_to_program_map))
  
  # there are more efficient ways of doing this, but useful for future flexibility to be as explicit as possible
  headcount_first_majors <- headcount %>% group_by(term_code,`Academic Year`,`Student Level`,Degree,Major) %>% 
    filter (Major %in% college_programs) %>% 
    count(name="first_majors")
  
  headcount_second_majors <- headcount %>% group_by(term_code,`Academic Year`,`Student Level`,Degree,`Second Major`) %>% 
    filter (`Second Major` %in% college_programs) %>% 
    count(name="second_majors")
  
  headcount_majors <- merge(headcount_first_majors, headcount_second_majors,
                         by.x=c("term_code","Academic Year", "Student Level","Degree","Major"), 
                         by.y=c("term_code","Academic Year", "Student Level","Degree","Second Major"),
                         all=TRUE)
  # replace NAs with 0s
  headcount_majors <- headcount_majors %>% replace(is.na(.), 0)

  # combine first and second majors (since total majors is always almost more important)
  headcount_majors <- headcount_majors %>% mutate(majors=(first_majors + second_majors))
  
  
  # separate minors
  headcount_first_minors <- headcount %>% group_by(term_code,`Academic Year`,`Student Level`,Degree,`First Minor`) %>% 
    filter (`First Minor` %in% college_programs) %>% 
    count(name="first_minors")
  
  headcount_second_minors <- headcount %>% group_by(term_code,`Academic Year`,`Student Level`,Degree,`Second Minor`) %>% 
    filter (`Second Minor` %in% college_programs) %>% 
    count(name="second_minors")
  
  headcount_minors <- merge(headcount_first_minors, headcount_second_minors,
                            by.x=c("term_code","Academic Year", "Student Level","Degree","First Minor"), 
                            by.y=c("term_code","Academic Year", "Student Level","Degree","Second Minor"),
                            all=TRUE)
  
  # replace NAs with 0s
  headcount_minors <- headcount_minors %>% replace(is.na(.), 0)
  
  # combine first and second minors (since total minors is always almost more important)
  headcount_minors <- headcount_minors %>% mutate(minors=(first_minors + second_minors))
  
  # combine majors and minors into single DF
  headcount_all <- merge(headcount_majors, headcount_minors,
                         by.x=c("term_code","Academic Year", "Student Level","Degree","Major"), 
                         by.y=c("term_code","Academic Year", "Student Level","Degree","First Minor"),
                         all=TRUE)
  
  # replace NAs with 0s
  headcount_all <- headcount_all %>% replace(is.na(.), 0)
  
  # reduce fields to summary data
  headcount_all <- headcount_all %>% select(term_code,`Academic Year`,`Student Level`,Degree,Major,majors,minors)
    
  # restore _PRGM  and _DEPT from Major
  headcount_all$major_PRGM <- major_to_program_map[headcount_all$Major]
  headcount_all$major_DEPT <- prgm_to_dept_map[headcount_all$major_PRGM]
  headcount_all <- headcount_all %>% mutate (major_DEPT = ifelse(is.na(major_DEPT), major_PRGM, major_DEPT))
  
  headcount_filtered <- headcount_all
  headcount_filtered %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  
  # filter according to opt params
  if (!is.null(opt$dept)) {
    headcount_filtered <- headcount_all %>% filter(major_DEPT == opt$dept)
  }
  
  # program filtering overrides dept filtering
  if (!is.null(opt$prgm)) {
    headcount_filtered <- headcount_all %>% filter(major_PRGM == opt$prgm)
  }

  if (!is.null(opt$aggregate) && opt$aggregate == "level") {
    headcount_filtered <- headcount_filtered %>% group_by ( term_code, `Academic Year`, `Student Level`, major_PRGM,  major_DEPT) %>% 
      summarize (majors = sum(majors), minors = sum(minors)) %>% 
      arrange(`Student Level`, term_code)
  }
  
  if (!is.null(opt$term)) {
    headcount_filtered <- filter_by_term(headcount_filtered, opt$term, "term_code")
  }
  
  
  # print for inspection
  # message("headcount_filtered:")
  # headcount_filtered %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(headcount_filtered)
}



# this function is called when generating dept reports
# requires academic-study.feather, created by parsers/parse-academic-study.R 
# (report data comes from Academic Study Deatil Guided Adhoc)

# the parser adds a column of dept codes via map_to_subj_codes
# there is no output for this function; data gets put into d_params

# filters based on d_params$prog_names

get_headcount_data_for_dept_report <- function (d_params,opt=list()) {
  message("welcome to get_headcount_data_for_dept_report!")
  
  # studio testing
  # opt <- list()
  # opt$aggregate <- "level"
  # opt$term <- 202480
  #opt$dept <- "HIST"
  # d_params <- list()
  # d_params$term_start <- 201980
  # d_params$term_end <- 202460
  # d_params$prog_names <- "History"

  headcount_all <- count_heads_in_college(opt)

  headcount_filtered <- headcount_all %>% filter (term_code >= d_params$term_start & term_code <= d_params$term_end)
  headcount_filtered %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  message("filtering Major in ",d_params$prog_names )
  headcount_filtered <- headcount_filtered %>% filter (Major %in% d_params$prog_names)
  headcount_filtered %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  

  # retrofit
  headcount_all <- headcount_filtered
  
  ##########  UNDERGRADUATES ##########
  hc_progs_under <- headcount_all %>% filter (`Student Level` == "Undergraduate") %>% 
    group_by(`Academic Year`, `Student Level`, Degree) %>% arrange(`Academic Year`, `Student Level`, Degree) %>% 
    #summarise(majors = majors + sec_majors, minors = minors + sec_minors) %>% 
    arrange (`Academic Year`,desc(majors))
  
  message("adding hc_progs_under to d_params$tables")
  d_params$tables[["hc_progs_under"]] <- hc_progs_under
  hc_progs_under %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # plotting more flexible with a long version
  message("pivoting to long...")
  hc_progs_under_long <- hc_progs_under %>% pivot_longer(c("majors","minors"),names_to="type",values_to="students")
  hc_progs_under_long %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  hc_progs_under_long_majors <- hc_progs_under_long %>% filter (type=="majors" & students > 0)
  hc_progs_under_long_minors <- hc_progs_under_long %>% filter (type=="minors" & students > 0 ) 
  
  message("adding hc_progs_under_long_majors and minors to d_params$tables")
  d_params$tables[["hc_progs_under_long_majors"]] <- hc_progs_under_long_majors
  d_params$tables[["hc_progs_under_long_minors"]] <- hc_progs_under_long_minors
  
  
  ########## GRADUATE degree types ##########
  hc_progs_grad <- headcount_all %>% filter (`Student Level` == "Graduate/GASM") %>% 
    group_by(`Academic Year`, `Student Level`, Degree) %>% arrange(`Academic Year`, `Student Level`, Degree)  %>% 
    #summarise(majors = sum(majors +sec_majors), minors = sum(minors+sec_minors)) %>% 
    arrange (`Academic Year`,desc(majors))
  
  message("adding hc_progs_grad to d_params$tables")
  d_params$tables[["hc_progs_grad"]] <- hc_progs_grad
  
  # plotting more flexible with a long version
  hc_progs_grad_long <- hc_progs_grad %>% pivot_longer(c("majors","minors"),names_to="type",values_to="students")
  
  hc_progs_grad_long_majors <- hc_progs_grad_long %>% filter (type=="majors" & students > 0)
  hc_progs_grad_long_minors <- hc_progs_grad_long %>% filter (type=="minors" & students > 0 )
  
  d_params$tables[["hc_progs_grad_long_majors"]] <- hc_progs_grad_long_majors
  d_params$tables[["hc_progs_grad_long_minors"]] <- hc_progs_grad_long_minors
  
  
  
  # define function to create plots, since we there are several with slightly different filtering
  # requires p_params (plot paramters) and the usual d_params (dept params)
  # returns the d_params objects with the new plot in it.
  create_headcount_plot <- function(p_params,d_params) {
    
    data <- get(p_params$data)
    
    if (nrow(data) > 0) {
      plot <- data %>% 
        mutate(Degree = fct_reorder(Degree, students)) %>%
        arrange(desc(students)) %>% 
        slice_head(n=10) %>% 
        ggplot(aes(x=term_code,y=students)) + 
        ggtitle(paste(d_params$dept_name, ":",  paste(d_params$prog_codes, collapse=", "))) +  
        theme(legend.position="bottom") +
        guides(color = guide_legend(title = "")) +
        geom_bar(aes(fill=Degree),position="stack", stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        scale_fill_brewer(palette=d_params$palette) +
        xlab( p_params$x_lab) + ylab(  p_params$y_lab) 
      
      # create plotly version
      plot <- ggplotly(plot, width=) %>% 
        layout(legend = list(orientation = 'h', x = 0.3, y = -.3),
               xaxis = list(standoff = -1)
        )
    } else {plot <- "No data to plot..."}
    
    #hc_progs_under_long_majors_plot
    plot_name <- paste0(p_params$data,"_plot")
    
    message("adding plot to d_params$plots...")
    d_params$plots[[plot_name]] <- plot
    
    return (d_params)
  } # create_headcount_plot
  

  
  #################################
  #  CREATE PLOTS
  
  message("creating plots...")
  
  p_params <- list()
  p_params$x_lab <- "Term"
  p_params$y_lab <- "Undergraduate Students"
  p_params$data <- "hc_progs_under_long_majors" # should be string of DF name
  d_params <- create_headcount_plot(p_params,d_params)
  
  p_params$data <- "hc_progs_under_long_minors"
  d_params <- create_headcount_plot(p_params,d_params)
  
  p_params$y_lab <- "Graduate Students"
  p_params$data <- "hc_progs_grad_long_majors"
  d_params <- create_headcount_plot(p_params,d_params)
  
  p_params$data <- "hc_progs_grad_long_minors"
  d_params <- create_headcount_plot(p_params,d_params)
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}
