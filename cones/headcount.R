# get counts of majors across Colleges to see change over time
# useful for trying to understanding relationship b/w changing majors and enrollments
# TODO: auto count second majors?
count_majors <- function(opt) {
  acad_study <- load_academic_studies()
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
# requires mapping other majors and programs in other colleges

count_heads_in_college <- function(opt) {

  headcount <- load_academic_studies()

  # don't filter by College field, since it represents only First Major
  # and therefore ignore students with Second Majors or minors in a different College.
  # instead, filter the Major (and similar) fields by names of majors in mappings.R
  message("filtering by A&S majors and minors...")
  college_programs <- as.list(names(major_to_program_map))
  
  # what if headcount was long instead of wide (first major, etc gets listed as a type in sep col)
  headlong <- headcount %>% pivot_longer(c('Major','Second Major','First Minor','Second Minor'), names_to= "major_type", values_to="major_name")
  
  # reduce fields to summary data
  headlong_summary <- headlong %>% group_by(term_code,`Academic Year`,`Student Level`,major_type,major_name) %>% 
    summarize (students = n())
  
  # add _PRGM  and _DEPT from new major_name 
  headlong_summary$PRGM <- major_to_program_map[headlong_summary$major_name]
  headlong_summary$DEPT <- prgm_to_dept_map[headlong_summary$PRGM]
  #head_long <- headcount_all %>% mutate (major_DEPT = ifelse(is.na(major_DEPT), major_PRGM, major_DEPT))
  
  # TODO: remove rows with NAs in major_name?
  
  # headcount_filtered <- headlong_summary %>% filter(DEPT == "HIST")
  # headcount_filtered <- headcount_filtered %>% filter(`Student Level` == "Undergraduate")
  # 
  # plot <- headcount_filtered %>%
  #   arrange(desc(count)) %>%
  #   slice_head(n=10) %>%
  #   ggplot(aes(x=term_code,y=count)) +
  #   theme(legend.position="bottom") +
  #   guides(color = guide_legend(title = "")) +
  #   geom_bar(aes(fill=major_type),position="dodge", stat="identity") +
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  # 
  # plot


  # filter according to DEPT
  if (!is.null(opt$dept)) {
    headlong_summary <- headlong_summary %>% filter(DEPT == opt$dept)
  }
  
  # filter according to PRGM, which OVERRIDES dept filtering
  if (!is.null(opt$prgm)) {
    headlong_summary <- headlong_summary %>% filter(PRGM == opt$prgm)
  }

  # if (!is.null(opt$aggregate) && opt$aggregate == "level") {
  #   headcount_filtered <- headcount_filtered %>% group_by ( term_code, `Academic Year`, `Student Level`, major_PRGM,  major_DEPT) %>% 
  #     summarize (majors = sum(majors), minors = sum(minors)) %>% 
  #     arrange(`Student Level`, term_code)
  # }
  
  if (!is.null(opt$term)) {
    headlong_summary <- filter_by_term(headlong_summary, opt$term, "term_code")
  }
  
  return(headlong_summary)
}



# this function is called when generating dept reports
# requires academic_study.Rds (from Academic Study Detail Guided Adhoc)

# the parser adds a column of dept codes via map_to_subj_codes
# there is no output for this function; data gets put into d_params

# filters based on d_params$prog_names

get_headcount_data_for_dept_report <- function (d_params,opt=list()) {
  message("welcome to get_headcount_data_for_dept_report!")
  
  # studio testing
  # opt <- list()
  # opt$term <- 202480
  # opt$dept <- "ANTH"
  # d_params <- list()
  # d_params$term_start <- 202480
  # d_params$term_end <- 202480
  # d_params$prog_names <- c("Anthropology","Forensic Science","Forensic Anthropology")

  
  # define function to create plots, since we there are several with slightly different filtering
  # requires p_params (plot paramters) and the usual d_params (dept params)
  # returns the d_params objects with the new plot in it.
  create_headcount_plot <- function(p_params,d_params) {
    
    data <- get(p_params$data)
    
    if (nrow(data) > 0) {
      plot <- data %>% 
        ggplot(aes(x=term_code,y=students)) +
        theme(legend.position="bottom") +
        guides(color = guide_legend(title = "")) +
        geom_bar(aes(fill=major_name),position="stack", stat="identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      
        
      # create plotly version
      plot <- ggplotly(plot, width=) %>% 
        layout(legend = list(orientation = 'h', x = 0.3, y = -.3),
               xaxis = list(standoff = -1)
        )
    } else {plot <- "No data to plot..."}
    
    plot
    plot_name <- paste0(p_params$data,"_plot")
    
    message("adding plot to d_params$plots...")
    d_params$plots[[plot_name]] <- plot
    
    return (d_params)
  } # create_headcount_plot
  
  
  # conduct headcount
  headcount <- count_heads_in_college(opt)

  # filter by term_start and term_end
  headcount_filtered <- headcount %>% filter (term_code >= d_params$term_start & term_code <= d_params$term_end)

  # filter by supplied program names (as determined by opt dept)
  message("filtering Major in ",d_params$prog_names )
  headcount_filtered <- headcount_filtered %>% filter (major_name %in% d_params$prog_names)


  major_names <- c("Major","Second Major")
  minor_names <- c("First Minor","Second Minor")
  
  ##########  UNDERGRADUATES ##########
  message("filtering for undergrads...")
  
  hc_progs_under <- headcount_filtered %>% filter (`Student Level` == "Undergraduate") 
  d_params$tables[["hc_progs_under"]] <- hc_progs_under

  hc_progs_under_long_majors <- hc_progs_under %>% filter (major_type %in% major_names & students > 0)
  d_params$tables[["hc_progs_under_long_majors"]] <- hc_progs_under_long_majors
  
  hc_progs_under_long_minors <- hc_progs_under %>% filter (major_type %in% minor_names & students > 0 ) 
  d_params$tables[["hc_progs_under_long_minors"]] <- hc_progs_under_long_minors
  
  

  ########## GRADUATE degree types ##########
  message("filtering for grads...")
  hc_progs_grad <- headcount_filtered %>% filter (`Student Level` == "Graduate/GASM")
  d_params$tables[["hc_progs_grad"]] <- hc_progs_grad
  
  hc_progs_grad_long_majors <- hc_progs_grad %>% filter (major_type %in% major_names & students > 0)
  d_params$tables[["hc_progs_grad_long_majors"]] <- hc_progs_grad_long_majors
  
  hc_progs_grad_long_minors <- hc_progs_grad %>% filter (major_type %in% minor_names & students > 0 )
  d_params$tables[["hc_progs_grad_long_minors"]] <- hc_progs_grad_long_minors
  
  
  
  #################################
  #  CREATE PLOTS
  
  message("creating plots via p_params and adding to d_params...")
  
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
  
  d_params$plots[["hc_progs_under_long_minors"]]
  
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}
