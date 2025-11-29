## Count students by selected program(s) using opt parameter structure
#'
#' @param df data.frame of academic studies
#' @param opt list containing filter options (campus, college, dept, major, minor, concentration)
#' @return data.frame of matching students (filtered and summarized)
count_heads_by_program <- function(df, opt = list()) {
  
  # df <- academic_studies
  # opt <- list()
  # opt[["dept"]] <- "History"
  #opt[["college"]] <- "College of Arts and Sciences"
  #opt[["campus"]] <- "Albuquerque/Main"

  message("[headcount.R] Welcome to count_heads_by_program!")
  message("[headcount.R] opt contents: ", paste(names(opt), collapse = ", "))

  important_cols <- c("ID", "term_code", "Actual College", "Translated College", "Student Campus", "Student Level", "Degree",
                      "Department", "Major", "Second Major",
                      "First Minor", "Second Minor",
                      "First Concentration", "Second Concentration")
  message("[headcount.R] Selecting important columns: ", paste(important_cols, collapse=", "))
  
  # Select columns that exist in the data
  available_cols <- important_cols[important_cols %in% colnames(df)]
  df <- df %>% select(all_of(available_cols)) %>% distinct()
  message("[headcount.R] Data shape after select/distinct: ", nrow(df), " rows, ", ncol(df), " cols")

  # Apply filters from opt parameter following CEDAR patterns
  if (!is.null(opt$campus) && length(opt$campus) > 0) {
    message("[headcount.R] Filtering by campus: ", paste(opt$campus, collapse = ", "))
    df <- df %>% filter(`Student Campus` %in% opt$campus)
  }
  
  if (!is.null(opt$college) && length(opt$college) > 0) {
    message("[headcount.R] Filtering by college: ", paste(opt$college, collapse = ", "))
    df <- df %>% filter(`Translated College` %in% opt$college)
  }
  
  if (!is.null(opt$dept) && length(opt$dept) > 0) {
    message("[headcount.R] Filtering by department: ", paste(opt$dept, collapse = ", "))
    df <- df %>% filter(Department %in% opt$dept)
  }

  # Transform to long format for program analysis
  long_df <- df %>%
    pivot_longer(
      cols = c("Major", "Second Major",
               "First Minor", "Second Minor",
               "First Concentration", "Second Concentration"),
      names_to = "program_type",
      values_to = "program_value"
    )
  message("[headcount.R] Data shape after pivot_longer: ", nrow(long_df), " rows")

  # Create filter expression for program selections
  filter_expr <- rep(FALSE, nrow(long_df))

  # Apply program-specific filters from opt
  if (!is.null(opt$major) && length(opt$major) > 0) {
    message("[headcount.R] Filtering by major: ", paste(opt$major, collapse = ", "))
    filter_expr <- filter_expr | (long_df$program_type %in% c("Major", "Second Major") & long_df$program_value %in% opt$major)
  }
  
  if (!is.null(opt$minor) && length(opt$minor) > 0) {
    message("[headcount.R] Filtering by minor: ", paste(opt$minor, collapse = ", "))
    filter_expr <- filter_expr | (long_df$program_type %in% c("First Minor", "Second Minor") & long_df$program_value %in% opt$minor)
  }
  
  if (!is.null(opt$concentration) && length(opt$concentration) > 0) {
    message("[headcount.R] Filtering by concentration: ", paste(opt$concentration, collapse = ", "))
    filter_expr <- filter_expr | (long_df$program_type %in% c("First Concentration", "Second Concentration") & long_df$program_value %in% opt$concentration)
  }

  # If no program filters specified, include all programs (but filtered by campus/college/dept)
  if (is.null(opt$major) && is.null(opt$minor) && is.null(opt$concentration)) {
    message("[headcount.R] No program filters specified - including all programs.")
    no_program <- TRUE
    filter_expr <- rep(TRUE, nrow(long_df))
  } else {
    no_program <- FALSE
  }
  
  filtered <- long_df[filter_expr, , drop=FALSE]
  message("[headcount.R] Data shape after filtering for selected values: ", nrow(filtered), " rows")

  
  # Summarize without program grouping
  if (no_program) {
    summarized <- filtered %>%
      filter(!is.na(program_value) & program_value != "") %>%
      group_by(`term_code`, `Student Level`, program_type) %>%
      summarize(student_count = n_distinct(ID), .groups = "drop") %>%
      arrange(`Student Level`, program_type, term_code)
  } 
  else {
    # Summarize by program
    summarized <- filtered %>%
      filter(!is.na(program_value) & program_value != "") %>%
      group_by(`term_code`, `Student Level`, program_type, program_value) %>%
      summarize(student_count = n_distinct(ID), .groups = "drop") %>%
      arrange(`Student Level`, program_type, term_code)
  }
    
  message("[headcount.R] Data shape after summarizing: ", nrow(summarized), " rows.")
  message("[headcount.R] Sample data:")
  print(head(summarized, 10))
  
# Return named list following CEDAR patterns
  result <- list(
    data = summarized,
    no_program_filter = no_program,
    metadata = list(
      total_students = n_distinct(df$ID),
      programs_included = if (no_program) "all" else c(opt$major, opt$minor, opt$concentration),
      filters_applied = names(opt)[!sapply(opt, is.null)]
    )
  )

  return(result)
}



#' Create separate headcount plots for Undergraduates and Graduates
#' @param summarized The summarized table from count_heads_by_program
#' @return A named list with plotly plots: $undergrad and $graduate
make_headcount_plots_by_level <- function(result) {

  message("[headcount.R] Welcome to make_headcount_plots_by_level!")

  # Extract data and flag from result
  summarized <- result$data
  no_program <- result$no_program_filter
    
  # Default empty list of plots
  plots <- list()

  # Define custom ordering for program types following CEDAR academic hierarchy
  program_type_order <- c("Major", "Second Major", "First Minor", "Second Minor", 
                         "First Concentration", "Second Concentration")
  
  # Convert program_type to ordered factor
  summarized$program_type <- factor(summarized$program_type, 
                                   levels = program_type_order, 
                                   ordered = TRUE)
  
  # For program values, create alphabetical ordering within types
  if (!no_program && "program_value" %in% colnames(summarized)) {
    # Order program values alphabetically for consistent display
    program_value_order <- sort(unique(summarized$program_value))
    summarized$program_value <- factor(summarized$program_value, 
                                      levels = program_value_order, 
                                      ordered = TRUE)
  }
  
  # Undergraduate plot 
  message("[headcount.R] Creating Undergraduate plot...")
  undergrad_data <- summarized[summarized$`Student Level` == "Undergraduate", ]
  if (nrow(undergrad_data) > 0) {
  
    if (!no_program) {
      
      # Create the plot, faceting by program_value
      p_undergrad <- ggplot(undergrad_data, aes(x = term_code, y = student_count, fill = program_type)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~program_value, scales = "fixed", ncol = 2) +
        labs(title = "Undergraduate Headcount by Program", x = "Term", y = "Student Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(palette = "Set2", name = "Program Type")
    } 
    else {
      # Create the plot, faceting by program_type
      p_undergrad <- ggplot(undergrad_data, aes(x = term_code, y = student_count, fill = program_type)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~program_type, scales = "fixed", ncol = 2) +
        labs(title = "Undergraduate Headcount", x = "Term", y = "Student Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(palette = "Set2", name = "Program Type")
    }
    
    # Convert to interactive plotly plot
    plots$undergrad <- ggplotly(p_undergrad) %>%
      layout(legend = list(orientation = 'h', x = 0.3, y = -0.3))
    
  } else { # no undergrad data
    plots$undergrad <- NULL
  }
  
  # plots$undergrad
  # p_undergrad
  
  
  # Graduate plot
  message("[headcount.R] Creating Graduate plot...")
  grad_data <- summarized[summarized$`Student Level` == "Graduate/GASM", ]
  
  if (nrow(grad_data) > 0) {

    grad_data$alpha_value <- ifelse(grad_data$program_type == "Doctor of Philosophy", 1.0, 0.6)

    if (!no_program) {
      p_grad <- ggplot(grad_data, aes(x = term_code, y = student_count, fill = program_value, alpha = I(alpha_value))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Graduate Headcount", x = "Term", y = "Student Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    else {
      p_grad <- ggplot(grad_data, aes(x = term_code, y = student_count, fill = program_type, alpha = I(alpha_value))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Graduate Headcount", x = "Term", y = "Student Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  
    }
    
    # convert to interactive plotly plot
    plots$graduate <- ggplotly(p_grad) %>% layout(legend = list(orientation = 'h', x = 0.3, y = -.3))
  } else {
    plots$graduate <- NULL
  }

  message("[headcount.R] Returning plots list with ", length(plots), " plots.")
  return(plots)
}



make_headcount_plots <- function(summarized) {
    message("[headcount.R] Creating plot for data with ", nrow(summarized), " rows...")

    if (nrow(summarized) > 0) {
      message("[headcount.R] Creating ggplot...")
      plot <- summarized %>%
        ggplot(aes(x=term_code,y=student_count)) +
        theme(legend.position="bottom") +
        guides(color = guide_legend(title = "")) +
        geom_bar(aes(fill=program_type),position="stack", stat="identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

      message("[headcount.R] Creating plotly plot...")
      plot <- ggplotly(plot) %>% 
        layout(legend = list(orientation = 'h', x = 0.3, y = -.3),
               xaxis = list(standoff = -1)
        )      
    } else {
      plot <- NULL
      message("[headcount.R] No data to plot.")
    }
    
    return (plot)
  } # create_headcount_plot
  



# count_heads_by_program <- function(academic_studies_data, opt) {
#   message("[headercount.R] Welcome to count_heads_by_program!")
#   acad_study <- academic_studies_data
#   acad_study <- acad_study %>% distinct(`Academic Period`, ID, .keep_all = TRUE)
 
#   # am i going to want a group_by dropdown in shiny?

#   # filter by major, minor, or concentration
#   if (!is.null(opt$major)) {
#     acad_study <- acad_study %>% filter(Major %in% opt$major)
#   }
#   if (!is.null(opt$minor)) {
#     acad_study <- acad_study %>% filter(`First Minor`` %in% opt$minor)
#   }
#   if (!is.null(opt$concentration)) {
#     acad_study <- acad_study %>% filter(`First Concentration` %in% opt$concentration)
#   }

#   #acad_study <- acad_study %>% filter (`Student Level` == "Undergraduate")
#   acad_study <- acad_study %>% group_by(`Academic Period`,College)
#   counts <- acad_study %>% summarize (count = n())

#   message("[headercount.R] About to return number of heads by program...")
#   return(counts)
# }




# get counts of majors across Colleges to see change over time
# useful for trying to understanding relationship b/w changing majors and enrollments
# TODO: auto count second majors?
count_majors <- function(academic_studies_data, opt) {
  message("[headercount.R] Welcome to count_majors!")
  acad_study <- academic_studies_data
  acad_study <- acad_study %>% distinct(`Academic Period`, ID, .keep_all = TRUE)
  #acad_study <- acad_study %>% filter (`Student Level` == "Undergraduate")
  acad_study <- acad_study %>% group_by(`Academic Period`,Major)
  majors <- acad_study %>% summarize (count = n())

  message("[headercount.R] About to return number of majors...")
  return(majors)
}


# count_heads reports the number of majors and minors in given data
# it adds DEPT and PRGM field codes (via map_to_subj_codes) for easy filtering

# it counts pre-majors along with majors, since they are combined in data_parser (but preserved with a 'pre' column)
# may be worth preserving that distinction here for reporting

count_heads <- function(academic_studies_data, opt) {
  message("[headercount.R] Welcome to count_heads!")

  important_cols <- c("ID", "term_code", "Actual College", "Translated College", "Student Campus", "Student Level", "Degree",
                      "Major", "Second Major",
                      "First Minor", "Second Minor",
                      "First Concentration", "Second Concentration", "Third Concentration")
  message("[headcount.R] Selecting important columns: ", paste(important_cols, collapse=", "))

  as_selected <- academic_studies_data %>% select(all_of(important_cols)) %>% distinct()
  message("[headcount.R] Data shape after select/distinct: ", nrow(as_selected), " rows, ", ncol(as_selected), " cols")


  # what if headcount was long instead of wide (first major, etc gets listed as a type in sep col)
  message("[headcount.R] Pivoting academic_studies_data to long format...")
  headlong <- as_selected %>% pivot_longer(c('Major','Second Major','First Minor','Second Minor'), names_to= "major_type", values_to="major_name")
  
  # reduce fields to summary data
  headlong_summary <- headlong %>% group_by(term_code,`Student Level`,major_type,major_name) %>% 
    summarize (students = n(), .groups = "drop")
  
  # add _PRGM and _DEPT from new major_name
  message("[headcount.R] Mapping major_name to PRGM...")
  headlong_summary$PRGM <- major_to_program_map[headlong_summary$major_name]
  
  message("[headcount.R] Mapping PRGM to DEPT...")
  headlong_summary$DEPT <- prgm_to_dept_map[headlong_summary$PRGM]
  
  # don't filter by College field, since it represents only First Major
  # and therefore ignore students with Second Majors or minors in a different College.

  # TODO: remove filtering here and expect calling function to handle
  
  # filter according to DEPT
  if (!is.null(opt$dept)) {
    message("[headcount.R] Filtering by DEPT: ", opt$dept)
    headlong_summary <- headlong_summary %>% filter(DEPT == opt$dept)
  }
  
  # filter according to PRGM, which OVERRIDES dept filtering
  if (!is.null(opt$prgm)) {
    message("[headcount.R] Filtering by PRGM: ", opt$prgm)
    headlong_summary <- headlong_summary %>% filter(PRGM == opt$prgm)
  }
  
  # filter account to term_code
  if (!is.null(opt$term)) {
    message("[headcount.R] Filtering by term_code <= ", opt$term)
    headlong_summary <- filter_by_term(headlong_summary, opt$term, "term_code")
  }
  
  message("[headcount.R] About to return headlong_summary...")
  return(headlong_summary)
}



# this function is called when generating dept reports
# requires academic_study.Rds (from Academic Study Detail Guided Adhoc)

# the parser adds a column of dept codes via map_to_subj_codes
# there is no output for this function; data gets put into d_params

# filters based on d_params$prog_names

get_headcount_data_for_dept_report <- function (academic_studies_data, d_params, opt=list()) {
  message("[headercount.R] Welcome to get_headcount_data_for_dept_report!")

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
  message("[headcount.R] Defining plot function...")
  
  create_headcount_plot <- function(p_params, d_params) {
    message("[headcount.R] Creating plot for data: ", p_params$data)
    data <- get(p_params$data)
    message("[headcount.R] Got data with ", nrow(data), " rows")

    if (nrow(data) > 0) {
      message("[headcount.R] Creating ggplot...")
      plot <- data %>% 
        ggplot(aes(x=term_code,y=students)) +
        theme(legend.position="bottom") +
        guides(color = guide_legend(title = "")) +
        geom_bar(aes(fill=major_type),position="stack", stat="identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

      message("[headcount.R] Creating plotly plot...")
      plot <- ggplotly(plot) %>% 
        layout(legend = list(orientation = 'h', x = 0.3, y = -.3),
               xaxis = list(standoff = -1)
        )      
    } else {
      plot <- NULL
      message("[headcount.R] No data to plot")
    }
    
    plot_name <- paste0(p_params$data,"_plot")
    message("[headcount.R] Adding plot ", plot_name, " to d_params...")

    message("[headcount.R] Adding plot to d_params$plots...")
    d_params$plots[[plot_name]] <- plot
    
    return (d_params)
  } # create_headcount_plot
  
  
  # conduct headcount - pass the data instead of loading it
  message("[headcount.R] Counting heads in college...")
  headcount <- count_heads(academic_studies_data, opt)

  # filter by term_start and term_end
  headcount_filtered <- headcount %>% filter (term_code >= d_params$term_start & term_code <= d_params$term_end)

  # filter by supplied program names (as determined by opt dept)
  message("[headcount.R] Filtering Major in ",d_params$prog_names )
  headcount_filtered <- headcount_filtered %>% filter (major_name %in% d_params$prog_names)


  major_names <- c("Major","Second Major")
  minor_names <- c("First Minor","Second Minor")
  
  ##########  UNDERGRADUATES ##########
  message("[headcount.R] Filtering for undergrads...")

  hc_progs_under <- headcount_filtered %>% filter (`Student Level` == "Undergraduate")
  d_params$tables[["hc_progs_under"]] <- hc_progs_under

  hc_progs_under_long_majors <- hc_progs_under %>% filter (major_type %in% major_names & students > 0)
  d_params$tables[["hc_progs_under_long_majors"]] <- hc_progs_under_long_majors
  
  hc_progs_under_long_minors <- hc_progs_under %>% filter (major_type %in% minor_names & students > 0 ) 
  d_params$tables[["hc_progs_under_long_minors"]] <- hc_progs_under_long_minors
  
  

  ########## GRADUATE degree types ##########
  message("[headcount.R] Filtering for grads...")
  hc_progs_grad <- headcount_filtered %>% filter (`Student Level` == "Graduate/GASM")
  d_params$tables[["hc_progs_grad"]] <- hc_progs_grad
  
  hc_progs_grad_long_majors <- hc_progs_grad %>% filter (major_type %in% major_names & students > 0)
  d_params$tables[["hc_progs_grad_long_majors"]] <- hc_progs_grad_long_majors
  
  hc_progs_grad_long_minors <- hc_progs_grad %>% filter (major_type %in% minor_names & students > 0 )
  d_params$tables[["hc_progs_grad_long_minors"]] <- hc_progs_grad_long_minors
  
  
  
  #################################
  #  CREATE PLOTS

  message("[headcount.R] creating plots via p_params and adding to d_params...")

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


  message("[headcount.R] About to return d_params...")
  message("[headcount.R] d_params has ", length(d_params$tables), " tables and ", length(d_params$plots), " plots")
  message("[headcount.R] Table names: ", paste(names(d_params$tables), collapse = ", "))
  message("[headcount.R] Plot names: ", paste(names(d_params$plots), collapse = ", "))
  message("[headcount.R] returning d_params with new plot(s) and table(s)...")
  return(d_params)
}
