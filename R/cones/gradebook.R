# aggregate_grades expects:
# a vector of cols to group by and summarizes (in opt param)
# a dfw_summary table, with passed, failed, dropped for each course/instructor
aggregate_grades <- function(dfw_summary, opt) {
  # Check if dfw_summary is empty
  if (nrow(dfw_summary) == 0) {
    message("[gradebook.R] dfw_summary is empty, returning empty summary dataframe")
    return(dfw_summary)
  }
  
  group_cols <- opt[["group_cols"]]
  group_cols <- convert_param_to_list(group_cols)
  group_cols <- as.character(group_cols)
  
  # Verify all group_cols exist in dfw_summary
  missing_cols <- setdiff(group_cols, names(dfw_summary))
  if (length(missing_cols) > 0) {
    message("[gradebook.R] WARNING: group_cols contains columns not in dfw_summary: ", paste(missing_cols, collapse = ", "))
    message("[gradebook.R] Available columns: ", paste(names(dfw_summary), collapse = ", "))
    # Remove missing columns from group_cols
    group_cols <- intersect(group_cols, names(dfw_summary))
    message("[gradebook.R] Using only available columns: ", paste(group_cols, collapse = ", "))
  }
  
  summary <- dfw_summary %>%
    group_by(across(all_of(group_cols))) %>% 
    summarize(passed = sum(passed), failed = sum(failed), early_dropped = sum(early_dropped), late_dropped = sum(late_dropped), .groups="keep") 
  
  # add dfw column with summarized data
  # failed already includes all non-passing grades (including Drop from late drops)
  summary <- summary %>%
    mutate (`DFW %` = round(failed/(passed + failed)*100,digits=2))
  
  return (summary)
}


# main controller for function, to be called from cedar or course report; dept report uses next function)
get_grades <- function(students, hr_data, opt) {
  
  # for studio testing
  #opt <- list()
  #opt$course <- "MATH 1220"
  #opt$dept <- "AMST"

  message("[gradebook.R] Welcome to get_grades!")
  
  # display received table data info
  message("[gradebook.R] Received students data: ", nrow(students), " rows")
  message("[gradebook.R] Received HR data: ", nrow(hr_data), " rows")
  message("[gradebook.R] Options: ", toString(opt))

  # filter students from opt params (usually course and term OR dept for dept reports)
  filtered_students <- filter_class_list(students,opt)

  # If no data after filtering, return empty list
  # this happens for instance for when a unit has no lower division and we try to get grades for plotting dfw rates
  if (nrow(filtered_students) == 0) {
    message("[gradebook.R] No students found after filtering, returning empty list")
    return(list())
  }

  message("[gradebook.R] only using data since 2019 (after Gen Ed implementation).")
  filtered_students <- filtered_students %>% filter (`Academic Period Code` >= 201980)
  
  #filtered_students <- filtered_students %>% filter (`Academic Period Code` == 202480)
  #filtered_students <- filtered_students %>% filter (`Primary Instructor Last Name` == "Jadalla")
  #filtered_students <- filtered_students %>% filter (SUBJ_CRSE == "MATH 1220")
  #hr_data <- fac_by_term
  
  message("[gradebook.R] setting Final Grade to `Drop` if registration status code is `DR`.")
  filtered_students <- filtered_students %>% mutate (`Final Grade` = ifelse(`Registration Status Code`=="DR", "Drop", `Final Grade`))
    
  # get distinct IDs in each course (be sure to use CRN, since same student can retake a course)
  message("[gradebook.R] finding distinct rows based on Student ID, Course Campus Code, Course College Code, Course Reference Number (CRN)... ")
  filtered_students <- filtered_students %>% distinct(`Student ID`,`Course Campus Code`,`Course College Code`, `Course Reference Number`, .keep_all=TRUE)
    
  # calculate grade points from letter grade received and add col to student data 
  # grades_to_points is defined in mappings.R
  message("[gradebook.R] merging grades_to_points table with grade data...")
  message("[gradebook.R] Rows before merge: ", nrow(filtered_students))
  filtered_students <- merge(filtered_students, grades_to_points, by.x="Final Grade", by.y="grade",all.x= TRUE)
  message("[gradebook.R] Rows after merge: ", nrow(filtered_students))

  # produce summary of grades (how many students got each grade in each section) 
  message("[gradebook.R] Producing summary of grades grouped by campus, college, term, course, instructor...")
  grade_counts <- filtered_students %>% group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE, `Primary Instructor Last Name`, `Primary Instructor ID` , `Final Grade`) %>% 
    summarize (count=n(), .groups="keep")
  message("[gradebook.R] Total grade records in grade_counts: ", sum(grade_counts$count))

  
  fac_small <- hr_data %>%
    distinct(`UNM ID`, term_code, .keep_all = TRUE) %>%  
    select(`UNM ID`, term_code, job_cat) 
  message("[gradebook.R] Rows in fac_small: ", nrow(fac_small))

  message("[gradebook.R] Merging faculty job category data by instructor ID AND term...")
  merged <- grade_counts %>% merge(fac_small, by.x=c("Primary Instructor ID", "Academic Period Code"), by.y=c("UNM ID", "term_code"))

  # test if trying to merge faculty data squashes student data
  if (nrow(merged) == 0) {
    message("[gradebook.R] Merging grade_counts with hr_data resulted in 0 rows. Probably a non A&S unit.")
  } else {
    message("[gradebook.R] Merging grade_counts with hr_data yielded rows: ", nrow(merged))
    grade_counts <- merged
  }

  message("[gradebook.R] defining standard group cols...")

  # Only include job_cat if it exists in grade_counts (i.e., if merge succeeded)
  group_cols <- c("Course Campus Code", "Course College Code", "Academic Period Code", "SUBJ_CRSE", "Primary Instructor Last Name") 
  if ("job_cat" %in% names(grade_counts)) {
    group_cols <- c(group_cols, "job_cat")
  }
  message("[gradebook.R] group_cols: ", paste(group_cols, collapse = ", "))  

  # TODO: make sure job_cat plot is skipped if job_cat col not present

  # passing_grades defined in includes/lists.R
  message("[gradebook.R] gathering passing grades...")
  message("[gradebook.R] passing grades are: ", paste(passing_grades, collapse = ", ") )
  passed <- grade_counts %>% filter (`Final Grade` %in% passing_grades) %>%  
    group_by(across(all_of(group_cols))) %>% 
    summarize (passed=sum(count), .groups="keep")

  # get counts of failing grades (not passing grades, including late drops)
  message("[gradebook.R] gathering failing grades (anything not in passing grades list, but not counting early drops)...")
  failed <- grade_counts %>% filter (`Final Grade` != "DR" & !`Final Grade` %in% passing_grades) %>% 
    group_by(across(all_of(group_cols))) %>% 
    summarize (failed=sum(count), .groups="keep")
  
  # get counts of withdraws (W) for more granular DFW analysis
  message("[gradebook.R] gathering late drops (i.e. W for a grade...")
  late_drops <- grade_counts %>% filter (`Final Grade` == "W") %>% 
    group_by(across(all_of(group_cols))) %>% 
    summarize (late_dropped=sum(count), .groups="keep")
  
  message("[gradebook] gathering EARLY DROPS (DR)...")
  early_drops <- grade_counts %>% filter (`Final Grade` == "Drop") %>% 
    group_by(across(all_of(group_cols))) %>% 
    summarize (early_dropped=sum(count), .groups="keep")
  
  # merge all summaries and create row for each term/course/instructor/method/pt combo
  message("[gradebook] merging data...")
  dfw_summary <- merge(passed, failed, all=TRUE)
  dfw_summary <- merge(dfw_summary, late_drops, all=TRUE)
  dfw_summary <- merge(dfw_summary, early_drops, all=TRUE)

  # replace all NAs with 0s
  message("[gradebook] replacing NAs with 0s...")
  dfw_summary <- dfw_summary %>% mutate_if(is.numeric, ~replace_na(., 0))
    
  # calculate DFW % for each course/instructor/term/method/pt combo
  # failed already includes Drop grades (from DR, DW, W, F, etc.
  # don't include DR, since those are early drops and not in enrollment counts
  message("[gradebook.R] calculating DFW %...")
  message("[gradebook.R] Based on class list data, count of failed/(passed+failed-early_dropped")
  dfw_summary <- dfw_summary %>% 
    mutate (`DFW %`=round(failed/(passed+failed)*100,digits=2))
    
  # initialize grades list for return data
  grades_list <- list()

  # add tables to return list
  grades_list[["counts"]] <- grade_counts
  grades_list[["dfw_summary"]] <- dfw_summary

  #message("dfw_summary:")
  #message(dfw_summary)

  ####################
  # GRADE AGGREGATIONS
  ####################
  
  # get averages by course, campus, college, and instructor but NOT TERM to get mean across terms
  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Primary Instructor Last Name", "SUBJ_CRSE")
  course_inst_avg <- aggregate_grades(dfw_summary, opt)
  
  # Add section count per instructor
  # Count unique term/course combinations per instructor (each term counted separately)
  # Using explicit columns to avoid issues with job_cat or other extra columns creating duplicates
  instructor_section_counts <- grade_counts %>%
    distinct(`Course Campus Code`, `Primary Instructor Last Name`, `Academic Period Code`, SUBJ_CRSE) %>%
    group_by(`Course Campus Code`, `Primary Instructor Last Name`) %>%
    summarize(sections_taught = n(), .groups = "drop")
  message("[gradebook.R] Rows in instructor_section_counts: ", nrow(instructor_section_counts))


  # Merge the section counts back into the course_inst
  grades_list[["course_inst_avg"]] <- course_inst_avg %>% ungroup() %>% 
    left_join(instructor_section_counts, by = c("Course Campus Code", "Primary Instructor Last Name"))
  message("[gradebook.R] Rows in course_inst_avg: ", nrow(grades_list[["course_inst_avg"]]))

  # get course averages by inst type
  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "SUBJ_CRSE", "job_cat")
  grades_list[["inst_type"]] <- aggregate_grades(dfw_summary, opt)
  message("[gradebook.R] Rows in inst_type: ", nrow(grades_list[["inst_type"]]))
  
  
  # get course averages by campus and college
  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "SUBJ_CRSE")
  grades_list[["course_term"]] <- aggregate_grades(dfw_summary, opt)
  message("[gradebook.R] Rows in course_term: ", nrow(grades_list[["course_term"]]))

  # get course averages (all terms)
  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "SUBJ_CRSE")
  grades_list[["course_avg"]] <- aggregate_grades(dfw_summary, opt)
  message("[gradebook.R] Rows in course_avg: ", nrow(grades_list[["course_avg"]]))

  # get course averages (for each terms)
  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "SUBJ_CRSE", "Academic Period Code")
  grades_list[["course_avg_by_term"]] <- aggregate_grades(dfw_summary, opt)
  message("[gradebook.R] Rows in course_avg_by_term: ", nrow(grades_list[["course_avg_by_term"]]))


  message("[gradebook.R] returning grades_list...")
  return (grades_list)  
}



# generate grade plots for course report
plot_grades_for_course_report <- function(grades, opt) {

  # studio testing...
  #grades <- grades_list
  
  message("[gradebook.R] Welcome to plot_grades_for_course_report!")

  # Check if grades data is available
  if (is.null(grades) || length(grades) == 0) {
    message("[gradebook] No grades data available for plotting.")
    return(NULL)
  } else {
    message("[gradebook.R] Grades data contains ", length(grades), " tables for plotting. Objects in grades object: ", paste(names(grades), collapse = ", "))
  }

  # Create a list to hold the plots
  plots <- list()

# get dfw_summary by course and term 
  dfw_summary_by_course <- grades[["course"]]
  
  # get dfw_summary averages across terms
  dfw_summary_by_course_avg <- grades[["course_avg"]]
  
  # get instructor-level averages across terms
  instructor_data <- grades[["course_inst_avg"]] %>%
    filter(!is.na(`Primary Instructor Last Name`) & `Primary Instructor Last Name` != "")
  
  # Create consistent factor levels for both datasets
  course_levels <- dfw_summary_by_course_avg %>% 
    arrange(`SUBJ_CRSE`) %>% 
    pull(SUBJ_CRSE) %>%
    unique()
  
  # Prepare bar data (CEDAR pattern)
  bar_data <- dfw_summary_by_course_avg %>% 
    mutate(SUBJ_CRSE = factor(SUBJ_CRSE, levels = course_levels))
  
  # Prepare instructor point data - use same structure as bar data for consistent positioning
  point_data <- instructor_data %>% 
    mutate(SUBJ_CRSE = factor(SUBJ_CRSE, levels = course_levels)) %>%
    group_by(SUBJ_CRSE, `Course Campus Code`) %>%
    mutate(instructor_index = row_number() - 1) %>%  # Index for stacking multiple instructors
    ungroup()

  # Define consistent dodge width
  dodge_width <- 0.8
  
  message("[gradebook.R] Plotting DFW summary plot...")
  dfw_summary_plot <- bar_data %>%
    ggplot(aes(y=SUBJ_CRSE, x=`DFW %`, fill=`Course Campus Code`,
               text = paste("Course:", SUBJ_CRSE, 
                           "<br>Campus:", `Course Campus Code`,
                           "<br>DFW %:", `DFW %`))) + 
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity", position=position_dodge(width=dodge_width), alpha=0.7) +
    geom_point(data = point_data,
               aes(x=`DFW %`, 
                   y=SUBJ_CRSE,  # Use the factor directly - let position_dodge handle positioning
                   color=`Course Campus Code`,
                   text = paste("Instructor:", `Primary Instructor Last Name`,
                               "<br>Course:", SUBJ_CRSE,
                               "<br>Campus:", `Course Campus Code`, 
                               "<br>DFW %:", `DFW %`,
                               "<br>Sections Taught:", sections_taught)), 
               position=position_jitterdodge(dodge.width=dodge_width, jitter.height=0.15, jitter.width=0),
               size=2, alpha=0.8, inherit.aes = FALSE) +
    ylab("Course") + xlab("mean DFW %")  +
    labs(caption = "Bars show course averages; dots show individual instructor averages")
  
  # Convert to plotly for interactivity and store in plots list
  plots[["dfw_summary_plot"]] <- ggplotly(dfw_summary_plot, tooltip = "text")
  plots$dfw_summary_plot

  # line plot of course averages by term and combine with bar plot
  message("[gradebook.R] Plotting DFW by term...")
  term_data <- grades[["course_avg_by_term"]]
  if (!is.null(term_data) && nrow(term_data) > 0) {
    term_levels <- sort(unique(term_data$`Academic Period Code`))
    term_plot <- term_data %>%
      mutate(
        AcadTerm = factor(`Academic Period Code`, levels = term_levels),
        SUBJ_CRSE = as.character(SUBJ_CRSE)
      ) %>%
      ggplot(aes(x = AcadTerm, y = `DFW %`, group = SUBJ_CRSE, color = `Course Campus Code`,
                 text = paste("Course:", SUBJ_CRSE,
                              "<br>Term:", `Academic Period Code`,
                              "<br>DFW %:", `DFW %`))) +
      geom_line() +
      geom_point() +
      labs(x = "Academic Period", y = "DFW %", title = "Course averages by term") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")

    plots[["dfw_by_term_plot"]] <- ggplotly(term_plot, tooltip = "text")
    #plots[["dfw_by_term_plot"]]
  }


  # plot of DFW rates by intructor type over
  message("[gradebook.R] Plotting DFW by instructor type...")
  term_data <- grades[["inst_type"]]
  if (!is.null(term_data) && nrow(term_data) > 0) {
    term_levels <- sort(unique(term_data$`Academic Period Code`))
    term_plot <- term_data %>%
      mutate(
        AcadTerm = factor(`Academic Period Code`, levels = term_levels),
        SUBJ_CRSE = as.character(SUBJ_CRSE)
      ) %>%
      ggplot(aes(x = AcadTerm, y = `DFW %`, fill = `job_cat`,
                 text = paste("Course:", SUBJ_CRSE,
                              "<br>Term:", `Academic Period Code`,
                              "<br>DFW %:", `DFW %`))) +
      #geom_line() +
      #geom_point() +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~`Course Campus Code`, ncol = 1) +
      labs(x = "Academic Period", y = "DFW %", title = "Course averages by instructor type") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")

    plots[["dfw_by_inst_type_plot"]] <- ggplotly(term_plot, tooltip = "text")
    #plots[["dfw_by_inst_type_plot"]]
  }


  message("[gradebook.R] Returning ", length(plots), " grade plots for course report.")  
  return(plots)
}



# this is specifically for creating dept report outputs using d_params 
# it does additional filtering for lower division courses if available
get_grades_for_dept_report <- function(students, hr_data, opt, d_params) {
  
  # studio testing set up  
  #opt <- list()
  #opt[["dept"]] <- "MATH"
  #students <- load_students()
  #hr_data <- load_datafile("hr_data")
  
  # for plotting
  myopt <- opt
  myopt[["dept"]] <- d_params$dept_code 
  
  
  # limit to ABQ campus and online until we have better plotting across campuses
  message("[gradebook.R] limiting to ABQ and EA campus for plotting...")
  myopt[["course_campus"]] <- c("ABQ","EA")

  message("[gradebook.R] limiting to lower division courses for plotting...")
  myopt[["level"]] <- "lower"
  
  # get various grade tables for the specified department
  grades <- get_grades(students, hr_data, myopt)
  
  # handle case of empty grades object
  if (is.null(grades) || length(grades) == 0) {
    message("[gradebook.R] No grades data available after filtering for plotting. Returning d_params unchanged.")
    return(d_params)
  } else {
    message("[gradebook.R] Grades data contains ", length(grades), " tables for plotting. Objects in grades object: ", paste(names(grades), collapse = ", "))
  }

  # get dfw_summary by course and term 
  dfw_summary_by_course <- grades[["course"]]

  message("[gradebook.R] adding dfw_summary_by_course to d_params...")
  d_params$tables[["grades_summary_for_ld"]] <- dfw_summary_by_course

  # get dfw_summary averages across terms
  dfw_summary_by_course_avg <- grades[["course_avg"]]
  
  # get instructor-level averages across terms
  instructor_data <- grades[["course_inst_avg"]] %>%
    filter(!is.na(`Primary Instructor Last Name`) & `Primary Instructor Last Name` != "")
  
  # Create consistent factor levels for both datasets
  course_levels <- dfw_summary_by_course_avg %>% 
    arrange(`SUBJ_CRSE`) %>% 
    pull(SUBJ_CRSE) %>%
    unique()
  
  dfw_summary_for_ld_plot <- dfw_summary_by_course_avg %>% 
    mutate(SUBJ_CRSE = factor(SUBJ_CRSE, levels = course_levels)) %>%
    ggplot(aes(y=SUBJ_CRSE, x=`DFW %`, fill=`Course Campus Code`,
               text = paste("Course:", SUBJ_CRSE, 
                           "<br>Campus:", `Course Campus Code`,
                           "<br>DFW %:", `DFW %`))) + 
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity", position=position_dodge(), alpha=0.7) +
    geom_point(data = instructor_data %>% 
                 mutate(SUBJ_CRSE = factor(SUBJ_CRSE, levels = course_levels)),
               aes(x=`DFW %`, y=SUBJ_CRSE, color=`Course Campus Code`,
                   text = paste("Instructor:", `Primary Instructor Last Name`,
                               "<br>Course:", SUBJ_CRSE,
                               "<br>Campus:", `Course Campus Code`, 
                               "<br>DFW %:", `DFW %`,
                               "<br>Sections Taught:", sections_taught)), 
               position=position_jitter(height=0.2, width=0), 
               size=2, alpha=0.8) +
    ylab("Course") + xlab("mean DFW %")  +
    labs(caption = "Bars show course averages; dots show individual instructor averages")
  
  # Convert to plotly for interactivity
  dfw_summary_for_ld_plot <- ggplotly(dfw_summary_for_ld_plot, tooltip = "text")
  
  dfw_summary_for_ld_plot
  


  message("[gradebook] adding grades_summary_for_ld_abq_ea_plot to d_params...")
  d_params$plots[["grades_summary_for_ld_abq_ea_plot"]] <- dfw_summary_for_ld_plot 

  
  message("[gradebook] returning d_params with new plot(s) and table(s)...")
  return(d_params)
}

