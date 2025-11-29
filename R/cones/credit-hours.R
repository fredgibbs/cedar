# get stats on how many credit hours students are taking
get_enrolled_cr <- function(filtered_students,courses,opt) {
  message("welcome to get_enrolled_cr!")
  
  # uncomment for testing
  # filtered_students <- load_students()
  # courses <- list('HIST 491',"HIST 492")
  #opt[["course"]] <- cl_crits
  
  #opt <- list()
  #myopt <- opt
  
  # filter students by courses (all terms) and group
  message("computing summary stats...")
  filtered_students <- filtered_students %>% distinct(`Student ID`, .keep_all=TRUE) %>% 
    group_by(`Academic Period Code`, `Total Credits`, term_type)
  
  summary <- filtered_students %>% summarize(count = n())
  
  summary_wide <- summary %>% pivot_wider(names_from = `Academic Period Code`, values_from = count)
  
  message("get_enrolled_cr now returning regstats...")
  return (summary_wide)
}



# This function makes a quick report of EARNED credit hours (based on class lists)
# expects students is already filtered according to opt params
get_credit_hours <- function (students) {
  message("[credit-hours.R] Welcome to get_credit_hours!")
  
  message("[credit-hours.R] filtering students by passing_grades...")
  filtered_students <- students %>%  filter(`Final Grade` %in% passing_grades)
  message("[credit-hours.R] Completed filtering, got ", nrow(filtered_students), " rows")

  message("[credit-hours.R] summarizing...")
  filtered_students_summary <- filtered_students %>% 
    group_by(`Academic Period Code`,`Course Campus Code`, `Course College Code`,DEPT,level, `Subject Code`) %>% 
    summarize(total_hours = sum(`Course Credits`), .groups="keep")
  message("[credit-hours.R] Completed summarizing, got ", nrow(filtered_students_summary), " rows")

  message("[credit-hours.R] creating totals across levels...")
  credit_hours_totals <- filtered_students %>% group_by(`Academic Period Code`,`Course Campus Code`,`Course College Code`,DEPT,`Subject Code`) %>% 
    summarize(level="total",total_hours = sum(`Course Credits`), .groups="keep")
  message("[credit-hours.R] Completed creating totals, got ", nrow(credit_hours_totals), " rows")

  message("[credit-hours.R] adding totals to level data...")
  credit_hours_data <- rbind(filtered_students_summary,credit_hours_totals) %>%  
    arrange(`Academic Period Code`,`Course Campus Code`,`Course College Code`,DEPT,`Subject Code`, factor(level,levels=c("lower","upper","grad","total")))
  message("[credit-hours.R] Completed rbind and arrange, got ", nrow(credit_hours_data), " rows")

  # removed add_acad_year call - using Academic Period Code directly
  message("[credit-hours.R] About to return credit_hours_data...")
  return(credit_hours_data)
}


# parse earned credit hours by major
# use class lists to look at who got passing grades and number of credits registered for
# tally these up to get credit hours for a subset of students
# called from dept-report.R
credit_hours_by_major <- function (students,d_params) {
  
  # general idea:
  # get ids from students in DEPT courses
  # merge with headcount to get all student data, like  majors, minors, second majors, second minors
  
  message("DEBUG: Starting credit_hours_by_major for dept: ", d_params$dept_code)
  message("filtering students by d_params...")
  filtered_students <- students %>% filter(`DEPT` == d_params$dept_code)
  message("DEBUG: After filtering by dept, got ", nrow(filtered_students), " rows")
  
  filtered_students <- filtered_students %>% filter (as.integer(`Academic Period Code`) >= d_params$term_start & as.integer(`Academic Period Code`) <= d_params$term_end) 
  message("DEBUG: After filtering by term range (", d_params$term_start, "-", d_params$term_end, "), got ", nrow(filtered_students), " rows")
  
  
  # filter out non credit earning students; passing_grades defined in includes/map_to_subj_code.R
  message("filtering students by passing grades...")
  filtered_students <- filtered_students %>%  filter(`Final Grade` %in% passing_grades)
  message("DEBUG: After filtering by passing grades, got ", nrow(filtered_students), " rows")
  
  # remove Pre from major and add boolean flag in separate column
  message("integrating pre-majors...")
  filtered_students$pre <- ifelse ( grepl("Pre", filtered_students$Major),TRUE,FALSE)
  filtered_students$Major <- str_remove(filtered_students$Major, "Pre ")
  filtered_students$Major <- str_remove(filtered_students$Major, "Pre-")
  
  message("standardizing data ...")
  filtered_students$`Student College` <- str_replace(filtered_students$`Student College`, "College of Educ & Human Sci", "College of Education")
  # removed add_acad_year call - using Academic Period Code directly
  
  
  # find students in SUBJ courses who are NOT majoring (first) in any programs of that dept
  #prgm_to_dept_map[[major_to_program_map[["East Asian Studies"]]]]
  #message("finding non-majors ...")
  
  # create col to indicate "home" program of student based on Major col
  #filtered_students$student_major_dept <-  prgm_to_dept_map[major_to_program_map[filtered_students$Major]]
  #non_majors <- filtered_students %>% filter (is.na(student_major_dept) | student_major_dept != opt$dept)
  #non_majors <- filtered_students %>% filter (prgm_to_dept_map[major_to_program_map[Major]] != opt$dept)
  
  # summarize by academic period code, student's home college, and major
  message("summarizing credit hours by student college and major...")
  credit_hours_data <- filtered_students %>% 
    group_by(`Academic Period Code`,`Student College`,Major) %>% 
    summarize(total_hours = sum(`Course Credits`)) %>% 
    arrange (`Academic Period Code`,desc(total_hours))
  message("DEBUG: After summarizing, got ", nrow(credit_hours_data), " rows")
  
  # create wide view with periods as columns
  message("DEBUG: About to pivot wider...")
  credit_hours_data_w <- credit_hours_data %>% pivot_wider(names_from = `Academic Period Code`, values_from = total_hours)
  message("DEBUG: After pivot_wider, got ", nrow(credit_hours_data_w), " rows and ", ncol(credit_hours_data_w), " columns")
  message("DEBUG: Column names after pivot: ", paste(names(credit_hours_data_w), collapse=", "))
  
  # create totals line
  message("DEBUG: Creating totals row...")
  # Get numeric column names (exclude the first two character columns)
  numeric_cols <- names(credit_hours_data_w)[3:ncol(credit_hours_data_w)]
  message("DEBUG: Numeric columns for totals: ", paste(numeric_cols, collapse=", "))
  
  # Create totals row by summing numeric columns
  totals_row <- credit_hours_data_w %>% 
    ungroup() %>%
    summarise(
      `Student College` = "Total",
      Major = "Total",
      across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE))
    )
  message("DEBUG: Successfully created totals row")
  
  credit_hours_data_w <- credit_hours_data_w %>% ungroup() %>% 
    bind_rows(totals_row)
  message("DEBUG: After adding totals row, got ", nrow(credit_hours_data_w), " rows")
  
  message("DEBUG: About to convert columns to numeric...")
  credit_hours_data_w <- credit_hours_data_w %>% mutate_at(c(3:ncol(credit_hours_data_w)), as.numeric)
  credit_hours_data_w[is.na(credit_hours_data_w)] <- 0
  message("DEBUG: Completed numeric conversion and NA replacement")
  
  # Sort by the most recent academic period (last column)
  if (ncol(credit_hours_data_w) > 2) {
    last_period_col <- names(credit_hours_data_w)[ncol(credit_hours_data_w)]
    message("DEBUG: Sorting by last period column: ", last_period_col)
    credit_hours_data_w <- credit_hours_data_w %>% arrange(desc(.data[[last_period_col]]))
  } 
  
  # save data for report
  message("DEBUG: Saving credit_hours_data_w table to d_params")
  d_params$tables[["credit_hours_data_w"]] <- credit_hours_data_w 
  
  
  # prep data for pie chart for NON-MAJORS
  message("finding non-majors' share of total credit hours ...")
  message("DEBUG: Available columns in credit_hours_data_w: ", paste(names(credit_hours_data_w), collapse=", "))
  
  sch_outside_pct <- credit_hours_data_w %>% filter (!(Major %in% d_params$prog_names))
  sch_outside_pct <- sch_outside_pct %>% filter (Major != "Total")  %>%  slice_head(n=9)
  message("DEBUG: sch_outside_pct has ", nrow(sch_outside_pct), " rows")
  
  # Check if we have data and get the most recent period column
  if (nrow(sch_outside_pct) > 0 && ncol(credit_hours_data_w) > 2) {
    recent_period_col <- names(credit_hours_data_w)[ncol(credit_hours_data_w)]
    message("DEBUG: Using period column: ", recent_period_col)
    
    # reorder Majors in order of credit hours using the most recent period
    sch_outside_pct <- sch_outside_pct %>%  mutate(Major = fct_reorder(Major, .data[[recent_period_col]]))
    
    message("creating plot...")
    sch_outside_pct_plot <- plot_ly(sch_outside_pct, labels = ~Major, values = as.formula(paste0("~`", recent_period_col, "`")), marker = list(colorscale = 'Viridis'))
    sch_outside_pct_plot <- sch_outside_pct_plot %>%
      add_pie(hole = 0.6) %>% 
      layout(title = '',
             legend = list(x = 100, y = 0.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    message("DEBUG: Successfully created sch_outside_pct_plot")
  } else {
    message("DEBUG: Insufficient data for sch_outside_pct plot")
    sch_outside_pct_plot <- NULL
  }
  
  message("saving plot in d_params...")
  d_params$plots[["sch_outside_pct_plot"]] <- sch_outside_pct_plot
  
  
  # prep data for pie chart of DEPT HOURS COMPARED AS PART OF TOTAL
  # this is way faster than making the filter an OR test
  message("finding majors' share of total credit hours ...")
  message("DEBUG: About to create department percentage chart...")
  
  sch_dept_pct_total <- credit_hours_data_w %>% filter (Major == "Total") %>% mutate (Major = "Non-majors")
  sch_dept_pct_program <- credit_hours_data_w %>% filter (Major %in% d_params$prog_names)
  sch_dept_pct <- rbind(sch_dept_pct_total,sch_dept_pct_program)
  message("DEBUG: sch_dept_pct has ", nrow(sch_dept_pct), " rows")
  
  # Check if we have data and create plot using dynamic period column
  if (nrow(sch_dept_pct) > 0 && ncol(credit_hours_data_w) > 2) {
    recent_period_col <- names(credit_hours_data_w)[ncol(credit_hours_data_w)]
    message("DEBUG: Creating dept pct plot with period column: ", recent_period_col)
    
    message("creating plot...")
    sch_dept_pct_plot <- plot_ly(sch_dept_pct, labels = ~Major, values = as.formula(paste0("~`", recent_period_col, "`")), type = 'pie', marker = list(colorscale = 'Viridis'))
    sch_dept_pct_plot <- sch_dept_pct_plot %>% 
      layout(legend = list(x = 100, y = 0.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    message("DEBUG: Successfully created sch_dept_pct_plot")
  } else {
    message("DEBUG: Insufficient data for sch_dept_pct plot")
    sch_dept_pct_plot <- NULL
  }
  message("saving plot...")
  #save(sch_dept_pct_plot, file="credit-hours/sch_dept_pct_plot.Rda")
  d_params$plots[["sch_dept_pct_plot"]] <- sch_dept_pct_plot
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}


# this function generates raw student credit hour data by faculty and graph for use in dept-reports
# called from dept-report.R
credit_hours_by_fac <- function (data_objects, d_params) {
  message("[credit-hours.R] Welcome to credit_hours_by_fac!")

  students <- data_objects[["class_lists"]]
  
  # load faculty data for associating title with person in course listings
  fac_by_term <- data_objects[["hr_data"]] 
  
  # Debug: Check what's in data_objects
  message("DEBUG: data_objects keys: ", paste(names(data_objects), collapse=", "))
  message("DEBUG: hr_data class: ", class(fac_by_term))
  message("DEBUG: hr_data is null: ", is.null(fac_by_term))
  
  if (is.null(fac_by_term)) {
    message("ERROR: hr_data is NULL in data_objects")
    d_params$plots[["chd_by_fac_facet_plot"]] <- "No HR data available"
    d_params$plots[["chd_by_fac_plot"]] <- "No HR data available"
    return(d_params)
  }
  
  if (nrow(fac_by_term) == 0) {
    message("ERROR: hr_data is empty")
    d_params$plots[["chd_by_fac_facet_plot"]] <- "No HR data available"
    d_params$plots[["chd_by_fac_plot"]] <- "No HR data available"
    return(d_params)
  }
  
  message("DEBUG: Using HR data from data_objects with ", nrow(fac_by_term), " rows")
  
  # for studio testing
  # d_params <- list(dept_code="CJ",subj_codes=c("CJ","COMM"),palette="Spectral")
  
  # filter out non credit earning students; passing_grades defined in includes/map_to_subj_code.R
  message("DEBUG: Filtering students by passing grades and dept...")
  filtered_students <- students %>%  filter(`Final Grade` %in% passing_grades & DEPT == d_params$dept_code)
  message("DEBUG: After filtering by passing grades and dept, got ", nrow(filtered_students), " rows")
  
  # filter for term params
  filtered_students <- filtered_students %>%  filter (`Academic Period Code` >= d_params$term_start &
                                                      `Academic Period Code` <= d_params$term_end)
  message("DEBUG: After filtering by term range, got ", nrow(filtered_students), " rows")
  
  message("DEBUG: About to merge faculty data with student data...")
  fac_by_term <- fac_by_term %>% select(-c("as_of_date"))
  merged <- merge(filtered_students,fac_by_term,by.x=c("Academic Period Code","Primary Instructor ID","DEPT"),by.y=c("term_code","UNM ID","DEPT"),x.all=TRUE)
  message("DEBUG: After merge, got ", nrow(merged), " rows")
  
  # summarize total hours earned by job_cat (in faculty data)
  message("DEBUG: Summarizing by job category...")
  credit_hours_data <- merged %>% 
    group_by(`Academic Period Code`, `Course Campus Code`, `Course College Code`, DEPT, level, job_cat) %>% 
    summarize(total_hours = sum(`Course Credits`), .groups="keep")
  message("DEBUG: After summarizing by job_cat, got ", nrow(credit_hours_data), " rows")
  
  
  credit_hours_data_main <- credit_hours_data %>%  filter (`Course Campus Code` %in% c("ABQ","EA")) 

  credit_hours_data_main <- credit_hours_data_main %>% 
    group_by(`Academic Period Code`, `Course College Code`, DEPT, level, job_cat) %>% 
    summarize(total_hours = sum(total_hours), .groups="keep")
  

  # create BAR PLOT (colored by job_cat), and FACETED by course level 
  if (nrow(credit_hours_data) > 0) {
  chd_by_fac_facet_plot <- credit_hours_data_main %>% 
    #mutate(job_cat = fct_reorder(job_cat, total_hours)) %>%
    ggplot(aes(x=`Academic Period Code`, y=total_hours)) + 
    ggtitle(paste0("using SUBJ codes: ",paste(d_params$subj_codes, collapse=", "))) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(aes(fill=job_cat),stat="identity", position="dodge") +
    facet_wrap(~level) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_fill_brewer(palette=d_params$palette) +
    xlab("Academic Period") + ylab("Credit Hours")
  }
  else {
    chd_by_fac_facet_plot <- "insufficient data."
  }
  
  chd_by_fac_facet_plot
  
  d_params$plots[["chd_by_fac_facet_plot"]] <- chd_by_fac_facet_plot
  
  
  
  # remove level from summaries
  credit_hours_data_main <- credit_hours_data_main %>% 
    group_by(`Academic Period Code`, `Course College Code`,DEPT, job_cat) %>% 
    summarize(total_hours = sum(`total_hours`))
  
  # create BAR PLOT of CH TOTALS colored by job_cat
  chd_by_fac_plot <-  credit_hours_data_main %>% 
    #mutate(job_cat = fct_reorder(job_cat, total_hours)) %>%
    ggplot(aes(x=`Academic Period Code`, y=total_hours)) + 
    ggtitle(paste0("using SUBJ codes: ",paste(d_params$subj_codes, collapse=", "))) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(aes(fill=job_cat),stat="identity", position="stack") +
    scale_fill_brewer(palette=d_params$palette) +
    #scale_fill_discrete(limits=c("lower","upper","grad")) +
    xlab("Academic Period") + ylab("Credit Hours")
  
  chd_by_fac_plot
  d_params$plots[["chd_by_fac_plot"]] <- chd_by_fac_plot
  
  message("DEBUG: Successfully completed credit_hours_by_fac")
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
  
} # end credit_hours_by_fac


###########################
# called from dept-report.R
get_credit_hours_for_dept_report <- function (class_lists, d_params) {
  message("[credit-hours.R] Welcome to get_credit_hours_for_dept_report!")
  
  # TODO: basic filtering from d_params

  # get basic credit hours data
  message("[credit-hours.R] Getting credit hours data...")
  credit_hours_data <- get_credit_hours(class_lists)

  # TODO: add SUBJ to data?

  # filter for term params
  message("filtering for d_params terms...")
  credit_hours_data <- credit_hours_data %>%  filter (`Academic Period Code` >= d_params$term_start & `Academic Period Code` <= d_params$term_end)
  
  
  # group by academic year and dept to create summary of college hours
  # FILTER FOR AS, ABQ (ABQ and EA have the same totals)
  # group only by period code and dept, so all campuses  (ABE and EA) and subject codes are included 
  college_credit_hours <- credit_hours_data %>%  filter (`Course College Code` ==  "AS") %>% 
    filter(`Course Campus Code` %in% c("ABQ","EA")) %>%
    group_by(`Academic Period Code`, DEPT) %>% 
    filter (level == "total") %>% 
    summarize (total_hours = sum(total_hours))
  
  
  college_credit_hours_plot <- ggplot(college_credit_hours, aes(x=`Academic Period Code`, y=total_hours)) + 
    #ggtitle(d_params$prog_name) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(aes(fill=DEPT),stat="identity",position="stack") + 
    scale_color_brewer(palette="Spectral") +
    xlab("Academic Period") + ylab("Credit Hours") 
  
  college_credit_hours_plot <- ggplotly(college_credit_hours_plot)
  college_credit_hours_plot
  d_params$plots[["college_credit_hours_plot"]] <- college_credit_hours_plot
  
  # add term type col
  college_credit_hours <- add_term_type_col(college_credit_hours,"Academic Period Code") %>% distinct()
  
  # create totals for college, grouped by academic year
  college_credit_hours <- college_credit_hours %>%  group_by (`Academic Period Code`) %>% mutate (college_total = sum(total_hours))
  
  # filter by dept code
  dept_credit_hours <- college_credit_hours %>% filter (DEPT == d_params$dept_code)
  
  
  # compute per-year % change in dept compared to that of college
  diff_fr_college_hours <- dept_credit_hours %>%  group_by (term_type, `Academic Period Code`, DEPT) %>% arrange(DEPT,term_type,`Academic Period Code`) 
  
  # add percent diff from prev term_type
  diff_fr_college_hours <- diff_fr_college_hours %>% group_by(term_type) %>% mutate (diff_d = total_hours / lag(total_hours,n=1) * 100)
  
  
  
  
  # add percent diff from prev year
  diff_fr_college_hours <- diff_fr_college_hours %>%  group_by(term_type) %>% mutate (diff_c = college_total / lag(college_total,n=1) * 100)
  
  # calc diff between college and department deltas
  diff_fr_college_hours <- diff_fr_college_hours %>%  group_by (term_type)  %>% mutate (diff_heavy = diff_d  -  diff_c )
  
  

  label <- paste0("period change for ",d_params$dept_code,": ")
  
  college_credit_hours_comp_plot <- ggplot(diff_fr_college_hours, aes(x=`Academic Period Code`, y=diff_heavy)) + 
    #ggtitle(d_params$prog_name) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(aes(),stat="identity",position="stack") + 
    scale_color_brewer(palette=d_params$palette) +
    xlab("Academic Year") + ylab("% diff from College") 
  
  college_credit_hours_comp_plot
  d_params$plots[["college_credit_hours_comp_plot"]] <- college_credit_hours_comp_plot
  
  
  # Create dual y-axis comparison plot
  # Get department totals by academic period
  dept_totals <- credit_hours_data %>%
    filter(DEPT == d_params$dept_code) %>%
    filter(`Course Campus Code` %in% c("ABQ","EA")) %>%
    group_by(`Academic Period Code`) %>%
    summarise(dept_total = sum(total_hours), .groups = 'drop')
  
  # Get college totals by academic period
  college_totals <- credit_hours_data %>%
    filter(`Course College Code` == "AS") %>%
    filter(`Course Campus Code` %in% c("ABQ","EA")) %>%
    group_by(`Academic Period Code`) %>%
    summarise(college_total = sum(total_hours), .groups = 'drop')
  
  # Combine the data
  dual_axis_data <- merge(dept_totals, college_totals, by = "Academic Period Code", all = TRUE)
  dual_axis_data[is.na(dual_axis_data)] <- 0
  
  # Calculate scaling factor to normalize department data to college scale
  max_college <- max(dual_axis_data$college_total, na.rm = TRUE)
  max_dept <- max(dual_axis_data$dept_total, na.rm = TRUE)
  scale_factor <- max_college / max_dept
  
  # Create the dual axis plot
  college_dept_dual_plot <- ggplot(dual_axis_data, aes(x = `Academic Period Code`)) +
    # College bars (primary y-axis)
    geom_col(aes(y = college_total, fill = "College Total"), alpha = 0.7, width = 0.6) +
    # Department line (scaled to college axis)
    geom_line(aes(y = dept_total * scale_factor, color = paste0(d_params$dept_code, " Department")), 
              size = 2, group = 1) +
    geom_point(aes(y = dept_total * scale_factor, color = paste0(d_params$dept_code, " Department")), 
               size = 3) +
    # Primary y-axis (college)
    scale_y_continuous(
      name = "College Credit Hours",
      labels = scales::comma,
      sec.axis = sec_axis(~ . / scale_factor, 
                         name = paste0(d_params$dept_code, " Department Credit Hours"),
                         labels = scales::comma)
    ) +
    scale_fill_manual(values = c("College Total" = "#2E8B57")) +
    scale_color_manual(values = setNames("#FF6B35", paste0(d_params$dept_code, " Department"))) +
    labs(
      title = paste0("Credit Hours Comparison: AS College vs ", d_params$dept_code, " Department"),
      subtitle = "College bars (left axis) vs Department line (right axis)",
      x = "Academic Period Code",
      fill = "",
      color = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.title.y.left = element_text(color = "#2E8B57"),
      axis.title.y.right = element_text(color = "#FF6B35"),
      axis.text.y.left = element_text(color = "#2E8B57"),
      axis.text.y.right = element_text(color = "#FF6B35"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))
  
  d_params$plots[["college_dept_dual_plot"]] <- college_dept_dual_plot
  
  
  # since this is a unit report, filter by param$dept_code 
  # TODO: enable filter by subj code or program code
  
  credit_hours_data_main <- credit_hours_data %>%
    filter (DEPT == d_params$dept_code) %>% 
    filter (`Course Campus Code` %in% c("ABQ","EA")) %>% 
    arrange(`Academic Period Code`, `Course College Code`,DEPT,`Subject Code`,level)
  
  # create period_hours column - removing acad_year aggregation
  # credit_hours_data_main <- credit_hours_data_main %>% 
  #   group_by(`Academic Period Code`, `Course College Code`,DEPT,`Subject Code`,level) %>% 
  #   mutate (period_hours = sum(total_hours)) 
  
  # remove level totals so we can facet by level
  chm_by_subj_level <- credit_hours_data_main %>% filter (level != "total")
  
  # create plot line plot of CREDIT HOURS FACETED BY SUBJECT  
  chd_by_year_facet_subj_plot <- ggplot(chm_by_subj_level, aes(x=`Academic Period Code`, y=total_hours)) + 
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(aes(fill=level),stat="identity",position="stack") + 
    # geom_line(aes(group=level)) + 
    # geom_point(aes(group=`level`)) +
    scale_color_brewer(palette=d_params$palette) +
    facet_wrap(~`Subject Code`,ncol = 3) + 
    theme(axis.text.x=element_text(angle = 75, hjust = 1)) +
    xlab("Academic Period Code") + ylab("Credit Hours") 
  
  #chd_by_year_facet_subj_plot
  d_params$plots[["chd_by_year_facet_subj_plot"]] <- chd_by_year_facet_subj_plot
  
  
  
  # filter only totals to show TOTAL HOURS WITHOUT LEVELS
  chm_by_subj <- credit_hours_data_main %>% filter (level == "total")
  
  # create line plot of TOTAL CREDIT HOURS BY SUBJECT (no faceting)
  chd_by_year_subj_plot <- ggplot(chm_by_subj, aes(x=`Academic Period Code`, y=total_hours)) + 
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(aes(fill=`Subject Code`),stat="identity",position="stack") + 
    theme(axis.text.x=element_text(angle = 75, hjust = 1)) +
    
    # geom_point(aes(group=`Subject Code`)) + 
    # geom_line(aes(group=`Subject Code`)) + 
    xlab("Academic Period Code") + ylab("Credit Hours") 
  
  d_params$plots[["chd_by_year_subj_plot"]] <- chd_by_year_subj_plot
  
  # create tables for display
  chd_by_period_table <- chm_by_subj %>% mutate(level = factor(level, levels = unique(level))) %>% spread(key=level, value=total_hours)
  chd_by_period_table <- chd_by_period_table %>% ungroup() %>% 
    select(`Academic Period Code`,DEPT, 4:ncol(chd_by_period_table))
  d_params$tables[["chd_by_period_table"]] <- chd_by_period_table
  
  
  
  
  # create totals by academic period WITHOUT subject
  chd_by_period <- credit_hours_data_main %>% 
    group_by(`Academic Period Code`, `Course Campus Code`, `Course College Code`,DEPT,level) %>% filter (level != "total") %>% 
    mutate (period_hours = sum(total_hours)) %>% 
    arrange(`Academic Period Code`,`Course College Code`,DEPT,level)
  

  # create plot of STACKED BAR showing credit hours colored by CREDIT HOURS BY COURSE LEVEL
  chd_by_period_plot <- ggplot(chd_by_period, aes(x=`Academic Period Code`, y=total_hours)) + 
    ggtitle(paste0("using SUBJ codes: ",paste(d_params$subj_codes, collapse=", "))) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(aes(fill=level),stat="identity", position="stack") +
    theme(axis.text.x=element_text(angle = 75, hjust = 1)) +
    scale_fill_brewer(palette=d_params$palette) +
    xlab("Academic Period Code") + ylab("Credit Hours")
  
  chd_by_period_plot
  d_params$plots[["chd_by_period_plot"]] <- chd_by_period_plot
  
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
  
}




