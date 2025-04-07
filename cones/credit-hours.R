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
  message("welcome to get_credit_hours!")
  
  message("filtering students by passing_grades...")
  filtered_students <- students %>%  filter(`Final Grade` %in% passing_grades)
  
  message("summarizing...")
  filtered_students_summary <- filtered_students %>% 
    group_by(`Academic Period Code`,`Course Campus Code`, `Course College Code`,DEPT,level, `Subject Code`) %>% 
    summarize(total_hours = sum(`Course Credits`), .groups="keep")
  
  message("creating totals across levels...")
  credit_hours_totals <- filtered_students %>% group_by(`Academic Period Code`,`Course Campus Code`,`Course College Code`,DEPT,`Subject Code`) %>% 
    summarize(level="total",total_hours = sum(`Course Credits`), .groups="keep")
  
  message("adding totals to level data...")
  credit_hours_data <- rbind(filtered_students_summary,credit_hours_totals) %>%  
    arrange(`Academic Period Code`,`Course Campus Code`,`Course College Code`,DEPT,`Subject Code`, factor(level,levels=c("lower","upper","grad","total")))
  
  # add academic year col
  credit_hours_data <- add_acad_year(credit_hours_data,"Academic Period Code")
  
  return(credit_hours_data)
}


# produces two CSV files:
# - credit hours by college, dept, level, and term
# - credit hours by dept and academic year (also saves a PNG plot of this data)
save_credit_hours <- function(credit_hours_data,opt) {
  
  # prepare filename string
  if (!is.null(opt$dept)) {
    label <- paste0(opt$dept,"-",opt$term)
  }  else {
    label <- paste0("all-depts-",opt$term)
  }
  
  output_file_prefix <- paste0(cedar_output_dir,"credit-hours/") 
  
  # save file
  filename <- paste0(output_file_prefix,label,"-credit-hours.csv")
  message("saving ",filename,"...")
  write.csv(credit_hours_data, filename, row.names=FALSE)
  
  # make more useful as chart and table by selecting just totals for academic years
  chd_by_year_dept_total <- credit_hours_data  # %>% filter(level == "total") 
  
  # summarize on acad_year to reduce period codes
  message("creating totals for academic years...")
  chd_by_year_dept_total <- chd_by_year_dept_total %>% group_by(`acad_year`,`Course College Code`,DEPT,level) %>% 
    summarize(acad_year_hours = sum(total_hours))

  # make wide for easier human scanning...
  chd_by_year_dept_total_w <- chd_by_year_dept_total %>% spread(acad_year, acad_year_hours)

  # more intuitive sorting
  chd_by_year_dept_total_w <- chd_by_year_dept_total_w %>% arrange(factor(level,levels=c("lower","upper","grad","total")))
  
  # print for inspection
  chd_by_year_dept_total_w %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  # save CSV file
  message("saving hours_by_year_dept_total.csv...")
  write.csv(chd_by_year_dept_total_w, paste0(output_file_prefix,"hours_by_year_dept_total.csv"), row.names=FALSE)
  
  # create plot
  chd_by_year_dept_total_plot <- ggplot(chd_by_year_dept_total, aes(x=acad_year, y=acad_year_hours, col=DEPT,group=DEPT)) + 
    ggtitle("Earned Credit Hours by Dept and Academic Year") +
    theme(legend.position="bottom") +
    geom_line(alpha=.2) + 
    geom_point(alpha=.1) +
    geom_smooth(method=lm, se=FALSE) +
    xlab("Academic Year") + ylab("Credit Hours") +
    theme(axis.text.x = element_text(angle = 30, hjust=1, vjust=.8)) 
  
  message("saving hours_by_year_dept_total.png...")
  ggsave(paste0(output_file_prefix,"/hours_by_year_dept_total.png"), width = 7, height = 9, dpi = 300, units = "in")
  
} # end save_credit_hours



# parse earned credit hours by major
# use class lists to look at who got passing grades and number of credits registered for
# tally these up to get credit hours for a subset of students
# called from dept-report.R
credit_hours_by_major <- function (students,d_params) {
  
  # general idea:
  # get ids from students in DEPT courses
  # merge with headcount to get all student data, like  majors, minors, second majors, second minors
  
  message("filtering students by d_params...")
  filtered_students <- students %>% filter(`DEPT` == d_params$dept_code)
  filtered_students <- filtered_students %>% filter (as.integer(`Academic Period Code`) >= d_params$term_start & as.integer(`Academic Period Code`) <= d_params$term_end) 
  
  
  # filter out non credit earning students; passing_grades defined in includes/map_to_subj_code.R
  message("filtering students by passing grades...")
  filtered_students <- filtered_students %>%  filter(`Final Grade` %in% passing_grades)
  
  # remove Pre from major and add boolean flag in separate column
  message("integrating pre-majors...")
  filtered_students$pre <- ifelse ( grepl("Pre", filtered_students$Major),TRUE,FALSE)
  filtered_students$Major <- str_remove(filtered_students$Major, "Pre ")
  filtered_students$Major <- str_remove(filtered_students$Major, "Pre-")
  
  message("standardizing data ...")
  filtered_students$`Student College` <- str_replace(filtered_students$`Student College`, "College of Educ & Human Sci", "College of Education")
  filtered_students <- add_acad_year(filtered_students,"Academic Period Code")
  
  
  # find students in SUBJ courses who are NOT majoring (first) in any programs of that dept
  #prgm_to_dept_map[[major_to_program_map[["East Asian Studies"]]]]
  #message("finding non-majors ...")
  
  # create col to indicate "home" program of student based on Major col
  #filtered_students$student_major_dept <-  prgm_to_dept_map[major_to_program_map[filtered_students$Major]]
  #non_majors <- filtered_students %>% filter (is.na(student_major_dept) | student_major_dept != opt$dept)
  #non_majors <- filtered_students %>% filter (prgm_to_dept_map[major_to_program_map[Major]] != opt$dept)
  
  # summarize by academic year, student's home college, and major
  credit_hours_data <- filtered_students %>% 
    group_by(`acad_year`,`Student College`,Major) %>% 
    summarize(total_hours = sum(`Course Credits`)) %>% 
    arrange (acad_year,desc(total_hours))
  
  # create wide view with years as columns
  credit_hours_data_w <- credit_hours_data %>% pivot_wider(names_from = acad_year, values_from = total_hours)
  
  # create totals line
  credit_hours_data_w <- credit_hours_data_w %>%  ungroup() %>% 
    rbind(c(`Student College`= "Total", Major= "Total", colSums(credit_hours_data_w[,3:ncol(credit_hours_data_w)], na.rm = TRUE)))
  
  credit_hours_data_w <- credit_hours_data_w %>% mutate_at(c(3:ncol(credit_hours_data_w)), as.numeric)
  credit_hours_data_w[is.na(credit_hours_data_w)] <- 0
  credit_hours_data_w <- credit_hours_data_w %>% arrange(desc(`2022-2023`))
  credit_hours_data_w <- credit_hours_data_w %>% arrange(desc(names(credit_hours_data_w)[8])) 
  
  # save data for report
  d_params$tables[["credit_hours_data_w"]] <- credit_hours_data_w 
  
  
  # prep data for pie chart for NON-MAJORS
  message("finding non-majors' share of total credit hours ...")
  sch_outside_pct <- credit_hours_data_w %>% filter (!(Major %in% d_params$prog_names))
  sch_outside_pct <- sch_outside_pct %>% filter (Major != "Total")  %>%  slice_head(n=9)
  
  # reorder Majors in order of credit hours
  sch_outside_pct <- sch_outside_pct %>%  mutate(Major = fct_reorder(Major, `2022-2023`))
  
  message("creating plot...")
  sch_outside_pct_plot <- plot_ly(sch_outside_pct, labels = ~Major, values = ~`2022-2023`, marker = list(colorscale = 'Viridis'))
  sch_outside_pct_plot <- sch_outside_pct_plot %>%
    add_pie(hole = 0.6) %>% 
    layout(title = '',
           legend = list(x = 100, y = 0.5),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  sch_outside_pct_plot
  
  message("saving plot in d_params...")
  d_params$plots[["sch_outside_pct_plot"]] <- sch_outside_pct_plot
  
  
  # prep data for pie chart of DEPT HOURS COMPARED AS PART OF TOTAL
  # this is way faster than making the filter an OR test
  message("finding majors' share of total credit hours ...")
  sch_dept_pct_total <- credit_hours_data_w %>% filter (Major == "Total") %>% mutate (Major = "Non-majors")
  sch_dept_pct_program <- credit_hours_data_w %>% filter (Major %in% d_params$prog_names)
  sch_dept_pct <- rbind(sch_dept_pct_total,sch_dept_pct_program)
  
  message("creating plot...")
  
  sch_dept_pct_plot <- plot_ly(sch_dept_pct, labels = ~Major, values = ~`2022-2023`, type = 'pie', marker = list(colorscale = 'Viridis'))
  sch_dept_pct_plot <- sch_dept_pct_plot %>% 
    layout(legend = list(x = 100, y = 0.5),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  sch_dept_pct_plot
  message("saving plot...")
  #save(sch_dept_pct_plot, file="credit-hours/sch_dept_pct_plot.Rda")
  d_params$plots[["sch_dept_pct_plot"]] <- sch_dept_pct_plot
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}


# this function generates raw student credit hour data by faculty and graph for use in dept-reports
# called from dept-report.R
credit_hours_by_fac <- function (students, d_params) {
  message("\nWelcome to credit_hours_by_fac!")
  
  # for studio testing
  # d_params <- list(dept_code="CJ",subj_codes=c("CJ","COMM"),palette="Spectral")
  
  # filter out non credit earning students; passing_grades defined in includes/map_to_subj_code.R
  filtered_students <- students %>%  filter(`Final Grade` %in% passing_grades & DEPT == d_params$dept_code)
  
  # filter for term params
  filtered_students <- filtered_students %>%  filter (`Academic Period Code` >= d_params$term_start &
                                                      `Academic Period Code` <= d_params$term_end)
  
  
  message("loading and merging faculty data with course lists...")
  fac_by_term <- load_hr_data()
  fac_by_term <- fac_by_term %>% select(-c("as_of_date"))
  merged <- merge(filtered_students,fac_by_term,by.x=c("Academic Period Code","Primary Instructor ID","DEPT"),by.y=c("term_code","UNM ID","DEPT"),x.all=TRUE)
  
  # summarize total hours earned by job_cat (in faculty data)
  credit_hours_data <- merged %>% 
    group_by(`Academic Period Code`, `Course Campus Code`, `Course College Code`, DEPT, level, job_cat) %>% 
    summarize(total_hours = sum(`Course Credits`), .groups="keep")
  
  
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
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
  
} # end credit_hours_by_fac


# called from dept-report.R
get_credit_hours_for_dept_report <- function (students,d_params) {
  message("\nWelcome to get_credit_hours_for_dept_report!")
  
  # get basic credit hours data
  credit_hours_data <- get_credit_hours(students)
  
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
  
  
  
  # since this is a unit report, filter by param$dept_code 
  # TODO: enable filter by subj code or program code
  
  credit_hours_data_main <- credit_hours_data %>%
    filter (DEPT == d_params$dept_code) %>% 
    filter (`Course Campus Code` %in% c("ABQ","EA")) %>% 
    arrange(`Academic Period Code`, `Course College Code`,DEPT,`Subject Code`,level)
  
  # create acad_year_hours column
  credit_hours_data_main <- credit_hours_data_main %>% 
    group_by(acad_year, `Course College Code`,DEPT,`Subject Code`,level) %>% 
    mutate (acad_year_hours = sum(total_hours)) 
  
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
    xlab("Academic Year") + ylab("Credit Hours") 
  
  chd_by_year_facet_subj_plot
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
    xlab("Academic Year") + ylab("Credit Hours") 
  
  chd_by_year_subj_plot
  d_params$plots[["chd_by_year_subj_plot"]] <- chd_by_year_subj_plot
  
  # create tables for display
  chd_by_year_table <- chm_by_subj %>% mutate(level = factor(level, levels = unique(level))) %>% spread(key=level, value=acad_year_hours)
  chd_by_year_table <- chd_by_year_table %>% ungroup() %>% 
    select(acad_year,DEPT, 4:ncol(chd_by_year_table))
  d_params$tables[["chd_by_year_table"]] <- chd_by_year_table
  
  
  
  
  # create totals by academic year WITHOUT subject
  chd_by_year <- credit_hours_data_main %>% 
    group_by(acad_year, `Course Campus Code`, `Course College Code`,DEPT,level) %>% filter (level != "total") %>% 
    mutate (acad_year_hours = sum(total_hours)) %>% 
    arrange(acad_year,`Course College Code`,DEPT,level)
  

  # create plot of STACKED BAR showing credit hours colored by CREDIT HOURS BY COURSE LEVEL
  chd_by_year_plot <- ggplot(chd_by_year, aes(x=`Academic Period Code`, y=total_hours)) + 
    ggtitle(paste0("using SUBJ codes: ",paste(d_params$subj_codes, collapse=", "))) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(aes(fill=level),stat="identity", position="stack") +
    theme(axis.text.x=element_text(angle = 75, hjust = 1)) +
    scale_fill_brewer(palette=d_params$palette) +
    xlab("Academic Period") + ylab("Credit Hours")
  
  chd_by_year_plot
  d_params$plots[["chd_by_year_plot"]] <- chd_by_year_plot
  
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
  
}




