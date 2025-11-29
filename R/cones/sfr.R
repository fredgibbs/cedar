# This function loads and merges headcount data and faculty count data for use in various reports
get_perm_faculty_count <- function(hr_data) {
    
  # Check if we have HR data
  if (is.null(hr_data) || nrow(hr_data) == 0) {
    message("[sfr.R] No hr_data found in data_objects or hr_data is empty")
    return(NULL)
  }

  message("[sfr.R] hr_data has", nrow(hr_data), "rows")
  message("[sfr.R] hr_data columns:", paste(colnames(hr_data), collapse=", "))

  # Check for required columns
  required_cols <- c("term_code", "DEPT", "job_cat", "Appt %")
  missing_cols <- required_cols[!required_cols %in% colnames(hr_data)]
  if (length(missing_cols) > 0) {
    message("[sfr.R] ERROR: Missing required columns:", paste(missing_cols, collapse=", "))
    return(NULL)
  }
  
  # sum appointment percentages
  fac_by_term_counts <- hr_data %>% 
    group_by(term_code, DEPT, job_cat) %>% 
    summarize(count = sum(`Appt %`/100, na.rm = TRUE), .groups = "drop")
  
  # only count permanent faculty in ratio calcs
  # job_cats are assigned in parse-HRreport.R
  perm_fac_count <- fac_by_term_counts %>% 
    filter(job_cat %in% c("Professor","Lecturer","Associate Professor","Assistant Professor"))
  
    message("[sfr.R] After filtering for permanent faculty:", nrow(perm_fac_count), "rows")
  
  if (nrow(perm_fac_count) == 0) {
    message("[sfr.R] ERRO R: No permanent faculty found after filtering")
    message("[sfr.R] Available job_cat values:", paste(unique(fac_by_term_counts$job_cat), collapse=", "))
    return(NULL)
  }
  
  perm_fac_count <- perm_fac_count %>% 
    group_by(term_code, DEPT) %>% 
    summarize(total = sum(count, na.rm = TRUE), .groups = "drop")

  message("[sfr.R] Returning perm_fac_count with ", nrow(perm_fac_count), " rows")

  return(perm_fac_count)
}


# This function calculates SFRs for dept reports
# Requires data/processed/fac_by_term.Rda, created in data/parsers/parse-HRreport.R
# Requires data/processed/academic-study.feather, created by data/parsers/parse-academic-study.R (from Academic Study Aggregate Guided Adhoc)
get_sfr <- function (data_objects) {
  message("[sfr.R] welcome to get_sfr!")

  academic_studies_data <- data_objects[["academic_studies"]]
  

  message("[sfr.R] calling headcount to count heads...")
  headcount_all <- count_heads(academic_studies_data, opt=list())

  if (is.null(headcount_all) || nrow(headcount_all) == 0) {
    message("[sfr.R] ERROR: No headcount data returned from count_heads()")
    return(NULL)
  }

  message("[sfr.R] headcount_all has", nrow(headcount_all), "rows")

  # TODO: count only majors or both majors and minors?
  # some programs have lots of minors that should be counted 
  # allowed_types <- c("Major")
  # headcount_all <- headcount_all %>% filter(major_type %in% allowed_types)

  message("[sfr.R] getting permanent faculty headcount...")
  perm_faculty_count <- get_perm_faculty_count(data_objects[["hr_data"]])
  
  if (is.null(perm_faculty_count) || nrow(perm_faculty_count) == 0) {
    message("[sfr.R] ERROR: No permanent faculty count data returned")
    return(NULL)
  }

  message("[sfr.R] perm_faculty_count has", nrow(perm_faculty_count), "rows")

  message("[sfr.R] merging student and faculty tables...")
  studfac_ratios <- merge(headcount_all, perm_faculty_count, 
                         by.x=c("term_code","DEPT"), 
                         by.y=c("term_code","DEPT"), 
                         all.x=TRUE)

  message("[sfr.R] After merge, studfac_ratios has", nrow(studfac_ratios), "rows")

  # filter out summer, which is meaningless for sfr purposes
  message("[sfr.R] filtering out summer for sfr purposes...")
  studfac_ratios <- studfac_ratios %>% filter (!str_detect(as.character(term_code), "60"))

  message("[sfr.R] After filtering summer, studfac_ratios has", nrow(studfac_ratios), "rows")

  if (nrow(studfac_ratios) == 0) {
    message("[sfr.R] ERROR: No data after filtering out summer terms")
    return(NULL)
  }
  
  # calc sums of majors and minors
  studfac_ratios <- studfac_ratios %>% 
    group_by(term_code, DEPT, `Student Level`, major_name, PRGM, total)
  
  # separate majors
  majors <- studfac_ratios %>% filter (major_type %in% c("Major","Second Major")) 
  majors <- majors %>%  summarize(major_type="all_majors", students = sum(students, na.rm = TRUE), .groups = "drop")
  message("[sfr.R] Majors data has", nrow(majors), "rows")
  
  # separate minors
  minors <- studfac_ratios %>% filter (major_type %in% c("First Minor","Second Minor")) 
  minors <- minors %>%  summarize(major_type="all_minors", students = sum(students, na.rm = TRUE), .groups = "drop")
  message("[sfr.R] Minors data has", nrow(minors), "rows")

  # combine majors and minors
  studfac_ratios <- rbind(majors,minors)

  message("[sfr.R] Combined majors/minors has", nrow(studfac_ratios), "rows")

  if (nrow(studfac_ratios) == 0) {
    message("[sfr.R] ERROR: No data after combining majors and minors")
    return(NULL)
  }
  
  # compute SFRs
  message("[sfr.R] computing studfac_ratios...")
  studfac_ratios <- studfac_ratios %>% group_by(term_code, DEPT,`Student Level`,major_type) %>% arrange(term_code,major_name,`Student Level`,major_type)
  studfac_ratios <- studfac_ratios %>% mutate(sfr = students / total)

  message("[sfr.R] Returning studfac_ratios")
  return(studfac_ratios)
}


# This function is called from dept_report.R 
# Creates plots for department reports.
get_sfr_data_for_dept_report <- function(data_objects, d_params) {
  message("[sfr.R] Welcome to Starting get_sfr_data_for_dept_report!")

  studfac_ratios <- get_sfr(data_objects)

  if (is.null(studfac_ratios) || nrow(studfac_ratios) == 0) {
    message("ERROR: No SFR data returned from get_sfr()")
    d_params$plots[["ug_sfr_plot"]] <- "No SFR Data Available"
    d_params$plots[["grad_sfr_plot"]] <- "No SFR Data Available"
    d_params$plots[["sfr_scatterplot"]] <- "No SFR Data Available"
    return(d_params)
  }

  message("[sfr.R] studfac_ratios has", nrow(studfac_ratios), "rows for dept report")

  # filter by UNDERGRADUATE and DEPT
  ug_sfr <- studfac_ratios %>% 
    filter(`Student Level` == "Undergraduate") %>% 
    filter(DEPT == d_params[["dept_code"]])

  message("[sfr.R] Undergraduate SFR data for dept", d_params[["dept_code"]], "has", nrow(ug_sfr), "rows")

  if (nrow(ug_sfr) > 0) {
    ug_sfr_plot <- ggplot(ug_sfr, aes(x=term_code)) +
      #ggtitle(paste(params["dept"], "-", params["program_str"])) +
      #ggtitle(sfr_dept_title) +
      guides(color = guide_legend(title = "")) +
      theme(legend.position="bottom") +
      labs(fill="",color="Comparison") +
      #scale_x_discrete(breaks=num.labs,labels=term.labs) +
      geom_bar(aes(y=sfr, fill=major_type), stat="identity", position="dodge") +
      xlab("Term") + ylab("Students per Faculty Member")
  } else {ug_sfr_plot <- "Insufficient Data"}
  
  ug_sfr_plot
  d_params$plots[["ug_sfr_plot"]] <- ug_sfr_plot
  
  
  # filter by GRADUATE and DEPT
  grad_sfr <- studfac_ratios %>% 
    filter(`Student Level` == "Graduate/GASM") %>% 
    filter(DEPT == d_params[["dept_code"]])
  
  # plot faculty ratio as grouped bars for grad and undergrad
  if (nrow(grad_sfr) > 0) {
    grad_sfr_plot <- ggplot(grad_sfr, aes(x=term_code)) +
      #ggtitle(paste(params["dept"], "-", params["program_str"])) +
      #ggtitle(sfr_dept_title) +
      guides(color = guide_legend(title = "")) +
      theme(legend.position="bottom") +
      #labs(fill="",color="Comparison") +
      #scale_x_discrete(breaks=num.labs,labels=term.labs) +
      geom_bar(aes(y=sfr, fill=major_type), stat="identity", position="dodge") +
      xlab("Term") + ylab("Students per Faculty Member")
  } else {grad_sfr_plot <- "Insufficient Data"}
  
  d_params$plots[["grad_sfr_plot"]] <- grad_sfr_plot
  
  
  # plot SFRs in college context
  # get sfrs for majors
  sfr_college <- studfac_ratios %>%
    filter(`Student Level` == "Undergraduate") %>% 
    filter(major_type == "all_majors")
  
  # until there is better college-level sorting, remove rows with NAs for DEPT (meaning non-AS in mappings)
  sfr_college <- sfr_college[!is.na(sfr_college$DEPT),]
  
  # filter by DEPT code to highlight dept in college context
  sfr_college_dept <- sfr_college %>%
    filter(DEPT == d_params$dept_code)
  
  # compress all college sfrs by dept (lose program info for simplicity)
  sfr_college <- sfr_college %>%
    ungroup() %>% group_by(term_code,DEPT,total) %>% 
    mutate (all_students = sum(students), sfr=all_students/total) %>% 
    distinct()
  
  
  # scatter plot to see dept in context of college for current semester
  if (nrow(sfr_college_dept) > 0) {
    sfr_scatterplot <- ggplot(sfr_college, aes(x=`term_code`, y=sfr)) +
      theme(legend.position="bottom") +
      guides(color = guide_legend(title = "",color="")) +
      geom_point(alpha=.5) +
      geom_line(alpha=.2,aes(group=DEPT)) +
      geom_point(sfr_college_dept, mapping=aes(x=`term_code`, y=sfr, color=major_name)) +
      geom_line(sfr_college_dept, mapping=aes(x=`term_code`, y=sfr, color=major_name, group=major_name)) +
      xlab("Semester") + ylab("Students per Faculty")
    
    if (d_params$dept_code != "PSYC") {
      sfr_scatterplot <- sfr_scatterplot +
        coord_cartesian(
          ylim = c(0,50)
        )
      
    } 
  } else {sfr_scatterplot <- "Insufficient HR data"}
  
  sfr_scatterplot
  d_params$plots[["sfr_scatterplot"]] <- sfr_scatterplot

  message("[sfr.R] returning d_params with new plot(s) and table(s)...")
  return(d_params)
}
