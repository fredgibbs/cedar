# This function loads and merges headcount data and faculty count data for use in various reports
get_perm_faculty_count <- function() {
  
  # load faculty headcount data
  # this file is created by parse-HRreport.R
  load(paste0(cedar_data_dir,"processed/fac_by_term.Rda")) # loads fac_by_term DF
  
  # sum appointment percentages
  fac_by_term_counts <- fac_by_term %>% group_by(term_code,DEPT,job_cat) %>% summarize(count = sum(`Appt %`/100))
  
  # only count permanent faculty in ratio calcs
  # job_cats are assigned in parse-HRreport.R
  perm_fac_count <- fac_by_term_counts %>% filter (job_cat %in% c("Professor","Lecturer","Associate Professor","Assistant Professor"))
  perm_fac_count <- perm_fac_count %>% group_by(term_code,DEPT) %>% summarize (count = sum(count))
  
  message("perm_fac_count:")
  perm_fac_count %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(perm_fac_count)
}


# This function calculates SFRs for dept reports
# Requires data/processed/fac_by_term.Rda, created in data/parsers/parse-HRreport.R
# Requires data/processed/academic-study.feather, created by data/parsers/parse-academic-study.R (from Academic Study Aggregate Guided Adhoc)
get_sfr <- function () {
  message("welcome to get_sfr!")
  
  message("calling headcount to count heads in college...")  
  headcount_all <- count_heads_in_college()
  
  perm_faculty_count <- get_perm_faculty_count()
  
  message("combining student and faculty tables...")
  studfac_ratios <- merge(headcount_all, perm_faculty_count, by.x=c("term_code","major_DEPT"), by.y=c("term_code","DEPT"),all.x=TRUE)
  
  message("studfac_ratios:")
  studfac_ratios %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # filter out summer
  message("filtering out summer...")
  studfac_ratios <- studfac_ratios %>% filter (!str_detect(as.character(term_code), "60"))
  
  # remove degrees
  # #TODO: maybe better to filter by A&S degrees to preserve that info for dept reports
  studfac_ratios <- studfac_ratios %>% 
    group_by(term_code,`Academic Year`,major_DEPT,`Student Level`,count) %>% 
    summarize(all_majors=sum(majors),all_minors=sum(minors))
  
  message("studfac_ratios summarized:")
  studfac_ratios %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # compute and add actual student faculty ratios
  message("computing ratios...")
  studfac_ratios$all_majors_sfr <- studfac_ratios$all_majors / studfac_ratios$count
  studfac_ratios$all_minors_sfr <- studfac_ratios$all_minors / studfac_ratios$count
  
  message("studfac_ratios:")
  studfac_ratios %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # get average of fall and spring headcounts to calc AY ratios
  studfac_ratios <- studfac_ratios %>% group_by(`Academic Year`,major_DEPT,`Student Level`) %>% 
    mutate(majors_sfr_ay=mean(all_majors_sfr),minors_sfr_ay=mean(all_minors_sfr))
  
  message("studfac_ratios averaged over academic year:")
  studfac_ratios %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(studfac_ratios)
}


# This function is called from dept_report.R 
# Creates plots for department reports.
get_sfr_data_for_dept_report <- function(d_params) {
  
  studfac_ratios <- get_sfr()
  
  # long view for easier plotting
  studfac_ratios_long <- studfac_ratios %>%
    pivot_longer(
      cols = ends_with("sfr_ay"),
      names_to = "type",
      values_to = "ratio",
      values_drop_na = TRUE
    )
  
  message("studfac_ratios_long:")
  studfac_ratios_long %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  

  # filter by UNDERGRADUATE and DEPT
  ug_sfr <- studfac_ratios_long %>% 
    filter(`Student Level` == "Undergraduate") %>% 
    filter(major_DEPT == d_params[["dept_code"]])
  
  if (nrow(ug_sfr) > 0) {
    ug_sfr_plot <- ggplot(ug_sfr, aes(x=`Academic Year`)) +
      #ggtitle(paste(params["dept"], "-", params["program_str"])) +
      #ggtitle(sfr_dept_title) +
      guides(color = guide_legend(title = "")) +
      theme(legend.position="bottom") +
      labs(fill="",color="Comparison") +
      #scale_x_discrete(breaks=num.labs,labels=term.labs) +
      geom_bar(aes(y=ratio, fill=type), stat="identity", position="dodge") +
      xlab("Academic Year") + ylab("Students per Faculty Member")
  } else {ug_sfr_plot <- "Insufficient Data"}
  
  d_params$plots[["ug_sfr_plot"]] <- ug_sfr_plot
  
  
  # filter by GRADUATE and DEPT
  grad_sfr <- studfac_ratios_long %>% 
    filter(`Student Level` == "Graduate/GASM") %>% 
    filter(major_DEPT == d_params[["dept_code"]])
  
  # plot faculty ratio as grouped bars for grad and undergrad
  if (nrow(grad_sfr) > 0) {
    grad_sfr_plot <- ggplot(grad_sfr, aes(x=`Academic Year`)) +
      #ggtitle(paste(params["dept"], "-", params["program_str"])) +
      #ggtitle(sfr_dept_title) +
      guides(color = guide_legend(title = "")) +
      theme(legend.position="bottom") +
      #labs(fill="",color="Comparison") +
      #scale_x_discrete(breaks=num.labs,labels=term.labs) +
      geom_bar(aes(y=ratio, fill=type), stat="identity", position="dodge") +
      xlab("Academic Year") + ylab("Students per Faculty Member")
  } else {grad_sfr_plot <- "Insufficient Data"}
  
  d_params$plots[["grad_sfr_plot"]] <- grad_sfr_plot
  
  
  # plot SFRs in college context
  # get sfrs for majors
  sfr_college <- studfac_ratios_long %>%
    filter(`Student Level` == "Undergraduate") %>% 
    filter(type == "majors_sfr_ay")
  
  # filter to highlight dept
  sfr_college_dept <- sfr_college %>%
    filter(major_DEPT == d_params$dept_code)
  
  # scatter plot to see dept in context of college for current semester
  if (nrow(sfr_college_dept) > 0) {
    sfr_scatterplot <- ggplot(sfr_college, aes(x=`Academic Year`, y=ratio)) +
      theme(legend.position="bottom") +
      guides(color = guide_legend(title = "",color="")) +
      geom_point(alpha=.5) +
      geom_line(alpha=.2,aes(group=major_DEPT)) +
      geom_point(sfr_college_dept, mapping=aes(x=`Academic Year`, y=ratio, color=major_DEPT)) +
      geom_line(sfr_college_dept, mapping=aes(x=`Academic Year`, y=ratio, color=major_DEPT, group=major_DEPT)) +
      xlab("Semester") + ylab("Students per Faculty")
    
    if (d_params$dept_code != "PSYC") {
      sfr_scatterplot <- sfr_scatterplot +
        coord_cartesian(
          ylim = c(0,50)
        )
      
    } 
  } else {sfr_scatterplot <- "Insufficient HR data"}
  
  # sfr_scatterplot
  d_params$plots[["sfr_scatterplot"]] <- sfr_scatterplot
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}
