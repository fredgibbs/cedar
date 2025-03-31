# This function loads and merges headcount data and faculty count data for use in various reports
get_perm_faculty_count <- function() {
  
  # load faculty headcount data
  # this file is created by parse-HRreport.R
  #load(paste0(cedar_data_dir,"processed/fac_by_term.Rda")) # loads fac_by_term DF
  fac_by_term <- load_hr_data()
    
  
  # sum appointment percentages
  fac_by_term_counts <- fac_by_term %>% group_by(term_code,DEPT,job_cat) %>% 
    summarize(count = sum(`Appt %`/100))
  
  # only count permanent faculty in ratio calcs
  # job_cats are assigned in parse-HRreport.R
  perm_fac_count <- fac_by_term_counts %>% filter (job_cat %in% c("Professor","Lecturer","Associate Professor","Assistant Professor"))
  perm_fac_count <- perm_fac_count %>% group_by(term_code,DEPT) %>% 
    summarize (count = sum(count))
  
  return(perm_fac_count)
}


# This function calculates SFRs for dept reports
# Requires data/processed/fac_by_term.Rda, created in data/parsers/parse-HRreport.R
# Requires data/processed/academic-study.feather, created by data/parsers/parse-academic-study.R (from Academic Study Aggregate Guided Adhoc)
get_sfr <- function () {
  message("welcome to get_sfr!")
  
  message("calling headcount to count heads in college...")  
  headcount_all <- count_heads_in_college(opt=list())
  
  # TODO: count only majors or both majors and minors?
  # some programs have lots of minors that should be counted 
  # allowed_types <- c("Major")
  # headcount_all <- headcount_all %>% filter(major_type %in% allowed_types)
  
  message("getting permanent faculty headcount...")  
  perm_faculty_count <- get_perm_faculty_count()
  
  message("combining student and faculty tables...")
  studfac_ratios <- merge(headcount_all, perm_faculty_count, by.x=c("term_code","DEPT"), by.y=c("term_code","DEPT"),all.x=TRUE)
  
  message("studfac_ratios:")
  studfac_ratios %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # filter out summer, which is meaningless for sfr purposes
  message("filtering out summer...")
  studfac_ratios <- studfac_ratios %>% filter (!str_detect(as.character(term_code), "60"))
  
  # calc sums of majors and minors
  studfac_ratios <- studfac_ratios %>% 
    group_by(term_code,`Academic Year`,DEPT,`Student Level`,major_name, PRGM, count)
  majors <- studfac_ratios %>% filter (major_type %in% c("Major","Second Major")) 
  #majors <- majors %>%  mutate(major_type = major_type, all_majors = sum(students))
  majors <- majors %>%  summarize(major_type="all_majors", students = sum(students))
  
  majors %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  
  minors <- studfac_ratios %>% filter (major_type %in% c("First Minor","Second Minor")) 
  #minors <- minors %>% mutate(major_type = major_type, all_minors = sum(students))
  minors <- minors %>%  summarize(major_type="all_minors", students = sum(students))
  
  minors %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  studfac_ratios <- rbind(majors,minors)
  #studfac_ratios <- merge(majors,minors, by=c("term_code","DEPT","Academic Year","Student Level","major_name","PRGM","count"))
  
  message("studfac_ratios summarized:")
  studfac_ratios <- studfac_ratios %>% group_by(term_code, DEPT,`Student Level`,major_type) %>% arrange(term_code,major_name,`Student Level`,major_type)
  studfac_ratios %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # compute and add actual student faculty ratios
  message("studfac_ratios:")
  #studfac_ratios$all_majors_sfr <- studfac_ratios$all_majors / studfac_ratios$count
  studfac_ratios$sfr <- studfac_ratios$students / studfac_ratios$count
  studfac_ratios %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # get mean of fall and spring headcounts to calc AY ratios
  # studfac_ratios <- studfac_ratios %>% group_by(`Academic Year`,DEPT,`Student Level`) %>% 
    #mutate(sfr_ay=mean(sfr))

  return(studfac_ratios)
}


# This function is called from dept_report.R 
# Creates plots for department reports.
get_sfr_data_for_dept_report <- function(d_params) {
  
  studfac_ratios <- get_sfr()
  
  # filter by UNDERGRADUATE and DEPT
  ug_sfr <- studfac_ratios %>% 
    filter(`Student Level` == "Undergraduate") %>% 
    filter(DEPT == d_params[["dept_code"]])
  
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
  
  # until there is better college-level sorting, remove NAs
  sfr_college <- sfr_college[!is.na(sfr_college$DEPT),]
  
  # filter by DEPT code to highlight dept in college context
  sfr_college_dept <- sfr_college %>%
    filter(DEPT == d_params$dept_code)
  
  # compress all college sfrs by dept (lose program info for simplicity)
  sfr_college <- sfr_college %>%
    ungroup() %>% group_by(term_code,DEPT,count) %>% 
    summarize (all_students = sum(students), sfr=all_students/count) %>% 
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
  
  # sfr_scatterplot
  d_params$plots[["sfr_scatterplot"]] <- sfr_scatterplot
  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}
