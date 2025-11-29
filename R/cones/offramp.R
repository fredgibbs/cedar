#TODO: all these functions are effectively private and should be scoped in get_grades 




get_turn_off <- function(filtered_students) {

  # for studio testing
  opt <- list()
  opt$course <- "AFST 1110"
  students <- load_students()
  class_list <- students
  filtered_students <- filter_class_list(students,opt)

  
# 1. Define passing and failing grades
failing_grades <- c("C-", "D", "D-", "F", "W", "NC", "NCR", "NP") # adjust as needed 
passing_grades <- c("A+", "A", "A-", "B+", "B", "B-","C+","C", "CR", "P") # adjust as needed


# 2. For each student-course-term, flag as failed or passed
filtered_students <- filtered_students %>%
  mutate(
    outcome = case_when(
      `Final Grade` %in% failing_grades ~ "fail",
      `Final Grade` %in% passing_grades ~ "pass",
      is.na(`Final Grade`) ~ "fail"
    )
  )

# 3. Get unique student-term pairs for fail and pass
failed <- filtered_students %>%
  filter(outcome == "fail") %>%
  select(`Student ID`, `Academic Period Code`) %>%
  distinct() %>%
  mutate(group = "failed")

passed <- filtered_students %>%
  filter(outcome == "pass") %>%
  select(`Student ID`, `Academic Period Code`) %>%
  distinct() %>%
  mutate(group = "passed")

# 4. Combine for analysis
analysis_df <- bind_rows(failed, passed)

# 5. Add next term 
analysis_df <- analysis_df %>% add_next_term_col(`Academic Period Code`)

# 6. For each student, check if they registered for any course in the next term
next_term_enrollments <- class_list %>%
  select(`Student ID`, `Academic Period Code`) %>%
  distinct() %>%
  rename(next_term = `Academic Period Code`) %>%
  mutate(registered = 1)

analysis_df <- analysis_df %>%
  left_join(next_term_enrollments, by = c("Student ID", "next_term")) %>%
  mutate(registered_next = ifelse(is.na(registered), 0, 1))



# 7. Summarize: what percent registered in next term by group?
summary <- analysis_df %>%
  group_by(group) %>%
  summarize(
    n = n(),
    registered_next = sum(registered_next),
    pct_registered_next = registered_next / n * 100,
  )

print(summary)

# 8. Optional: Chi-squared test
table_for_test <- table(analysis_df$group, analysis_df$registered_next)
chisq.test(table_for_test)
}


#library(dplyr)
#library(purrr)
#library(broom) # for tidy() output from chisq.test


# for studio testing
# opt <- list()
# opt$course_college <- "AS"
# opt$level <- "lower"
# students <- load_students()
# class_list <- students
# filtered_students <- filter_class_list(students,opt)


get_turn_off_by_course <- function(filtered_students) {
  failing_grades <- c("C-", "D", "D-", "F", "W", "NC", "NCR", "NP")
  passing_grades <- c("A+", "A", "A-", "B+", "B", "B-","C+","C", "CR", "P")

  # Add outcome
  students <- filtered_students %>%
    mutate(
      outcome = case_when(
        `Final Grade` %in% failing_grades ~ "fail",
        `Final Grade` %in% passing_grades ~ "pass",
        is.na(`Final Grade`) ~ "fail"
      )
    )

  print(names(students))

  # For each course, do the analysis
  results <- students %>%
    group_by(SUBJ_CRSE) %>%
    group_map(~{
      course_data <- .
      # Get unique student-term pairs for fail and pass
      failed <- course_data %>%
        filter(outcome == "fail") %>%
        select(`Student ID`, `Academic Period Code`, SUBJ_CRSE) %>%
        distinct() %>%
        mutate(group = "failed")
      passed <- course_data %>%
        filter(outcome == "pass") %>%
        select(`Student ID`, `Academic Period Code`, SUBJ_CRSE) %>%
        distinct() %>%
        mutate(group = "passed")
      analysis_df <- bind_rows(failed, passed)
      # Add next term
      analysis_df <- add_next_term_col(analysis_df, "Academic Period Code")
      # Next term enrollments
      next_term_enrollments <- students %>% ungroup() %>%
        select(`Student ID`, `Academic Period Code`) %>%
        distinct() %>%
        rename(next_term = `Academic Period Code`) %>%
        mutate(registered = 1)
      analysis_df <- analysis_df %>%
        left_join(next_term_enrollments, by = c("Student ID", "next_term")) %>%
        mutate(registered_next = ifelse(is.na(registered), 0, 1))
      # Table and test
      tbl <- table(analysis_df$group, analysis_df$registered_next)
      # Only run test if both groups have both outcomes
      if (all(dim(tbl) == c(2,2))) {
        test <- chisq.test(tbl)
        tidy_test <- broom::tidy(test)
        tibble(
          SUBJ_CRSE = unique(course_data$SUBJ_CRSE),
          p.value = tidy_test$p.value,
          statistic = tidy_test$statistic,
          failed_stopout = mean(analysis_df$registered_next[analysis_df$group == "failed"] == 0, na.rm=TRUE),
          passed_stopout = mean(analysis_df$registered_next[analysis_df$group == "passed"] == 0, na.rm=TRUE),
          n_failed = sum(analysis_df$group == "failed"),
          n_passed = sum(analysis_df$group == "passed")
        )
      } else {
        tibble(
          SUBJ_CRSE = unique(course_data$SUBJ_CRSE),
          p.value = NA,
          statistic = NA,
          failed_stopout = NA,
          passed_stopout = NA,
          n_failed = sum(analysis_df$group == "failed"),
          n_passed = sum(analysis_df$group == "passed")
        )
      }
    }, .keep = TRUE) %>% bind_rows()

  # Optionally, filter for significant results
  results <- results %>% arrange(p.value)
  return(results)
}





# this function uses class lists to report all grades
get_all_grades <- function(filtered_students) {
  
  message("filtering out drops from students with grades...")
  filtered_students <- filtered_students %>% filter (substring(`Registration Status Code`,1,1) != "D") 
  
  # get distinct IDs in each course
  filtered_students <- filtered_students %>% distinct(`Student ID`,`Course Campus Code`,`Course College Code`,SUBJ_CRSE, .keep_all=TRUE)
  
  # calculate grade points from letter grade received and add col to student data 
  # grades_to_points is defined in mappings.R
  message("merging grade points...")
  grades <- merge(filtered_students,grades_to_points,by.x="Final Grade",by.y="grade",keep.all= T)
  
  return(grades)
}



# create DF of just students who dropped specified courses(s)
# DR codes omitted here, for student who drop before drop deadline 
get_dropped <- function(filtered_students) {
  drop_codes <- list("DG","DW","DD")
  dropped <- filtered_students %>% filter (`Registration Status Code` %in% drop_codes) 
  return(dropped)
}



# produce summary of grades (how many students got each grade) for specified courses(s)
# the output (grades_summary) is used to as input for additional aggregating and filtering
get_grades_summary <- function(grades) {
  message("getting grades summary...")
  
  grades_summary <- grades %>% group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE, level, `Long Course Title`, `Primary Instructor Last Name`, `Instruction Delivery Mode Code`, `Sub-Academic Period Code` , `Final Grade`, .groups="keep") %>% 
    summarize (count=n(), .groups="keep")
  
  # arranging by final grade helps the grades stay in order when pivoting to wide
  message("re-ordering grades...")
  grades_summary$`Final Grade` <- factor(grades_summary$`Final Grade`,levels=unlist(grades_to_points[1]))
  grades_summary <- grades_summary %>% arrange(`Final Grade`)
  
  # message("initial grades summary:")
  # grades_summary %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  return(grades_summary)
}


# create a summary of pass / fail / drop
# passing_grades defined in includes/lists.R
get_pf_sum <- function(grades,dropped) {
  message("creating summaries for passing, not passing, etc...")
  
  group_cols <- c("Course Campus Code", "Course College Code", "Academic Period Code", "SUBJ_CRSE", "level", "Primary Instructor Last Name","Instruction Delivery Mode Code", "Sub-Academic Period Code") 
  
  passed <- grades %>% filter (`Final Grade` %in% passing_grades) %>%  
    group_by_at(group_cols) %>% 
    summarize (passed=n(), .groups="keep")
  failed   <- grades %>% filter (!`Final Grade` %in% passing_grades) %>% 
    group_by_at(group_cols) %>% 
    summarize (failed=n(), .groups="keep")
  dropped_summary   <- dropped %>% 
    group_by_at(group_cols) %>% 
    summarize (dropped=n(), .groups="keep")
  
  # create row for each term/course/instructor/method/pt combo
  pf_sum <- merge(passed,failed,all="TRUE")
  pf_sum <- merge(pf_sum,dropped_summary,all="TRUE")
  
  # replace all NAs with 0s
  pf_sum <- pf_sum %>% mutate_if(is.numeric, ~replace_na(., 0))
  
  return(pf_sum)
}



# return a summary of pass / fail / drop / data by term and course
# this usually gets appended to full grade data
get_pf_sum_by_course <- function(pf_sum) {
  pf_sum_by_course <- pf_sum %>% group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE ) %>% 
    summarize (passed = sum(passed), failed = sum(failed), dropped = sum(dropped))

  return(pf_sum_by_course)
}



# produces a wide view of how many students got each grade BY COURSE, aggregating across all instructors
# so, there is one line for each course, with all outcomes as columns
get_grades_summary_by_course <- function(grades_summary,pf_sum_by_course) {
  message("getting grades summary by course...")
  
  # summarize WITHOUT INSTRUCTOR and WITHOUT COURSE TITLE (to group all topics titles together)
  # TODO: could add a grades_summary_by_course_topic if needed
  grades_summary_by_course  <- grades_summary %>% 
    group_by(`Course Campus Code`, `Course College Code`, `Academic Period Code`, SUBJ_CRSE, level,`Final Grade`) %>% 
    summarize (total = sum(count), .groups="keep")
  
  message("re-ordering grades...")
  grades_summary_by_course$`Final Grade` <- factor(grades_summary_by_course$`Final Grade`,levels=unlist(grades_to_points[1]))
  grades_summary_by_course <- grades_summary_by_course %>% arrange(`Final Grade`)
  
  # swing to wide
  grades_summary_by_course <- grades_summary_by_course %>% pivot_wider(names_from=`Final Grade`, values_from=total) %>% 
    arrange(`Academic Period Code`)
  
  # change NAs to zeros
  grades_summary_by_course <- grades_summary_by_course %>% mutate_if(is.numeric, ~replace_na(., 0))
  
  # merge with pass/fail rates
  grades_summary_by_course <- merge(grades_summary_by_course,pf_sum_by_course)
  
  # compute DFW %
  grades_summary_by_course <- grades_summary_by_course %>% 
    mutate (`DFW %`=round((dropped+failed)/(dropped+passed+failed)*100,digits=2), .after=`SUBJ_CRSE` ) %>% 
    arrange(`Course Campus Code`, `Course College Code`, `Academic Period Code`,SUBJ_CRSE)
  
  return(grades_summary_by_course)
}


# summarize grades takes a vector of cols to group by and summarizes 
summarize_grades <- function(grades_summary_by_course,opt) {
  group_cols <- opt[["group_cols"]]
  group_cols <- convert_param_to_list(group_cols)
  group_cols <- as.character(group_cols)
  
  summary <- grades_summary_by_course %>% 
    group_by_at(group_cols) %>% 
    summarize(passed = sum(passed), failed = sum(failed), dropped = sum(dropped), .groups="keep") %>% 
    mutate (`DFW %`=round((dropped+failed)/(passed+failed+dropped)*100,digits=2))
  
  return (summary)
}



# main controller for function, to be called from cedar or course report; dept report uses next function)
# no parameters required, but usually a course will be specified 
get_grades <- function(students,opt) {
  
  # for studio testing
  # opt <- list()
  # students <- load_students()
  # opt$course <- "AFST 1110"
  # opt$dept <- "HIST"
  # opt$term <- "202460"
  
  course <- opt[["course"]]
  message("procesing course: ",course)
  
  if (!is.null(course) && is.character(course)){
    # if course set to "forecasts", use list of courses already in forecast_table.
    # this is a good way to round out forecast data
    if (as.character(course) == "forecasts") {
      forecast_data <- load_forecasts()
      opt$course <- unique(as.list(forecast_data$SUBJ_CRSE))
      message("finished processing course as forecasts!")
    }
    # TODO: accept CSV file as input
  }
  
  # filter students from opt params (usually course and term OR dept for dept reports)
  filtered_students <- filter_class_list(students,opt)
  
  message("only using data since 2019, after Gen Ed implementation.")
  filtered_students <- filtered_students %>% filter (`Academic Period Code` >= 201980)
  
  message("setting Final Grade to `Drop` if registration status code is `DR`.")
  filtered_students <- filtered_students %>% mutate (`Final Grade` = ifelse(`Registration Status Code`=="DR", "Drop", `Final Grade`))
  
  # get all grades
  grades <- get_all_grades(filtered_students)
  
  # get LONG view of grades received in each course 
  grades_summary <- get_grades_summary(grades)
  
  # find students who dropped
  dropped <- get_dropped(filtered_students)
  
  # create pass/fail summary
  pf_sum <- get_pf_sum(grades,dropped)
  pf_sum_by_course <- get_pf_sum_by_course(pf_sum)
  
  # create wide view of all grades received for each course each term, incl sums of passed, failed, dropped
  # this gets passed to more specific aggregating functions specified by opt params
  grades_summary_by_course <- get_grades_summary_by_course(grades_summary,pf_sum_by_course)
  
  # check for old aggregate param
  if (!is.null(opt[["aggregate"]])) {
    message("WARNING: aggregate param is depricated. All aggregating done by default and returned in grades list.")
  }    
  
  # initialize grades list for return data
  grades <- list()
  
  # returns the basic grade summary by course, which is used for the other views
  # grades_summary_by_course is always called above for all functions, so we can just return it
  grades[["course"]] <- grades_summary_by_course

  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "Academic Period Code", "SUBJ_CRSE", "level")
  grades[["course_term"]] <- summarize_grades(grades_summary_by_course, opt)

  opt[["group_cols"]] <- c("Course Campus Code", "Course College Code", "SUBJ_CRSE", "level")
  grades[["course_avg"]] <- summarize_grades(grades_summary_by_course, opt)

  message("returning grades...")
  return (grades)  
}



# this is specifically for creating dept report outputs using d_params 
# it does additional filtering for lower division courses if available
get_grades_for_dept_report <- function(students,opt,d_params) {
  
  # studio testing set up  
  # opt <- list()
  # students <- load_students()

  # for plotting
  myopt <- opt
  myopt[["dept"]] <- d_params$dept_code 
  # myopt[["dept"]] <- "HIST"
  
  # limit to ABQ campus and online until we have better plotting across campuses
  message("limiting to ABQ and EA campus for plotting...")
  myopt[["course_campus"]] <- c("ABQ","EA")
  
  
  # get grades by each course and term for table in dept report
  myopt$aggregate <- "course"
  grades_summary_by_course <- get_grades(students,myopt)[["course"]]
  
  # filter for lower division, unless there aren't any (like MSST)
  grades_summary_by_course_ld <- grades_summary_by_course %>% filter (level == "lower")
  if (nrow(grades_summary_by_course_ld) == 0) {
    grades_summary_by_course_ld <- grades_summary_by_course
  }
  
  message("adding grades_summary_by_course_ld to d_params...")
  d_params$tables[["grades_summary_for_ld"]] <- grades_summary_by_course_ld 
  
  
  # get average grades for plot in dept report
  myopt$aggregate <- "course_avg"
  grades_summary_by_course_avg <- get_grades(students,myopt)[["course_avg"]]
  
  grades_summary_by_course_avg_ld <- grades_summary_by_course_avg %>% filter (level == "lower") 
  if (nrow(grades_summary_by_course_avg_ld) == 0) {
    grades_summary_by_course_avg_ld <- grades_summary_by_course_avg
  }
  
  grades_summary_by_course_avg_ld <- grades_summary_by_course_avg_ld %>% ungroup()
  
  # get instructor-level data for the same courses
  instructor_data <- grades_summary_by_course_ld %>%
    group_by(`Course Campus Code`, SUBJ_CRSE, `Primary Instructor Last Name`) %>%
    summarize(
      passed = sum(passed), 
      failed = sum(failed), 
      dropped = sum(dropped),
      .groups = "keep"
    ) %>%
    mutate(`DFW %` = round((dropped + failed) / (passed + failed + dropped) * 100, digits = 2)) %>%
    filter(!is.na(`Primary Instructor Last Name`) & `Primary Instructor Last Name` != "") %>%
    ungroup()
  
  grades_summary_for_ld_plot <- grades_summary_by_course_avg_ld %>% 
    mutate(SUBJ_CRSE = fct_reorder(SUBJ_CRSE, `DFW %`)) %>%
    ggplot(aes(y=SUBJ_CRSE, x=`DFW %`, fill=`Course Campus Code`)) + 
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity", position=position_dodge(), alpha=0.7) +
    geom_point(data = instructor_data %>% 
                 mutate(SUBJ_CRSE = factor(SUBJ_CRSE, levels = levels(grades_summary_by_course_avg_ld$SUBJ_CRSE))),
               aes(x=`DFW %`, y=SUBJ_CRSE, color=`Course Campus Code`), 
               position=position_jitter(height=0.2, width=0), 
               size=2, alpha=0.8) +
    ylab("Course") + xlab("mean DFW % (since 2019)")  +
    labs(caption = "Bars show course averages; dots show individual instructor averages")
  
  # grades_summary_for_ld_plot
  
  message("adding grades_summary_for_ld_abq_ea_plot to d_params...")
  d_params$plots[["grades_summary_for_ld_abq_ea_plot"]] <- grades_summary_for_ld_plot 

  
  message("returning d_params with new plot(s) and table(s)...")
  return(d_params)
}

