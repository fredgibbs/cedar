#' Calculate Course-Level Enrollment Statistics
#'
#' This function summarizes student enrollments for each course, counting and averaging registration status codes (e.g., registered, dropped, waitlisted) at the course level.
#' It returns a summary table with counts and means for each registration status, optionally filtered by specific status codes.
#'
#' @param students A data frame of student-level course registration data. Must include columns such as 'Course Campus Code', 'Course College Code', 'Academic Period Code', 'SUBJ_CRSE', 'Student ID', 'Registration Status Code', and 'term_type'.
#' @param reg_status Optional character vector of registration status codes to filter by (e.g., c("RE", "DR")). If NULL (default), all status codes are summarized and additional summary columns are calculated (registered, drops, waitlist, etc.).
#'
#' @return A data frame summarizing enrollments at the course level. If reg_status is NULL, the result includes columns for registered, early/late/total drops, waitlist, and means across terms. If reg_status is specified, the result is filtered for those codes only.
#'
#' @details
#' - The function groups students by course, term, and registration status, then summarizes counts and means.
#' - If reg_status is NULL, it produces a wide summary with columns for registered, drops (early/late/all), waitlist, and total enrollments, as well as means across terms.
#' - If reg_status is provided, only those status codes are summarized.
#' - NAs are replaced with 0 in the summary output.
#'
#' @examples
#' # Summarize all registration statuses:
#' calc_cl_enrls(students_df)
#' # Summarize only registered and dropped students:
#' calc_cl_enrls(students_df, reg_status = c("RE", "DR"))

calc_cl_enrls <- function(filtered_students, reg_status = NULL) {

  # filtered_students <- load_students()
  #opt <- list()
  #opt[["course"]] <- "HIST 1105"
  #filtered_students <- filter_class_list(students,opt)
  # reg_status <- c("DR")
  # reg_status <- NULL


  reg_stats_summary <- tibble()
  
  message("[enrl.R] Welcome to calc_cl_enrls!")
  
  # get distinct rows within courses; use SUBJ_CRSE to lump all sections topics courses together
  message("[enrl.R] Getting distinct student within courses...")
  cl_enrls <- filtered_students %>%
    group_by(`Course Campus Code`,`Course College Code`,`Academic Period Code`, `SUBJ_CRSE`) %>% 
    distinct(`Student ID`, .keep_all = TRUE)
  
  # count students in each term by reg status code
  message("[enrl.R] Counting students in each campus/college/course/term by reg status code...")
  cl_enrls <- cl_enrls %>% group_by (`Course Campus Code`,`Course College Code`,SUBJ_CRSE,`Registration Status Code`,`Academic Period Code`, term_type) %>% 
    summarize(count = n(), .groups="keep") 
  
  # calc mean reg codes per course and term type
  message("[enrl.R] Calculating mean counts across terms...")
  cl_enrls <- cl_enrls %>% group_by (`Course Campus Code`,`Course College Code`,SUBJ_CRSE,term_type,`Registration Status Code`) %>% 
    mutate(mean = round(mean(count),digits=1))
  
  
  if (is.null(reg_status)) {
    message("[enrl.R] reg_status is NULL; using all registration codes...")

    # Group without Registration Status Code
    cl_enrls <- cl_enrls %>% group_by(`Course Campus Code`,`Course College Code`,SUBJ_CRSE,`Academic Period Code`, term_type)

    message("[enrl.R] gathering information about registrations...")
    reg_stats_summary <- cl_enrls %>% filter(`Registration Status Code` %in% c("RE","RS"))  %>%
      summarize(registered = sum(count), .groups="keep")

    message("[enrl.R] gathering early drops (reg code DR)...")
    de <- cl_enrls %>% filter (`Registration Status Code` %in% c("DR")) %>%
      summarize(dr_early = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, de, all=T)

    message("[enrl.R] gathering late drops (reg codes DG, DW)...")
    dl <- cl_enrls %>% filter (`Registration Status Code` %in% c("DG","DW")) %>%
      summarize(dr_late = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, dl, all=T)

    message("[enrl.R] gathering total drops (reg codes DR, DG, DW)...")
    da <- cl_enrls %>% filter (`Registration Status Code` %in% c("DR","DG","DW")) %>%
      summarize(dr_all = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, da, all=T)

    message("[enrl.R] gathering information about waitlist status...")
    wl <- cl_enrls %>% filter (`Registration Status Code` %in% c("WL")) %>%
      summarize(wl_all = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, wl, all=T)
    
    # Filter out waitlisted students in registration totals
    message("[enrl.R] filtering out waitlisted students from registration totals...")
    cl_total <- cl_enrls %>% filter (!`Registration Status Code` %in% c("WL")) %>% 
      summarize(cl_total = sum(count), .groups="keep")
    reg_stats_summary <- merge(reg_stats_summary, cl_total, all=T)
    
    # remove NAs from merging
    message("[enrl.R] replacing NAs with 0...")
    reg_stats_summary[is.na(reg_stats_summary)] <- 0
    
    
    # regroup without APC to calc means
    message("[enrl.R] regrouping without Academic Period Code to calculate means...")
    reg_stats_summary <- reg_stats_summary %>% group_by(`Course Campus Code`,`Course College Code`,SUBJ_CRSE, term_type)
    
    # get means across term_types
    message("[enrl.R] calculating means across term types...")
    reg_stats_summary <- reg_stats_summary %>%
      mutate(across(c(dr_early, dr_late, dr_all,cl_total,registered), ~ round(mean(.), digits = 2), .names = "{.col}_mean"))
  } # end if reg_status is null

  # if given list of reg codes, filter for those
  else if (!is.null(reg_status)) {
    message("[enrl.R] Filtering for status codes: ", reg_status)
    reg_stats_summary <- cl_enrls %>% filter (`Registration Status Code` %in% reg_status)
  }

  message("[enrl.R] calc_cl_enrls returning ",nrow(reg_stats_summary)," rows.")

  return (reg_stats_summary)
}

  
compress_aop_pairs <- function (courses,opt) {
  message("compressing AOP courses into single row...")
  
  # for testing...
  # courses <- load_courses()
  # opt <- list()
  # #opt[["course"]] <- "BIOL 2305"
  # opt[["term"]] <- "202210"
  # courses <-  filter_DESRs(courses,opt)
  
  # for clarity, combine aop and twin courses into single entry
  # test to see if we're filtering by dept
  courses <- courses %>%  group_by(TERM, XL_CODE)
  
  # get just AOP courses
  courses_aop <- courses %>% filter (INST_METHOD == "MOPS")
  
  # AOP sections don't necessarily have a partner, so remove those without one
  # TODO: handle case of AOP course having partner, but not being crosslisted
  # might be able to check on course title
  courses_aop <- courses_aop %>% filter (XL_CODE != "0")
  
  # get pairs of aop and twin section
  aop_pairs <- courses_aop %>% filter (XL_CODE %in% courses_aop$XL_CODE) %>% 
    distinct(CRN, .keep_all = TRUE) %>% 
    group_by(TERM,XL_CODE)
  
  # to collapse the aop and online section into one row, get each section's enrollment
  aop_pairs <- aop_pairs %>% mutate (sect_enrl = ENROLLED, pair_enrl = total_enrl - ENROLLED)
  
  # arrange by inst_method, and take first row of group
  aop_single <- aop_pairs %>% arrange(INST_METHOD) %>% filter (row_number() == 1)
  # message("aop sections:")
  # print(aop_single)
  
  # since compressing two sections into one, change ENROLLED to mimic total_enrl
  # otherwise, compressing effectively deletes the non-aop section enrollment
  aop_single <- aop_single %>% mutate (ENROLLED = total_enrl)
  
  # remove all pairs from orig course list
  courses <- courses %>% filter (!(XL_CODE %in% courses_aop$XL_CODE)) %>% distinct(CRN, .keep_all = TRUE) %>% 
    group_by(TERM,XL_CODE)
  
  # add all single rows
  courses <- rbind(courses,aop_single)
  
  message("returning compressed aop rows...")
  
  return(courses)
} # end compress_aop_pairs




# generic summary function based on group_cols
# replaces the many variants of aggregate
summarize_courses <- function (courses, opt) {
  message("[enrl.R] summarizing courses with group_cols...")
  
  # set default group_cols
  # group by CRSE_TITLE to differentiate topics courses that use same SUBJ_CRSE
  if (is.null(opt[["group_cols"]])) {
    group_cols <- c("CAMP","COLLEGE","TERM", "term_type", "SUBJ","SUBJ_CRSE","CRSE_TITLE","level","gen_ed_area")
    message("[enrl.R] group_cols is null; using default: ", paste(group_cols, collapse = ", "))
  }
  else {
    group_cols <- opt[["group_cols"]]
    group_cols <- convert_param_to_list(group_cols)
    group_cols <- as.character(group_cols)
    message("[enrl.R] specified grouping by: ", paste(group_cols, collapse = ", "))
  }
  
  # Main summary across sections
  message("[enrl.R] summarizing enrollments...")
  summary <- courses %>% ungroup() %>% group_by_at(group_cols) %>% 
    summarize(sections=n(), 
      xl_sections=sum(XL_CODE != "0" & XL_CODE != "", na.rm=TRUE),
      reg_sections=sum(XL_CODE == "0" | XL_CODE == "" | is.na(XL_CODE)),
      avg_size=round(mean(ENROLLED),digits=1), 
      enrolled=sum(ENROLLED),
      avail=sum(SEATS_AVAIL),
      waiting=sum(WAIT_COUNT), 
      .groups="keep")
  
  return(summary)
}


############# aggregate function (for enrollment summaries)
aggregate_courses <- function(courses, opt) {
  message("[enrl.R] Welcome to aggregate_courses!")

  if (!is.null(opt[["group_cols"]])) {
    message("[enrl.R] opt$group_cols is not null. Summarizing by group_cols...")
    summary <- summarize_courses(courses,opt)
  }
  else {
    message("[enrl.R] ERROR: opt is: ", opt)
    stop("[enrl.R] opt$group_cols is null. Please specify group_cols for aggregation.")
  }
  
  # return the summary DF
  message("[enrl.R] Done aggregating! Returning summary with ", nrow(summary), " rows...")
  return(summary)

} # end aggregate_courses



get_enrl_for_dept_report <- function(courses, d_params) {
  
  message("welcome to get_enrl_for_dept_report!")  
  
  myopt <- list()
  myopt$dept <- d_params[["dept_code"]]
  myopt$group_cols <- c("SUBJ","SUBJ_CRSE","CRSE_TITLE","level","gen_ed_area")
  myopt$x <- "compress"
  myopt$uel <- TRUE 
  
  #TODO: filter out AOP sections so it doesn't bring down averages?
  
  message("getting enrollment data via get_enrl...")
  summary_across_terms <- get_enrl(courses,myopt)  # filter, aggregate, etc
  
  # for inspection, rank by avg size across terms or total enrolled
  highest_total_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(enrolled)) %>% slice_head(n=10)    
  highest_mean_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(avg_size)) %>% slice_head(n=10)    
  
  highest_total_enrl_plot <- highest_total_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, enrolled)) %>%
    ggplot(aes(y=CRSE_TITLE, x=enrolled)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    ylab("Course") + xlab("Total Enrollment (since 2019)") 
  
  highest_mean_enrl_plot <- highest_mean_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, avg_size)) %>%
    ggplot(aes(y=CRSE_TITLE, x=avg_size)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_bar(stat="identity") +
    ylab("Course") + xlab("Mean Enrollment (since 2019)") 
  
  
  # histogram of avg class sizes
  highest_mean_enrl <- summary_across_terms  %>% ungroup() %>% arrange(desc(avg_size))  
  
  highest_mean_histo_plot <- highest_mean_enrl %>% 
    mutate(CRSE_TITLE = fct_reorder(CRSE_TITLE, avg_size)) %>%
    ggplot(aes(x=avg_size)) + 
    #ggtitle(plot_title) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "")) +
    geom_histogram(aes(fill=level),bins = 30) +
    scale_fill_brewer(palette=d_params$palette) +
    ylab("Number of courses") + xlab("# of students") 
  
  highest_mean_histo_plot <- ggplotly(highest_mean_histo_plot) %>% 
    layout(legend = list(orientation = 'h', x = 0.3, y = -.3),
           xaxis = list(standoff = -1))
  
  
  # load d_params w/ enrollment plots
  d_params$plots[["highest_total_enrl_plot"]] <- highest_total_enrl_plot
  d_params$plots[["highest_mean_enrl_plot"]] <- highest_mean_enrl_plot
  d_params$plots[["highest_mean_histo_plot"]] <- highest_mean_histo_plot
  
  return(d_params)
}


make_enrl_plot_from_cls <- function (reg_stats_summary, opt) {
  message("[enrl.R] Welcome to make_enrl_plot_from_cls!")
  
  # for testing
  # opt <- list()
  # opt$group_cols <- "SUBJ_CRSE, TERM"
  # opt$course <- c("ENGL 1110","ENGL 1120")
  # opt$term_type <- "spring"
  # reg_stats_summary <- calc_cl_enrls(students)  # filter, aggregate, etc
  
  plots <- list()
  
  if (nrow(reg_stats_summary) > 0) {
    plot <- ggplot(reg_stats_summary, aes(x = `Academic Period Code`, y = registered, 
                                         fill = SUBJ_CRSE, 
                                         group = SUBJ_CRSE)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ `Course Campus Code`, scales = "fixed") +
      labs(title = "Enrollment by Campus", x = "Term", y = "Student Count", fill = "Course") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots$cl_enrl <- ggplotly(plot) %>% layout(legend = list(orientation = 'h', x = 0.3, y = -.3))
  } else {
    plots$cl_enrl <- NULL
  }
  
  plots$cl_enrl
  
  return(plots)
}



# takes summary data from enrl (already filtered, aggregated, etc)
make_enrl_plot <- function(summary, opt) {
  message("[enrl.R] Welcome to make_enrl_plot!")

  ########## for studio testing
  # opt <- list()
  # opt$group_cols <- "SUBJ_CRSE, TERM"
  # opt$course <- c("ENGL 1110","ENGL 1120")
  # opt$term_type <- "spring"
  # summary <- get_enrl(courses,opt)  # filter, aggregate, etc

  # create empty list of plots
  plots <- list()

  # Validate input
  if (missing(summary) || !is.data.frame(summary)) {
    message("[enrl.R] Cannot create plot: Invalid summary data.")
    return(NULL)
  }

  message("[enrl.R] Data shape: ", nrow(summary), " rows")
  message("[enrl.R] Columns: ", paste(colnames(summary), collapse = ", "))

  # Validate group_cols
  group_cols <- opt$group_cols
  if (is.null(group_cols) || !("TERM" %in% group_cols) || length(group_cols) < 2) {
    message("[enrl.R] Cannot create plot: opt$group_cols must include 'TERM' and at least one other column name.")
    return(NULL)
  }
# The other grouping column (besides TERM)
  other_group <- setdiff(group_cols, "TERM")[1]
  message("[enrl.R] Grouping by: ", other_group)

  # Facet settings from opt (optional)
  facet_field <- opt[["facet_field"]]
  
  # TODO make more dynamic with Shiny inputs
  facet_scales <- "fixed"
  facet_ncol <- NULL

  if (!is.null(facet_field)) {
    message("[enrl.R] Faceting enrollment plot by field: ", facet_field)
  }
  if (!is.null(opt[["facet_scales"]])) facet_scales <- opt[["facet_scales"]]
  if (!is.null(opt[["facet_ncol"]])) facet_ncol <- as.integer(opt[["facet_ncol"]])

  # Enrollment plot
  message("[enrl.R] Creating Enrollment plot...")
  if (nrow(summary) > 0) {
    plot <- ggplot(summary, aes(x = TERM, y = enrolled, group = .data[[other_group]], color = .data[[other_group]])) +
      geom_line(stat = "identity") +
      labs(title = "Enrollment by Group", x = "Term", y = "Student Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # apply facet if requested and valid
    if (!is.null(facet_field) && facet_field %in% colnames(summary)) {
      if (is.null(facet_ncol)) {
        plot <- plot + facet_wrap(vars(.data[[facet_field]]), scales = facet_scales)
      } else {
        plot <- plot + facet_wrap(vars(.data[[facet_field]]), scales = facet_scales, ncol = facet_ncol)
      }
      message("[enrl.R] Faceting enrollment plot by: ", facet_field, " (scales=", facet_scales, ", ncol=", facet_ncol, ")")
    }

    plots$enrl <- ggplotly(plot) %>% layout(legend = list(orientation = 'h', x = 0.3, y = -.3))
  } else {
    plots$enrl <- NULL
  }

#plots$enrl

message("[enrl.R] returning plots...")
return (plots)
}



###################################
get_enrl <- function (courses,opt,group_cols=NULL) {

  ########## for studio testing
  # opt <- list()
  # load_global_data()
  # all_courses <- courses
  # # opt$group_cols <- "SUBJ_CRSE"
  # # opt$course <- "ENGL 1110"
  # opt$dept <- "AFST"
  # 
  message("[enrl.R] Welcome to get_enrl!")

# check for old aggregate flag until totally phased out
  agg_by <- opt$aggregate
  if (!is.null(agg_by)) {
    stop("[enrl.R] ERROR: old aggregate param detected: ", agg_by)
  }

  # default status to A for active courses
  if (is.null(opt$status)) {
    message("[enrl.R] setting default status to A (active courses only.)")
    opt$status <- "A"
  }

  # default to use exclude list
  if (is.null(opt$uel)) {
    message("[enrl.R] setting default to use exclude list (uel=TRUE).")
    opt$uel <- TRUE
  }
  
  # filter courses according to options
  message("[enrl.R] filtering courses (via filter_DESRs) according to options...")
  courses <- filter_DESRs(courses, opt)

  # define standard columns to keep
  # this includes section enrollment (ENROLLED) and total enrollment from cross-listed courses (total_enrl)
  select_cols <- c("CAMP","COLLEGE", "DEPT", "TERM","term_type","CRN","SUBJ","SUBJ_CRSE","SECT","level","CRSE_TITLE","INST_METHOD","PT","INST_NAME", "job_cat", "ENROLLED","total_enrl","XL_SUBJ","XL_CODE", "SEATS_AVAIL","WAIT_COUNT","gen_ed_area")
  message("[enrl.R] selecting columns: ", paste(select_cols, collapse = ", "))

  ### AOP COMPRESSION
  if (!is.null(opt$aop) && opt$aop == "compress") {
    message("[enrl.R] compressing AOP pairs...")
    courses <- compress_aop_pairs(courses,opt) 
    select_cols <- c(select_cols, "sect_enrl","pair_enrl")
    courses <- courses %>% select(all_of(select_cols))
  }
  else {
    message("[enrl.R] leaving AOP pairs alone...")
    courses <- courses %>% select(all_of(select_cols))
  }
  
  # courses get listed multiple times b/c of crosslisting (inc aop, but also in general)
  # also, a course can also be listed multiple times depending on the lecture/recitation model (b/c of XL_CRSE column)
  
  # remove dupes since we have final columns
  courses <- courses %>% distinct() %>% 
    arrange(CAMP,COLLEGE,SUBJ_CRSE,CRSE_TITLE,term_type,PT,INST_METHOD,INST_NAME)
  
  # check if aggregating
  if(!is.null(opt$aggregate) || !is.null(opt$group_cols)) {
    courses <- aggregate_courses(courses, opt)
  } else {
    message("[enrl.R] No aggregating!")
  }

  message("[enrl.R] All done in get_enrl! Returning data with ", nrow(courses) ," rows...\n")
  return(courses)
  
} # end get_enrl function


###################################
# LOW ENROLLMENT DASHBOARD FUNCTIONS
###################################

#' Get courses below enrollment threshold
#'
#' Identifies courses with enrollment below a specified threshold, grouped by
#' campus, department, course title, and instructional method.
#'
#' @param courses Data frame of course sections (DESRs)
#' @param opt Options list with filtering parameters
#' @param threshold Numeric enrollment threshold (default 15)
#'
#' @return Data frame of low-enrollment courses with enrollment history
get_low_enrollment_courses <- function(courses, opt, threshold = 15) {
  message("[enrl.R] Getting low enrollment courses (threshold: ", threshold, ")...")
  
  # studio testing
  #load_global_data()
  
  # opt <- list()
  # opt$term <- "202280"  
  # opt$dept <- c("HIST","GES")
  # threshold <- 15
  

  # default status to A for active courses
  message("[enrl.R] setting opt status to A (active courses only.)")
  opt$status <- "A"

  # default to use exclude list
  message("[enrl.R] setting opt to use exclude list (uel=TRUE).")
  opt$uel <- TRUE
  
  # HOME leaves one row per all cross-dept xled sections in courses data
  # it's in the "home" dept and with total_enrl as sum of xlisted sections (OR XL_ENRL)
  # we want to filter out non-home xl'ed courses since enrollments tend to be quite small
  opt[["crosslist"]] <- "home"
  message("[enrl.R] setting opt crosslist to HOME to keep only XLed sections in home unit.")
  
  # filter courses
  # since we care about low enrolled sections--not aggregates--don't summarize (ie don't call get_enrl). 
  filtered_courses <- filter_DESRs(courses, opt)
  
  # filter for courses below threshold
  low_enrl <- filtered_courses %>% 
    filter(total_enrl < threshold) %>%
    arrange(CAMP, DEPT, CRSE_TITLE, total_enrl)
  
  # for testing inspection
  # low_enrl <- low_enrl %>% select (CAMP, TERM, SUBJ_CRSE, CRSE_TITLE, XL_CODE, XL_SUBJ, XL_CRSE, ENROLLED, total_enrl)
  
  message("[enrl.R] Found ", nrow(low_enrl), " low enrollment courses below threshold.")
  return(low_enrl)
}


#' Get enrollment history for a specific course
#'
#' Retrieves the last N terms of enrollment data for a specific course offering.
#'
#' @param courses Data frame of course sections (DESRs)
#' @param campus Campus code
#' @param dept Department code
#' @param subj_crse Subject and course number (e.g., "HIST 1105")
#' @param crse_title Course title
#' @param im Instructional method code
#' @param n_terms Number of historical terms to retrieve (default 3)
#'
#' @return Data frame with TERM and enrolled columns
get_course_enrollment_history <- function(courses, campus, dept, subj_crse, crse_title, im, n_terms = 3) {
  message("[enrl.R] Getting enrollment history for: ", crse_title, " (", im, ") - ", subj_crse)
  
  # Filter for specific course
  course_history <- courses %>%
    filter(
      CAMP == campus, 
      DEPT == dept, 
      SUBJ_CRSE == subj_crse,
      CRSE_TITLE == crse_title, 
      INST_METHOD == im,
      STATUS == "A"  # Active courses only
    ) %>%
    group_by(TERM) %>%
    summarize(enrolled = sum(ENROLLED), .groups = "drop") %>%
    arrange(desc(TERM)) %>%
    slice_head(n = n_terms) %>%
    arrange(TERM)  # Re-sort ascending for plotting
  
  message("[enrl.R] Found ", nrow(course_history), " historical terms")
  return(course_history)
}


#' Create enrollment history string for display
#'
#' Generates a text representation of enrollment history (e.g., "12 → 10 → 8")
#'
#' @param history_data Data frame with TERM and enrolled columns
#'
#' @return Character string with enrollment trend
format_enrollment_history <- function(history_data) {
  if (nrow(history_data) == 0) return("No history")
  
  enrollments <- history_data %>% pull(enrolled)
  terms <- history_data %>% pull(TERM)
  
  # Create formatted string with terms and enrollments
  history_str <- paste(
    paste0(terms, ": ", enrollments),
    collapse = " → "
  )
  
  return(history_str)
}