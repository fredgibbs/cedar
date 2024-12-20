# Two main functions:
# 1. compute accuracy stats for already completed forecasts using the forecast_data.Rda file
# 2. generate a formatted HTML/ASPX report 


####### TESTING, INCOMPLETE ########
find_major_enrl_correlations <- function(courses, opt) {

  # uncomment for studio testing
  # courses <- load_courses(opt)
  # opt <- list()
  # 
  # aggregate course enrollments across sections
  # get enrollment data for courses in forecast_data
  myopt <- opt
  myopt$aggregate <- "course"
  myopt[["term"]] <- NULL
  #myopt[["course"]] <- list('ENGL 1110',"ENGL 1120")
  
  # test majors and enrollment correlations
  message("getting enrollment data...")
  enrls <- get_enrl(courses,myopt)
  
  # find any correlation between changing numbers of majors and course enrollment
  # ends up being too messy
  majors <- get_agg_major(students)
  merged <- merge(enrls, majors, by.y=c("Academic Period Code"), by.x="TERM", keep = TRUE )
  merged <- merged %>% group_by(SUBJ_CRSE,Major)  
  cor <- merged %>% summarize(cor=cor(enrolled,count)) %>%  arrange(desc(cor))
  
}


# calc_forecast_accuracy loads previously generated forecasting data from the forecasts.Rda file
# and merges enrollment data with forecast data to display together and compute accuracy of forecasts
# OPTIONAL param: opt$course for filtering returned data

calc_forecast_accuracy <- function(students,courses, opt) {
  message("Welcome to calc_forecast_accuracy!")
  
  # uncomment for studio testing
  # courses <- load_courses(opt)
  # students <- load_students(opt)
  # opt <- list()
  # opt$course <- "CHEM 1120C"

  # load existing forecast data
  forecast_data <- load_forecasts(opt)
  
  # filter for course
  if (!is.null(opt$course)) {
    message("filtering forecast data for course...")
    course_list <- convert_param_to_list(opt$course)
    forecast_data <- forecast_data %>% filter (SUBJ_CRSE %in% course_list)
  }
  
  # check to see if we have any data
  if (!is.null(forecast_data) && nrow(forecast_data) == 0) {
    stop("no forecast data for that course.")  
  }
  
  #message("original forecast data:")
  #forecast_data %>% tibble::as_tibble() %>% print(n = 10, width=Inf)
  
  #TODO: i think here, before wide view, is best place to calc accuracy stats.
  
  # make easier to read with wide format
  message("pivoting to wide...")
  forecast_data <- forecast_data %>% select (-c(continuing_forecast,incoming_forecast)) 
  forecast_data_wide <- forecast_data %>% pivot_wider(names_from = method, values_from = forecast)
  #forecast_data_wide %>% tibble::as_tibble() %>% print(n = 10, width=Inf)
  
  
  # get enrollment data for courses in forecast_data
  myopt <- opt
  myopt$aggregate <- "course"
  myopt$aop <- "compress"
  myopt[["term"]] <- NULL
  myopt[["course"]] <- as.list(unique(forecast_data$SUBJ_CRSE))
  
  message("getting DESR enrollment data...")
  enrls <- get_enrl(courses,myopt)
  
  # calc mean enrollments by term_type; maybe move to enrl?
  #enrls <- add_term_type_col(enrls,"TERM")
  #enrls <- enrls %>% group_by(term_type,SUBJ_CRSE)
  
  # enrls <- enrls %>% mutate (enrl_mean_tt = mean (enrolled))
  # enrls <- enrls %>% mutate (avail_mean_tt = mean (avail))
  # enrls <- enrls %>% mutate (sections_mean_tt = mean (sections))
  
  message("merging forecast summary data with enrollment summary data...")
  enrl_w_forecast <- merge (forecast_data_wide, enrls ,by=c("SUBJ_CRSE", "TERM"),all.x=T)
  enrl_w_forecast <- add_term_type_col(enrl_w_forecast,"TERM")
  
  # fill in missing values
  enrl_w_forecast <- enrl_w_forecast %>% fill(SUBJ,CRSE_TITLE,level,gen_ed_area)
  
  #print(enrl_w_forecast)
  

  message("getting CL enrollment data...")
  cl_enrls <- calc_cl_enrls(students,reg_status="all")
  cl_enrls <- cl_enrls %>% rename( cl_total = count )
  
  message("merging forecast summary data with enrollment summary data...")
  enrl_w_forecast <- merge (enrl_w_forecast, cl_enrls, by.x=c("SUBJ_CRSE", "TERM"), by.y=c("SUBJ_CRSE", "Academic Period Code") ,all.x=T)
  
  #print(enrl_w_forecast)
  
  message("selecting fields...")
  forecast_summary <- enrl_w_forecast[, c("TERM","term_type","SUBJ","SUBJ_CRSE","level","CRSE_TITLE","enrolled","cl_total","sections","avg_size","avail","waiting" ,  "conduit", "major")]
  #,"enrl_mean_tt" ,"avail_mean_tt" , "sections_mean_tt"
  
  message("computing accuracy...")
  forecast_summary <- forecast_summary %>% 
    mutate (c_accr = round(conduit/enrolled,digits=2), 
            c_cl_accr = round(conduit/cl_total,digits=2), 
            m_accr = round(major/enrolled,digits=2), 
            m_cl_accr = round(major/cl_total,digits=2) ) 
  
  # only include rows with complete data
  # this removes future rows without enrollment data
  # or rows with course/term combos that haven't been forecasted
  
  #forecast_summary_complete <- na.omit(forecast_summary)
  
  forecast_summary[is.na(forecast_summary)] <- 0
  forecast_summary_complete <- forecast_summary
  
  # calc group summary stats
  message("computing summary stats...")
  
  forecast_summary_avgs <- forecast_summary_complete %>%
    group_by(SUBJ_CRSE,term_type) %>%
    summarize(
      avg_c_accr = round(abs( 1 - mean(c_accr, na.rm=TRUE)),digits=2),
      avg_m_accr = round(abs( 1 - mean(m_accr, na.rm=TRUE)),digits=2),
      avg_m_cl_accr = round(abs( 1 - mean(m_cl_accr, na.rm=TRUE)),digits=2), .groups="keep"
    )

  forecast_summary_sds <- forecast_summary_complete %>% 
    group_by(SUBJ_CRSE,term_type) %>% 
    summarize(
      sd_c_accr = sd(c_accr, na.rm=TRUE),
      sd_m_accr = sd(m_accr, na.rm=TRUE),
      sd_m_cl_accr = sd(m_cl_accr, na.rm=TRUE), .groups="keep"
    )
  
  
  #### INcOMPLETE LINEAR MODEL WORK
  # summary <- get_enrl(courses,myopt)
  # summary <- add_term_bins(summary,"TERM")
  # 
  # options(scipen=999)
  # 
  # # returns tidy tibble with summary stats
  # stats <- summary %>% group_by(SUBJ_CRSE,term_type)  %>% group_modify(~ broom::tidy(lm(enrolled ~ term_bin, data = .x)))
  # 
  # # can also use standard lm functionality
  # #lms <- enrls %>% group_by(SUBJ_CRSE,term_type)  %>% group_map(~ (lm(enrolled ~ term_bin, data = .x)))
  # 
  # stats$estimate <- round(stats$estimate, digits=2)
  # 
  # stats_sorted <- stats %>% filter(term == "term_bin") %>% arrange(desc(estimate))
  # 
  
  
  message("identifying the best method...")
  forecast_summary_avgs$method <- colnames(forecast_summary_avgs)[apply(forecast_summary_avgs,1,which.min)]

  forecast_summary_avgs$method <- substring(forecast_summary_avgs$method,5)
  forecast_summary_avgs$method <- substr(forecast_summary_avgs$method,1,nchar(forecast_summary_avgs$method)-5)
  
  #forecast_summary_avgs <- forecast_summary_avgs %>% select(SUBJ_CRSE,term_type,pref_method=method)
  forecast_summary <- merge(forecast_summary,forecast_summary_avgs)
  forecast_summary <- forecast_summary %>% rename(pref_method = method)
  # forecast_summary_stats %>% tibble::as_tibble() %>% print(n = 25, width=Inf)

  # group according to SUBJ_CRSE and term_type so that lag function can get previous term_type enrollment
  forecast_summary <- forecast_summary %>% group_by(SUBJ_CRSE,term_type) %>% arrange(TERM)
  
  #forecast_summary <- forecast_summary %>%  mutate (pref_method = ifelse (pref_method == "major_cl","major",pref_method))
  
  forecast_summary <- forecast_summary %>% mutate(usevals = case_when(
                                                      pref_method == "m" ~ major,
                                                      pref_method == "m_cl" ~ major,
                                                      pref_method == "c" ~ conduit
                                                  ))
  #usevals <- forecast_summary[match(forecast_summary$pref_method , names(forecast_summary))][1,]
  #usevals <- usevals %>%  rename(usevals = 1)
  #forecast_summary <- cbind(forecast_summary,usevals)
  
  message("estimating section needs...")
  # try to fractionally estimate how many sections we need
  forecast_summary <- forecast_summary %>% 
    #mutate (major_section_needs = ( major / ((lag(enrolled) + lag(avail)) / lag(sections))))
    mutate (rec_sections = round( usevals / ((lag(enrolled) + lag(avail)) / lag(sections)),digits=2))
  
  # calc how much anticipated needs differs from course means
  forecast_summary <- forecast_summary %>% 
    mutate (diff_fr_prev = round( rec_sections - lag(sections),digits=2)) 
  
  # flag any courses that need more investigation because they are on the bubble of needing more or less
  # if within .3 of integer, round up or down to it.
  # otherwise section recommendation to -1 to flag for further analysis
  forecast_summary <- forecast_summary %>% mutate (recommendation = 
                                                     ifelse(ceiling(rec_sections) -  rec_sections < .3, ceiling(rec_sections), 
                                                            ifelse(rec_sections - floor(rec_sections) < .3, floor(rec_sections), -100)
                                                            )
                                                   )
  
  # flag any courses with large diff_fr_prev (large is more than 1 section)
  # these can be algorithmically smoothed over time based on other course data,
  # but for now we need manual inspection
  forecast_summary <- forecast_summary %>% mutate (recommendation = 
                                                     ifelse(diff_fr_prev > 1, -101, recommendation))
  
  
  #forecast_summary <- forecast_summary %>% select(-c(term_type,SUBJ,level,usevals))
  forecast_summary <- forecast_summary %>% ungroup() %>% select(-term_type,-SUBJ,-level,-usevals)
  
  # reorder by course
  forecast_summary <- forecast_summary %>% arrange(SUBJ_CRSE,TERM)
  
  # reorder by diff_fr_prev
  # forecast_summary <- forecast_summary %>% arrange(diff_fr_prev,SUBJ_CRSE,term_type,TERM)
  
  # TODO: divide up by area?
  
  # filter at very end so previous enrollments are available for earlier calculations
  if (!is.null(opt$term)) {
    message("filtering for term...")
    term_list <- convert_param_to_list(opt$term)
    forecast_summary <- forecast_summary %>%  filter_by_term(opt$term,"TERM")
  }
  
  message("forecast-report done and returning forecast_summary...")
  return(forecast_summary)
}


########### UNFINISHED
########## RESET TABLE FUNCTION 
# load Rda, remove rows for specified course, re-save
reset_forecast_course <- function(opt) {
  
  if (is.null(opt$course)) {
    stop("Please specify a course (-c or --course).")
  }
  
  # for studio use...
  # opt$course <- "HIST 491"
  
  # even though load_forecast_data can be used, we still need to set the filename for saving
  forecast_rda_file <- paste0(cedar_data_dir,"/processed/forecasts.Rda")
  load(forecast_rda_file) # loads forecast_data
  
  # a few ways of cleaning data for studio use.
  # here we can find things we don't want...
  unique(forecast_data$SUBJ_CRSE)
  
  forecast_data <- forecast_data[grep("ARCH",forecast_data$SUBJ_CRSE, invert=TRUE),]
  forecast_data <- forecast_data[grep(" [0-9]{3}$",forecast_data$SUBJ_CRSE, invert=TRUE),]
  
  # save new data
  message("saving forecasts.Rda...")
  save(forecast_data,file=forecast_rda_file)
}



############### GENERAL FUNCTION START ##################
# this function generates a report, and first computes accuracy stats

report_forecasts <- function(forecast_data, opt) {
  message("welcome to forecast-report!")

  # for studio testing...
  # opt <- list()
  # opt$method <- "all" 
  # #opt$course <- "MATH 1240"
  # opt$term <- 202410
  
  # opt$course <- "cl_comm1000"
  # opt$term <- "springs"
  
  #TODO: detect if no forecast_data and run calc_forecast_accuracy
  #forecast_data <- calc_forecast_accuracy(students,courses,opt)
  #forecast_data %>% tibble::as_tibble() %>% print(n=nrow(.), width=Inf)
  
  if (!is.null(opt[["arrange"]])) {
    arrange_col <- opt[["arrange"]]
    forecast_data <- forecast_data %>%  arrange(get({{arrange_col}}) )
  }
  
  # payload
  d_params <- list("opt" = opt,
                   "tables" = list(
                    "forecasts" = forecast_data
                     #"forecast_next_term" = forecast_next_term
                   )
  )
  
  message("rendering forecast report...")
  
  # set output data
  output_filename <- "forecast-report"
  d_params$output_filename <- output_filename
  d_params$rmd_file <- "cones/forecast-report/forecast-report.Rmd"
  d_params$output_dir_base <- paste0(cedar_output_dir,"forecast-reports/")
  
  create_report(opt,d_params)
  
  message("all done in forecast-report!")
}
