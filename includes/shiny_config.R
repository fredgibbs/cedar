cedar_base_dir <- "./"

# recommended to keep as is, but you can specify other folders you've created
cedar_output_dir <- paste0(cedar_base_dir,"output/")
#cedar_data_dir <- paste0(cedar_base_dir,"data/")

# if you want to save the MyReports excel files you will download, update the following:
cedar_data_archive_dir <- NULL
# set the above to NULL (no quotes) to skip archiving MyReports downloads


# standard OneDrive location to save shared files (requires local OneDrive syncing enabled)
cedar_onedrive_dir <- NULL

# define the current term
cedar_current_term <- 202510

# these term codes control how much data appears on dept-reports
cedar_report_start_term <- 201980
cedar_report_end_term <- 202510

# registration underway for next term (compared to current term set above)
cedar_registration_underway <- FALSE

cedar_regstats_thresholds <- list()
cedar_regstats_thresholds[["sd_buffer"]] <- 1 # % of standard deviation before flagging as concern
cedar_regstats_thresholds[["min_count"]] <- 20 # min number of students in a course before we flag as a concern
cedar_regstats_thresholds[["min_impacted"]] <- 20 # min difference b/w enrollment and mean (= number of students affected) 
cedar_regstats_thresholds[["min_pct_sd"]] <- 1.2 # percent of students outside the mean compared to standard deviation
cedar_regstats_thresholds[["min_squeeze"]] <- .5 # squeeze is ratio of avail seats to  mean attrition
cedar_regstats_thresholds[["min_wait"]] <- 20 # min number of students on waitlist before being flagged
cedar_regstats_thresholds[["section_proximity"]] <- .3 # how close to integer before rounding up/down for recommended sections? closer to .5 reduces -100s

cedar_report_palette <- "Spectral"

rstudio_pandoc <- NULL