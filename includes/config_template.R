cedar_base_dir <- "FULL PATH TO YOUR CEDAR DIRECTORY" # this should end in /cedar/

# recommended to keep as is, but you can specify other folders you've created
cedar_output_dir <- paste0(cedar_base_dir,"output/")
cedar_data_dir <- paste0(cedar_base_dir,"data/")

# if you want to save the MyReports excel files you will download, update the following:
cedar_data_archive_dir <- "FULL PATH TO WHERE YOU WANT TO STORE DOWNLOADED MYREPORT FILES" 
# set the above to NULL (no quotes) to skip archiving MyReports downloads

# standard OneDrive location to save shared files (requires local OneDrive syncing enabled)
cedar_onedrive_dir <- "FULL PATH TO A LOCAL ONEDRIVE LOCATION"
# example: cedar_onedrive_dir <- "~/Library/CloudStorage/OneDrive-UniversityofNewMexico/CEDAR/reports"

# define the current term
cedar_current_term <- 202510

# these term codes control how much data appears on dept-reports
cedar_report_start_term <- 201980
cedar_report_end_term <- 202510

# registration underway for next term (compared to current term set above)
cedar_registration_underway <- FALSE

cedar_regstats_thresholds <- list()
cedar_regstats_thresholds[["sd_buffer"]] <- 1 # % of standard deviation before flagging as concern
cedar_regstats_thresholds[["min_count"]] <- 20 # min number of students in a bump or dip before we flag as a concern
cedar_regstats_thresholds[["min_impact"]] <- 50 # rough number of students affected abs((count - mean))
cedar_regstats_thresholds[["z"]] <- 1.2 # percent of students outside the mean affected compared to standard deviation. Z = (mean – count) / sd 
cedar_regstats_thresholds[["min_mean_attr"]] <- 20 # min mean attrition before a course can be squeezed
cedar_regstats_thresholds[["min_squeeze"]] <- .5 # squeeze is avail seats / mean attrition
cedar_regstats_thresholds[["min_wait"]] <- 20 # min number of students on waitlist before being flagged

cedar_report_palette <- "Spectral"

# use Rstudio's pandoc version
rstudio_pandoc <- "/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools"