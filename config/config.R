Sys.setenv("shiny" = FALSE)

cedar_use_small_data <- FALSE  # or FALSE for production

# Data serialization format: "qs" (faster, recommended) or "rds" (base R, fallback)
cedar_use_qs <- TRUE

cedar_base_dir <- "/Users/fwgibbs/Dropbox/projects/cedar/"
cedar_output_dir <- paste0(cedar_base_dir,"output/")
cedar_data_dir <- paste0(cedar_base_dir,"data/")

############ MyReports downloads
#cedar_myreports_cloud_downloads_url <- "http://144.126.214.254:8080/downloads/"

# used by parse-data.R to find MyReports downloads
cedar_myreports_local_dir <- "/Users/fwgibbs/Dropbox/projects/shared-data/"

# if running in docker
cedar_myreports_docker_dir <- "./data/"

# if running in a local container
#cedar_myreports_local_docker_dir <- "/Users/fwgibbs/Dropbox/projects/shared-data/"

# if running in a droplet container
#cedar_myreports_droplet_docker_dir <- "/srv/shiny-server/cedar/data/"


############ CEDAR R datafiles
# i need a way of getting to droplet Rds files from the local machine

# if running local node app
cedar_data_local_dir <- cedar_data_dir

# if running in docker
cedar_data_docker_dir <- "./data/"


# if running in a local container
#cedar_data_local_docker_dir <- "/Users/fwgibbs/Dropbox/projects/shared-data/"

# if running in a droplet container
#cedar_data_droplet_docker_dir <- "/srv/shiny-server/cedar/data/"


# original cloud location for CEDAR data files
# cedar_cloud_data_urls <- list(
#   hr_data = "https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/EVuBWFMaTDBFgtdSCyG1m-ABXYHteq5r6okXOi3lkDuJRg?download=1",
#   class_lists = "https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/EZqUuRUL8FJElYNcBLaCpOABXq7VHxAZ2_xbWBC_REy3xA?download=1",
#   degrees = "https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/EQQpk-z1pRBFn9lXWDpOBmYBhyvazVJc1HLs3zHAsOcxKg?download=1",
#   desrs = "https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/Ef4vke1U6z9Hl9GJWtDpGXgBne76woiYNdKvkVEw-CGg9w?download=1",
#   academic_studies = "https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/ERBYdp8fKk5Kn5QvmxXgZcsBiUdDz0NiOnV6wFj9W0se7w?download=1",
#   forecasts = "https://unmm-my.sharepoint.com/:u:/g/personal/fwgibbs_unm_edu/ETo3uGhA0glEs8V9fs4DsKkBu9PW-5VprYd3GFc13Kc8Pw?download=1"
# )


# if you want to archive processed downloaded MyReports, update the following:
# set to NULL (no quotes) to skip archiving MyReports downloads
cedar_data_archive_dir <- NULL


# define the current term
cedar_current_term <- 202580

# these term codes control how much data appears on dept-reports
cedar_report_start_term <- 202180
cedar_report_end_term <- 202560

# registration underway for next term (compared to current term set above)
cedar_registration_underway <- FALSE

cedar_regstats_thresholds <- list()
cedar_regstats_thresholds[["min_impacted"]] <- 15 # min difference b/w enrollment and mean (= number of students affected) 
cedar_regstats_thresholds[["pct_sd"]] <- .5 # percent of students outside the mean compared to standard deviation
cedar_regstats_thresholds[["min_squeeze"]] <- .3 # squeeze is ratio of avail seats to  mean attrition
cedar_regstats_thresholds[["min_wait"]] <- 20 # min number of students on waitlist before being flagged
cedar_regstats_thresholds[["section_proximity"]] <- .3 # how close to integer before rounding up/down for recommended sections? closer to .5 reduces -100s

cedar_report_palette <- "Spectral"

rstudio_pandoc <- "/usr/local/bin/"