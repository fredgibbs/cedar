message("[global.R] Welcome to global.R!")

# Note: renv is already activated by .Rprofile -> renv/activate.R when R starts
# No need to call renv::activate() again here

# Function to detect if running in Docker
is_docker <- function() {
  file.exists("/.dockerenv") ||
    (file.exists("/proc/1/cgroup") && any(grepl("docker|containerd", readLines("/proc/1/cgroup"))))
}

# Log environment
if (is_docker()) {
  message("[global.R] Running inside a Docker container.")
  Sys.setenv("docker" = TRUE)
} else {
  message("[global.R] Running locally or in an unknown environment.")
  Sys.setenv("docker" = FALSE)
}

# Set Shiny environment variable to be TRUE
message("[global.R] Setting shiny environment variable to be TRUE...")
Sys.setenv("shiny" = TRUE)

message("[global.R] Loading libraries...")
library(jsonlite)
library(shiny)
library(plotly)
library(DT)
library(bslib)
library(tidyverse)
library(htmlwidgets)
library(htmltools)
library(yaml)
library(qs)

message("[global.R] Loading shiny_config...")
source("config/shiny_config.R")

message("[global.R] Loading functions...")
source("R/branches/load_funcs.R")

message("[global.R] Calling load_funcs...")
load_funcs(cedar_base_dir)


# Copy all .html files from data/ to www/ at startup
# This allows them to be served by the Shiny server
data_dir <- file.path(getwd(), "data")
www_dir <- file.path(getwd(), "www")

if (dir.exists(data_dir)) {
  html_files <- list.files(data_dir, pattern = "\\.html$", full.names = TRUE)
  if (length(html_files) > 0) {
    if (!dir.exists(www_dir)) {
     dir.create(www_dir, recursive = TRUE)
    }
    file.copy(html_files, www_dir, overwrite = TRUE)
    message("[global.R] Copied HTML reports from data/ to www/: ", paste(basename(html_files), collapse = ", "))
  }
}

# if running in a docker container, look for data in the usual data dir
if (is_docker()) {
  docker_data_dir <- "/srv/shiny-server/cedar/data/"
  message("[global.R] Loading data files for Docker environment. Data dir: ", docker_data_dir)
  
  # Helper function to time data loading
  timed_read_data <- function(path, label) {
    message(sprintf("loading %s...", label))
    t <- system.time({ obj <- load_cedar_data(path) })
    message(sprintf("[global.R] Loaded %s in %.2f seconds.", label, t["elapsed"]))
    obj
  }

# List of base filenames (without extension) for data files
file_list <- c("DESRs", "class_lists", "academic_studies", "degrees", "forecasts", "hr_data")

# Function to get the correct file path (regular or _small)
get_data_path <- function(base_name, data_dir, use_small = FALSE) {
  ext <- get_data_extension()
  message(sprintf("[global.R] get_data_extension() returned: %s", ext))
  message(sprintf("[global.R] cedar_use_qs is: %s", if(exists("cedar_use_qs")) cedar_use_qs else "NOT SET"))
  
  if (use_small) {
    # Try QS format for small file
    small_path_qs <- file.path(data_dir, paste0(base_name, "_small.qs"))
    if (file.exists(small_path_qs)) return(small_path_qs)
    
    # Try RDS format for small file
    small_path_rds <- file.path(data_dir, paste0(base_name, "_small.Rds"))
    if (file.exists(small_path_rds)) return(small_path_rds)
  }
  # Return path with preferred extension (load_cedar_data will handle fallback)
  message(sprintf("[global.R] Using large data file for %s", paste0(base_name, ext)))
  file.path(data_dir, paste0(base_name, ext))
}

# Load all files into a named list
data_objects <- list()

# Iterate over each base filename and load the corresponding data file
for (base_name in file_list) {
  use_small <- exists("cedar_use_small_data") && isTRUE(cedar_use_small_data)
  message(sprintf("[global.R] Loading data for %s (use_small: %s)...", base_name, use_small))
  data_path <- get_data_path(base_name, data_dir, use_small)
  message(sprintf("[global.R] Data path: %s", data_path))

  # Let load_cedar_data handle file existence and format fallback (QS -> RDS)
  data_objects[[base_name]] <- timed_read_data(data_path, basename(data_path))
} # end data loading


# Drop unused columns (as defined in lists.R) in Academic Studies and class_lists 
if (!is.null(data_objects[["academic_studies"]]) && length(academic_studies_drop_cols)) {
  data_objects[["academic_studies"]] <- data_objects[["academic_studies"]] %>%
    select(-all_of(academic_studies_drop_cols))
}

if (!is.null(data_objects[["class_lists"]]) && length(class_lists_drop_cols)) {
  data_objects[["class_lists"]] <- data_objects[["class_lists"]] %>%
    select(-all_of(class_lists_drop_cols))
}


# Assign loaded data to variables for backwards compatibility 
# TODO: clean up once backwards compatibility is not needed
courses <- data_objects[["DESRs"]]
students <- data_objects[["class_lists"]]
academic_studies <- data_objects[["academic_studies"]]
degrees <- data_objects[["degrees"]]
forecasts <- data_objects[["forecasts"]]
hr_data <- data_objects[["hr_data"]]
} # end data loading for docker

# Initialize logging system
message("[global.R] Initializing logging system...")
init_logging()

message("[global.R] Loading ui.R and server.R...")
source("ui.R")
source("server.R")