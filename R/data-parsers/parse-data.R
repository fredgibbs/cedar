# parse-data.R
# This script processes various MyReports data files, parses them, and saves the results. 
# It is designed to be run from the command line or as a Plumber API endpoint.

# Default behavior is to process all .xlsx files in the MyReports downloads directory,
# parse them according to the specifications defined in report_specs, and save the results
# as Rds files in the specified data directory. It can also archive the original .xlsx
# files if archiving is enabled in the configuration.

# define details for each kind of MyReports report
message("defining report specifications...")
report_specs <- list(
  desr = list(
    data_file = "DESRs",
    term_col = "TERM",
    parser = "parse-DESR.R",
    filename_sig = "Department_Enrollment_Status"
  ),
  cl = list(
    data_file = "class_lists",
    term_col = "Academic Period",
    ID_col = c("Primary Instructor ID", "Student ID"),
    parser = "parse-class-list.R",
    filename_sig = "Class_List_Guided_Adhoc"
  ),
  as = list(
    data_file = "academic_studies",
    term_col = "Academic Period",
    ID_col = "ID",
    parser = "parse-academic-study.R",
    filename_sig = "Academic_Study_Detail_Guided"
  ),
  deg = list(
    data_file = "degrees",
    term_col = "Academic Period",
    ID_col = "ID",
    parser = "parse-degrees.R",
    filename_sig = "Graduates_and_Pending_Graduates"
  )
)

# Function to check if running in Docker
is_docker <- function() {
  file.exists("/.dockerenv") ||
    (file.exists("/proc/1/cgroup") && any(grepl("docker|containerd", readLines("/proc/1/cgroup"))))
}

# Function to check if running in Posit Connect
is_posit_connect <- function() {
  nzchar(Sys.getenv("CONNECT_SERVER")) ||
    nzchar(Sys.getenv("RSTUDIO_CONNECT_SERVER")) ||
    nzchar(Sys.getenv("RS_SERVER_NAME")) ||
    nzchar(Sys.getenv("RSCONNECT_SERVER"))
}

# Helper function to check memory usage and warn if high
check_memory <- function(context = "") {
  mem_info <- gc(verbose = FALSE)
  mem_used_mb <- sum(mem_info[, 2])  # Memory used in MB
  
  if (mem_used_mb > 1500) {  # Warn if using more than 1.5GB
    message("[parse-data.R] WARNING: High memory usage (", round(mem_used_mb, 0), " MB) at: ", context)
    gc(verbose = FALSE)  # Force garbage collection
  }
  
  return(mem_used_mb)
}


#' process_reports
#'
#' Main function to process MyReports data files.
#' - Loads configuration and required packages.
#' - Determines environment (Docker/local) and sets directories.
#' - Finds and processes .xlsx files for specified report types.
#' - Converts Excel files to CSV, parses data, and saves results as Rds.
#' - Handles encryption of sensitive ID columns.
#' - Designed for command line use.
#'
#' @param report Character vector of report types to process (e.g., "desr", "cl", "as", "deg").
#' @param guide Logical; if TRUE, prints usage instructions.
#' @return None. Side effects: saves processed data, prints progress messages.
process_reports <- function(
  report = NULL,
  guide = FALSE
){

message("[parse-data.R] Welcome to process_reports!")

# uncoment for studio testing...
# report <- list("desr")
  
# Load required packages
message("[parse-data.R] Loading required packages...")
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(cellranger, optparse, tidyverse, readxl, fs, data.table, lubridate, qs)

# set base dir
message("[parse-data.R] Setting base dir...")
if (is_docker()) {
  base_dir <- "./"
} else {
  base_dir <- "/Users/fwgibbs/Dropbox/projects/cedar/"
}
message("[parse-data.R] base dir set to: ", base_dir)

config_dir <- file.path(base_dir, "config")
message("[parse-data.R] setting config directory to: ", config_dir)

R_dir <- file.path(base_dir, "R")
message("[parse-data.R] setting R directory to: ", R_dir)

parsers_dir <- file.path(base_dir, "R", "data-parsers")
message("[parse-data.R] setting parsers directory to: ", parsers_dir)


if (is_docker()) {
  message("[parse-data.R] Docker environment detected, so loading shiny_config.R...")
  source(file.path(config_dir, "shiny_config.R"))
} else {
  message("[parse-data.R] NOT in docker, so loading config.R...")
  source(file.path(config_dir, "config.R"))
}

message("[parse-data.R] sourcing include files...")
source(file.path(R_dir,"lists/mappings.R"))
source(file.path(R_dir,"lists/terms.R"))
source(file.path(R_dir,"lists/gen_ed_courses.R"))
source(file.path(R_dir,"branches/misc_funcs.R"))
source(file.path(R_dir,"branches/filter.R"))
source(file.path(R_dir,"branches/data.R"))
# convert report to list
report_list <- convert_param_to_list(report)

message("[parse-data.R] Determining MyReports downloads directory...")
if (is_docker()) {
  myreports_dir <- "./data/"
} else {
  myreports_dir <- cedar_myreports_local_dir
}
message("[parse-data.R] MyReports directory set to: ", myreports_dir)


message("[parse-data.R] Looking for .xlsx files in data directory: ", myreports_dir)
if (!dir.exists(myreports_dir)) {
  stop("[parse-data.R] ERROR: Data directory does not exist: ", myreports_dir)
}

# Get all relevant .xlsx files in the MR downloads directory
message("[parse-data.R] getting list of files to process...")
file_list <- list.files(myreports_dir, pattern = "\\.xlsx$", full.names = TRUE)
if (length(file_list) == 0) {
    message("[parse-data.R] No .xlsx files found in: ", myreports_dir, ".")
  } else {
    message("[parse-data.R] Found ", length(file_list), " total .xlsx files.")
  }

# loop through reports as specified in command line
message("[parse-data.R] processing reports: ", paste(report_list, collapse = ", "))

for (report in report_list) {
  # for studio testing
  # report <- report_list[[1]]
  
  message("[parse-data.R] processing report type: ", report)

  # get report specs
  message("[parse-data.R] Getting report specs...")
  if (!report %in% names(report_specs)) {
    stop("[parse-data.R] ERROR: Invalid report type specified: ", report, ". Valid options are: ",
         paste(names(report_specs), collapse = ", "), call.=FALSE)
  }
  report_spec <- report_specs[[report]]
  
  # Filter files by filename signature
  sig <- report_spec$filename_sig
  sig_list <- file_list[grepl(sig, basename(file_list), ignore.case = TRUE)]

  # If no relevant files found, print a message
  if (length(sig_list) == 0) {
    message("[parse-data.R] No .xlsx files found in: ", myreports_dir, " matching signature: ", sig)
  } else {
    message("[parse-data.R] Found ", length(sig_list), " .xlsx files.")
  }

  # Process each file in the list of report type
  for (file in sig_list) {
    # uncomment for studio testing
    # file <- file_list[1]

    message("\n[parse-data.R] Processing file: ", file, "..." )

    # recognize if rebuilding database by detecting no file
    # TODO: am i looking for local or cloud file or both?
    message("[parse-data.R] Setting existing Rds data dir...")
    if (is_docker()) {
      rds_data_dir <- "./data"
    } else {
      rds_data_dir <- cedar_data_dir
    }
    message("[parse-data.R] rds_data_dir set to: ", rds_data_dir)

    message("[parse-data.R] Loading previous data...")
    # Use appropriate extension based on cedar_use_qs config
    ext <- get_data_extension()
    oldfile <- file.path(rds_data_dir, paste0(report_spec$data_file, ext))
    message("[parse-data.R] existing datafile: ", oldfile)

    # check if old data file exists
    if (file.exists(oldfile)) {
      old_data <- load_cedar_data(oldfile)
      message("[parse-data.R] Loaded ", nrow(old_data), " rows.")
      rebuild <- FALSE
    }
    else {
      message("[parse-data.R] No previous data found.")
      old_data <- tibble()
      rebuild <- TRUE
    }

    message("[parse-data.R] Loading latest data...")

    check_memory("before loading Excel file")

    # Convert xlsx to csv using external tool (more reliable for large/complex files than readxl)
    xlsx_file <- file
    csv_file <-  file.path(rds_data_dir,paste0(report_spec$data_file,".csv"))
    message("[parse-data.R] Converting xlsx to csv: ", xlsx_file, " -> ", csv_file)

    # Run the conversion with error checking
    tryCatch({
      result <- system2("xlsx2csv", args = c(xlsx_file, csv_file), 
                       stdout = TRUE, stderr = TRUE)
      
      # Check if CSV was created and has content
      if (!file.exists(csv_file)) {
        stop("xlsx2csv did not create output file: ", csv_file)
      }
      
      if (file.size(csv_file) == 0) {
        stop("xlsx2csv created empty file: ", csv_file)
      }
      
      message("[parse-data.R] Successfully converted xlsx to csv (", 
              round(file.size(csv_file) / 1024 / 1024, 2), " MB)")
      
    }, error = function(e) {
      stop("[parse-data.R] ERROR: xlsx2csv conversion failed: ", e$message, 
           "\nMake sure xlsx2csv is installed (pip install xlsx2csv)")
    })

    # Now read the CSV
    new_data <- fread(file.path(csv_file))

    new_data <- new_data %>%
      filter(
        !if_all(everything(), ~ is.na(.) | trimws(.) == "")
      )

    message("[parse-data.R] Loaded ",nrow(new_data) ," rows from CSV file.")

    check_memory("after loading CSV data")

    # remove any data from new term present in old data
    if (!rebuild) {
      message("[parse-data.R] Filtering out current term data in old data...")
      new_term <- unique(na.omit(new_data[[{{report_spec[["term_col"]]}}]]))
      message("[parse-data.R] New term: ", new_term)
      old_data <- old_data %>% filter(!(!!as.symbol(report_spec[["term_col"]]) %in% new_term))
      message("[parse-data.R] old_data now has ",nrow(old_data) ," rows.")
    }

    message("[parse-data.R] Adding as_of_date column...")

    # Extract download date from filename, supplied by MyReports
    file_date <- str_extract(file, "[0-9]{4}[0-9]{2}[0-9]{2}")
    
    # Add column as_of_date so we know how recent data is
    new_data$as_of_date <- ymd(file_date)
    
    # source appropriate parser based on report type
    parser_file <- file.path(parsers_dir, report_spec$parser)
    message("[parse-data.R] parser file set to: ", parser_file)
    if (!file.exists(parser_file)) {
      stop("[parse-data.R] ERROR: Parser file not found: ", parser_file)
    }
    message("[parse-data.R] Sourcing parser...")
    source(parser_file) # defines parse function for report type

    message("[parse-data.R] Parsing new data...")
    new_data <- parse(new_data)

    # encrypt student IDs if ID_col exists 
    # only encrypt new data!
    message("[parse-data.R] checking new_data for ID cols...")
    if (!is.null(report_spec) && !is.null(report_spec$ID_col) ) {
      for (col in report_spec$ID_col) {
        if (!col %in% names(new_data)) {
          stop("[parse-data.R] ERROR: ID column not found in data: ", col)
        }
        message("encrypting ID column: ", col, "...")
        new_data[[col]] <- as.character(new_data[[col]])
        new_data[[col]] <- sapply(new_data[[col]], digest::digest, algo = "md5")
      } # end for each ID_col
    }
    
    
    # TODO handle different number of columns
    # meanwhile, print out the diffs for some debug info
    if (!rebuild) {
      message("[parse-data.R] cols in OLD not in NEW data")
      print(setdiff(names(old_data),names(new_data)))

      message("[parse-data.R] cols in NEW not in OLD data")
      print(setdiff(names(new_data),names(old_data)))
      
      # combine data
      message("combining new data with old data...")
      common_cols <- intersect(names(old_data), names(new_data))

      # Convert to data.table for memory-efficient operations
      old_dt <- as.data.table(old_data)
      new_dt <- as.data.table(new_data)
      rm(old_data, new_data)
      gc(verbose = FALSE)  # Reclaim memory after removing large objects

      for (col in common_cols) {
        # If either is character, coerce both to character
        if (is.character(old_dt[[col]]) || is.character(new_dt[[col]])) {
          old_dt[, (col) := as.character(get(col))]
          new_dt[, (col) := as.character(get(col))]
        }
      }
      
      # Use data.table's efficient row binding
      data <- rbindlist(list(old_dt, new_dt), use.names = TRUE, fill = TRUE)
      rm(old_dt, new_dt)
      gc(verbose = FALSE)  # Reclaim memory after combining data
      
      # Convert back to tibble if needed
      data <- as_tibble(data)
    } 
    else {
      message("[parse-data.R] No old data; no need to combine anything.")
      data <- new_data
    }

    message("[parse-data.R] First 5 rows:")
    print(head(data, 5))

    message("[parse-data.R] Last 5 rows:")
    print(tail(data, 5))


    # figure out where to save the data
    if (is_docker()) {
      cedar_data_archive_dir <- NULL # no archiving in Docker
    } else { # running locally
      # everything should be set in config.R
    }

    message("[parse-data.R] Reminder: rds_data_dir set to ", rds_data_dir)

    # Use appropriate file extension based on cedar_use_qs config flag
    ext <- get_data_extension()
    data_file <- file.path(rds_data_dir, paste0(report_spec$data_file, ext))
    message("[parse-data.R] Saving data file: ", data_file, "...")

    # stop(message="stopping before actual save for testing purposes.")

    tryCatch({
      save_cedar_data(data, file = data_file)
      if (file.exists(data_file)) {
        message("[parse-data.R] File successfully saved: ", data_file)
        message("[parse-data.R] saved ",nrow(data) ," rows.")
        message("[parse-data.R] removing original .xlsx file: ", file, "...")
        file.remove(file) # remove original .xlsx file after saving
        message("[parse-data.R] original .xlsx file removed.")
      } else {
        message("[parse-data.R] ERROR: File was not saved: ", data_file)
      }
    }, error = function(e) {
      message("[parse-data.R] ERROR during save: ", e$message)
    })


    # if data archiving enabled, archive downloaded file to archive folder
    # defined in config.R (or shiny-config.R)
    if (exists("cedar_cloud_data_dir") && !is.null(cedar_data_archive_dir)) {
      archive_dir <- file.path(cedar_data_archive_dir, report_spec$dir)
      message("[parse-data.R] moving .xlsx file to archive folder: ", archive_dir, "...")
      
      filepath <- as.character(file)
      
      file.copy(to =   paste0(archive_dir, basename(filepath)),
                from = filepath)
      #file.remove(from = filepath)

      message("[parse-data.R] xlsx file archived.")
    } else {
      message("[parse-data.R] No archiving directory specified; skipping archiving of data files.")
    } # end if archiving data files
    
    message("[parse-data.R] Done processing file: ", file)
  } # end process excel file
  
  message("[parse-data.R] Finished processing report type: ", report)
} # end report loop

message("[parse-data.R] All done in process_reports!")
} # end process_reports function



# ---- MAIN ---------
message("[parse-data.R] Welcome to parse-data!")
message("[parse-data.R] Setting up option parser...")
suppressPackageStartupMessages(library(optparse))
option_list = list(
  make_option(c("-r","--report"), help="specifies what report to process. separate by commas without spaces if multiple. default is all (desr, cl, as, deg).", metavar="character"),
  make_option(c("--guide"), default=FALSE, action="store_true", help="show instructions and options for specified function.")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
message("[parse-data.R] Parsed options: ", str(opt))

# check if report is specified
if (is.null(opt$report) || opt$report == "") {
  message("[parse-data.R] No report specified. Using all (desr, cl, as, deg) by default.")
  opt$report <- names(report_specs) 
} else {
  message("[parse-data.R] Processing report(s): ", opt$report)
}

# check if guide is requested
if (opt$guide) {
  message("Showing guide for process_reports function...")
  message("Available reports: ", paste(names(report_specs), collapse = ", "))
  message("Use --report to specify which report(s) to process.")
}

# Call the function with command line arguments
process_reports(
  report = opt$report,
  guide = opt$guide
)

# ---- END MAIN -----