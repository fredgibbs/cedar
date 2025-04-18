# run this script from the data-parsers folder
pacman::p_load(optparse, tidyverse, readxl, fs, data.table, lubridate)

message("Welcome to parse-data!")

# load basic includes 
source("../includes/config.R")
source(paste0(cedar_base_dir,"/includes/mappings.R"))
source(paste0(cedar_base_dir,"/includes/lists.R"))
source(paste0(cedar_base_dir,"/includes/gen_ed_courses.R"))
source(paste0(cedar_base_dir,"/includes//misc_funcs.R"))

# define details for each kind of MyReports report
report_specs <- list(
  desr = list(
    dir = "DESRs/",
    data_file = "DESRs",
    term_col = "TERM",
    parser = "parse-DESR.R"
  ),
  cl = list(
    dir = "class_lists/",
    data_file = "class_lists",
    term_col = "Academic Period",
    ID_col = c("Primary Instructor ID", "Student ID"),
    parser = "parse-class-list.R"
  ),
  as = list(
    dir = "academic_studies/",
    data_file = "academic_studies",
    term_col = "Academic Period",
    ID_col = "ID",
    parser = "parse-academic-study.R"
  ),
  deg = list(
    dir = "degrees/",
    data_file = "degrees",
    term_col = "Academic Period",
    ID_col = "ID",
    parser = "parse-degrees.R"
  )
)

# define options for command line
option_list = list(
  make_option(c("-r","--report"), # options are "desr","cl","as","deg"
              help="specifies what report to process."), 
  
  make_option(c("--guide"), default=FALSE, action="store_true",
              help="show instructions and options for specified function."),
  
  make_option(c("-t", "--term"), type="character", 
              help="term code (i.e. 202410)", metavar="character"),
  
  make_option(c("--onedrive"),  default=FALSE, action="store_true",
              help="us to automatically save file to ondrive directory as specified in config.R", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);


if (is.null(opt$report)){
  stop("No function (-r or --report) specified. Specify '-f guide' to see options. ", call.=FALSE)
}

# uncomment for studio testing
# opt <- list()
# opt["report"] <- "desr"

# convert report to list
report_list <- convert_param_to_list(opt$report)


# loop through reports as specified in command line
for (report in report_list) {
  # uncomment for studio testing
  # report <- report_list[[1]]
  
  message("processing report type: ", report)
  
  # get report specs
  report_spec <- report_specs[[report]]
  
  # get list of files in downloads folder and process one by one
  message("getting list of downloaded files...")
  report_dir <- paste0(cedar_data_dir,'downloads/',report_spec$dir)
  file_list <- dir_ls(report_dir)
  
  if (length(file_list) == 0) {
    message("no downloaded files found in: ",report_dir)
  }
  
  for (file in file_list) {
    # uncomment for studio testing
    # file <- file_list[1]
    
    message("processing file: ", file, "..." )
    
    # recognize if rebuilding database by detecting no file
    # TODO: am i looking for local or cloud file or both?
    message("loading previous data...")
    oldfile <- paste0(cedar_data_dir,"processed/",report_spec$data_file,".Rds")
    
    if (file.exists(oldfile)) {
      old_data <- readRDS(oldfile)
      message("loaded ",nrow(old_data) ," rows.")
      rebuild <- FALSE
    }
    else {
      message("no previous data found.")
      old_data <- tibble()
      rebuild <- TRUE
    }
    
    message("loading latest data...")
    new_data <- read_xlsx(file, guess_max = 10000)
    message("loaded ",nrow(new_data) ," rows.")
    
    message("excel file loaded. processing data...")
    
    # remove any data from new term present in old data 
    if (!rebuild) {
      message("filtering out current term data in old data...")
      new_term <- unique(new_data[[{{report_spec[["term_col"]]}}]])
      message("new term: ", new_term)
      old_data <- old_data %>% filter (!!as.symbol(report_spec[["term_col"]]) != new_term)
      message("old_data now has ",nrow(old_data) ," rows.")
    }
    
    message("adding as_of_date column...")
    
    # Extract download date from filename, supplied by MyReports
    file_date <- str_extract(file, "[0-9]{4}[0-9]{2}[0-9]{2}")
    
    # Add column as_of_date so we know how recent data is
    new_data$as_of_date <- ymd(file_date)
    
    # source appropriate parser based on report type
    message("sourcing parser...")
    source(report_spec$parser) # defines parse function for report type
    new_data <- parse(new_data)
    
    
    # encrypt student IDs if ID_col exists 
    # only encrypt new data!
    message("checking new_data for ID cols...")
    if (!is.null(report_spec) && !is.null(report_spec$ID_col) ) {
      for (col in report_spec$ID_col) {
        if (!col %in% names(new_data)) {
          stop("ID column not found in data: ", col)
        }
        message("encrypting ID column: ", col, "...")
        new_data[[col]] <- as.character(new_data[[col]])
        new_data[[col]] <- sapply(new_data[[col]], digest::digest, algo = "md5")
      } # end for each ID_col
    }
    
    
    # TODO handle different number of columns
    # meanwhile, print out the diffs for some clue
    if (!rebuild) {
      message("cols in OLD not in NEW data")
      print(setdiff(names(old_data),names(new_data)))
      
      message("cols in NEW not in OLD data")
      print(setdiff(names(new_data),names(old_data)))
      
      # combine data
      message("combining new data with old data...")
      data <- rbind(old_data, new_data)
    } 
    else {
      message("no old data; no need to combine anything.")
      data <- new_data
    }
    

    filename <- paste0(cedar_data_dir,"processed/",report_spec$data_file,".Rds")
    message("saving Rds file: ", filename, "...")
    saveRDS(data,file=filename)
    message("saved ",nrow(data) ," rows.")
    
  
    # if data archiving enabled, archive downloaded file to archive folder (from config.R)
    if (!is.null(cedar_data_archive_dir)) {
      archive_dir <- paste0(cedar_data_archive_dir, report_spec$dir)
      message("moving .xlsx file to archive folder: ", archive_dir, "...")
      
      filepath <- as.character(file)
      
      file.copy(to =   paste0(archive_dir, basename(filepath)),
                from = filepath)
      file.remove(from = filepath)
      
      message("xlsx file archived.")
    } # end if archiving data files
  } # end process excel file
  
  # if cloud_data_dirs are specified in config.R, copy the Rds file to OneDrive
  if (!is.null(cedar_cloud_data_dir)) {
    cloud_filepath <- paste0(cedar_cloud_data_dir, report_spec$data_file,".Rds")
    local_filepath <- paste0(cedar_data_dir,"processed/",report_spec$data_file,".Rds")

    message("copying Rds FROM local data folder: ",local_filepath)
    message("copying Rds TO local OneDrive folder: ",cloud_filepath,"...")
    
    file.copy(to =   cloud_filepath,
              from = local_filepath,
              overwrite = TRUE)
    
    message("Rds file copied to OneDrive.")
  }
  
  
} # end report loop

message("all done in parse-data.")
