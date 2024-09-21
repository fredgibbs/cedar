# Data for this report should be gathered through the MyReports report "Graduates_and_Pending_Graduates_Guided_Adhoc"
# Select all detail fields for future use, though most are not used now.
# It IS possible to select more than one semester at a time: get Fall 2019 through present
# Remove any old files and save resulting Excel file in data/downloads/degrees

# NOTE: Be sure to run this script from the data/parsers folder.

pacman::p_load(tidyverse, readxl, fs, data.table, lubridate)

source("load-parser-includes.R")

# get list of files in downloads folder and process one by one
message("getting list of downloaded files...")
file_list <- dir_ls(paste0(cedar_data_dir,'/downloads/degrees/'))

for (file in file_list) {
  # uncomment for studio testing
  #file <- file_list[1]
  
  # load existing course data
  message("loading degree data for previous terms...")
  load(paste0(cedar_data_dir,"/processed/degrees.Rda")) # loads DF degrees
  old_degrees <- degrees
  message("loaded ",nrow(old_degrees) ," rows.")
  
  message("loading latest graduates data...")
  new_degrees <- read_xlsx(file)
  message("loaded ",nrow(new_degrees) ," rows.")
  
  message("excel file loaded. processing data...")
  
  # remove old enrollment data for term of current DESR 
  # replace it with new data from most recent DESR
  message("filtering out current term data from old data...")
  file_term <- unique(new_degrees$`Academic Period Code`)
  old_degrees <- old_degrees %>% filter (`Academic Period Code` != file_term)
  
  # Extract download date from filename, supplied by MyReports
  file_date <- str_extract(file, "[0-9]{4}[0-9]{2}[0-9]{2}")
  
  # Add column as_of_date so we know how recent data is
  new_degrees$as_of_date <- ymd(file_date)
  
  new_degrees <- distinct(new_degrees)
  
  message("done processing latest degrees data.") 
  
  # Fold new data into original courses.Rda (courses_from_completed_terms.Rda)
  # skip if resetting the Rda file
  message("Adding new degrees data to previous degrees data...")
  degrees <- rbind(old_degrees, new_degrees)
  
  # uncomment for resetting Rda file (but don't reset unless we can get all old data from MyReports)
  degrees <- new_degrees
  
  message("saving degrees.Rda file in the data-processed folder...")
  save(degrees,file=paste0(cedar_data_dir,"/processed/degrees.Rda"))
  
  # IF data archiving enabled, archive downloaded file to archive folder (from config.R)
  if (!is.null(cedar_data_archive_dir)) {
    message("moving .xlsx file to archive folder...")
    
    filepath <- as.character(file)
    
    file.copy(to =   paste0(cedar_data_archive_dir,"degrees/", basename(filepath)),
              from = filepath)
    file.remove(from = filepath)
    
    message("xlsx file archived.")
  } # end if archiving data files
} # end process excel file

message("all done!")