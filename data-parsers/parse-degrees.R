# Data for this report should be gathered through the MyReports report "Graduates_and_Pending_Graduates_Guided_Adhoc"
# Select all detail fields for future use, though most are not used now.
# It IS possible to select more than one semester at a time: always get Fall 2019 through present term (or whatever end date)
# Remove any old files and save resulting Excel file in data/downloads/degrees

# NOTE: Be sure to run this script from the data/parsers folder.

pacman::p_load(tidyverse, readxl, fs, data.table, lubridate)

source("load-parser-includes.R")

# get list of files in downloads folder and process one by one
message("getting list of downloaded files...")
file_list <- dir_ls(paste0(cedar_data_dir,'/downloads/degrees/'))

for (file in file_list) {
  # uncomment for studio testing
  file <- file_list[1]
  
  # check for and load existing course data
  existing_file <- paste0(cedar_data_dir,"/processed/degrees.Rda")
  
  if (file.exists(existing_file)) {
    old_data_exists <- TRUE
    message("loading degree data for previous terms...")
    load(existing_file) # loads DF degrees
    old_degrees <- degrees
    message("loaded ",nrow(old_degrees) ," rows.")
    
    #TODO: see what range of terms we have in new degree data
    
  } else {
    old_data_exists <- FALSE
    message("no existing degrees data found.")
  }
  
  # load new data
  message("loading latest graduates data...")
  new_degrees <- read_xlsx(file)
  message("loaded ",nrow(new_degrees) ," rows.")
  
  message("excel file loaded.")
  
  # TODO: check for col differences
  #setdiff(names(old_degrees),names(new_degrees))
  #setdiff(names(new_degrees),names(old_degrees))
  
  
  # TODO: below assumes we only have a single term, which may not be true.
  # remove old enrollment data
  # replace it with new data from most recent DESR
  #message("filtering out current term data from old data...")
  #file_term <- unique(new_degrees$`Academic Period Code`)
  #old_degrees <- old_degrees %>% filter (`Academic Period Code` != file_term)
  
  # Extract download date from filename, supplied by MyReports
  message("gathering download date from filename...")
  file_date <- str_extract(file, "[0-9]{4}[0-9]{2}[0-9]{2}")
  
  # Add column as_of_date so we know how recent data is
  message("adding as_of_date column to new degrees data...")
  new_degrees$as_of_date <- ymd(file_date)
  
  new_degrees <- distinct(new_degrees)
  
  message("done processing latest degrees data.") 
  
  # Fold new data into original data; skip if resetting the Rda file
  if (old_data_exists) {
    message("Adding new degrees data to previous degrees data...")
    degrees <- rbind(old_degrees, new_degrees)
    message("done combining degrees data.")
  } else {
    degrees <- new_degrees
  }
  
  # save the new data
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