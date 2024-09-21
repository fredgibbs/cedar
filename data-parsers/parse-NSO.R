# this should be run from the "parsers" folder

pacman::p_load(tidyverse, readxl, fs, data.table, lubridate)

source("load-parser-includes.R")


# Load latest NSO
# TODO: check to make sure there is just one file available
message("getting list of files to parse...")
file_list <- dir_ls(paste0(cedar_data_dir,'/downloads/NSOs/'))

# TODO: skip first two rows from orig XL files (manually deleting and re-saving for now...)
xls <- map(file_list, read_xlsx)
NSO_registrants <- rbindlist(xls, fill=TRUE)

message("cleaning up data...")

# filter by students registered at ABQ campus (about 150 students in 2023 data)
# NSO_registrants <- NSO_registrants %>% filter (REGISTERED_ABQ == "Y") 

# Extract date from filename, supplied by MyReports
file_date <- str_extract(file_list, "[0-9]{4}[0-9]{2}[0-9]{2}")

# Add column as_of_date for registration tracking.
NSO_registrants$as_of_date <- ymd(file_date)


message("done processing latest NSO Excel file.") 

# TODO: get date/year from file data instead of hardcoding
message("saving data/processed/NSO-data-24.Rda...") 
save(NSO_registrants,file=paste0(cedar_data_dir,"processed/NSO-data-24.Rda"))


# move latest NSO file (excel file from MyReports) to archive folder
message("moving NSO report to data archive folder...")

latest_data_dir <- paste0(cedar_data_dir,'downloads/NSOs/')

filename <- list.files(latest_data_dir)

file.copy(to =   paste0(cedar_data_archive_dir,"NSOs/", filename),
          from = paste0(latest_data_dir, filename))

file.remove(from = paste0(latest_data_dir, filename))

message("all done!")
