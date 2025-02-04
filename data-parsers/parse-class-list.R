# this script parses the Excel file export of the  Class List Guided Adhoc Report from MyReports
# it loads existing data, adds new data, and once combined, does minor processing for easier use
# this is inefficient in loading data, but ensures new transformations or lookups get applied to older data as well.

pacman::p_load(tidyverse, readxl, fs, data.table, lubridate,feather)

message("Welcome to parse-class-list!")

# load basic includes; manually set working dir if testing in RStudio
# setwd('~/Dropbox/cedar/data-parsers')
source("load-parser-includes.R")

# get list of files in downdloads folder and process one by one
message("getting list of downloaded files...")
file_list <- dir_ls(paste0(cedar_data_dir,'downloads/class-lists'))

for (file in file_list) {
  # uncomment for studio testing
  # file <- file_list[1]
  
  message("loading previous data...")
  old_students <- read_feather(paste0(cedar_data_dir,"/processed/class_list.feather"))
  message("loaded ",nrow(old_students) ," rows.")
  
  message("loading latest class list...")
  new_students <- read_xlsx(file)
  message("loaded ",nrow(new_students) ," rows.")
  
  message("excel file loaded. processing data...")
  
  # TODO check to see if expected number of columns
  # setdiff(names(old_students),names(new_students))
  # setdiff(names(new_students),names(old_students))
  
  # get term code from new file and remove data of same term from previous data
  message("removing old data with term of new file...")
  current_term <- new_students$`Academic Period Code`[1]
  old_students <- old_students %>% filter (`Academic Period Code` != current_term)
  
  # filter by campus (abq and online)
  message("filtering by campus...")
  new_students <- new_students %>% filter (`Course Campus Code` == "ABQ" | `Course Campus Code` == "EA") 
  
  
  # Extract date from filename, supplied by MyReports
  file_date <- str_extract(file, "[0-9]{4}[0-9]{2}[0-9]{2}")
  
  # Add column as_of_date for registration tracking.
  new_students$as_of_date <- ymd(file_date)
  
  # add helper columns for easier display
  new_students <- new_students %>% unite(CRSE_SECT, c("Course Number", "Course Section Number"), sep="-", remove=FALSE)
  new_students <- new_students %>% unite(CRSE_TITLE, c("Course Number", "Short Course Title"), sep=": ", remove=FALSE)
  new_students <- new_students %>% unite(SUBJ_CRSE, c("Subject Code", "Course Number"), sep=" ", remove=FALSE)
  
  # preserve order of operations here
  # set crse_base field as int for easier sorting (without L or X,Y,Z, etc)
  new_students$crse_base <- new_students$`Course Number`
  
  # find labs via course number (has an "L" and set new col flag)
  new_students$lab <- ifelse ( grepl("[[:alpha:]]", new_students$crse_base),TRUE,FALSE)
  
  # create crse_base col with only crse NUMBER (no chars)
  new_students$crse_base[new_students$lab == TRUE] <- substring(new_students$crse_base[new_students$lab == TRUE],1, nchar(new_students$crse_base[new_students$lab == TRUE])-1)
  
  new_students$crse_base <- as.integer(new_students$crse_base)  
  
  
  # add level code for easier parsing
  message("creating level field...")
  new_students <- new_students %>%
    mutate(level = case_when(
      crse_base < 300 ~ "lower",
      crse_base >= 1000 ~ "lower",
      crse_base >= 500 & crse_base < 700 ~  "grad",
      crse_base >= 300 & crse_base < 500 ~ "upper"
    ))  
  
  # add term type col
  message("creating term type field...")
  new_students <- new_students %>%
    mutate(term_type = case_when(
      substring(`Academic Period Code`,5,6) == 80 ~ "fall",
      substring(`Academic Period Code`,5,6) == 10 ~ "spring",
      substring(`Academic Period Code`,5,6) == 60 ~ "summer"
    ))  
  
  # update old subject codes with modern ones
  message("updating subject codes...")
  new_students <- update_codes(new_students,"Subject Code")
  
  # create DEPT field that uses SUBJ and finds home DEPT
  message("creating DEPT field...")
  new_students$DEPT <- subj_to_dept_map[new_students$`Subject Code`]
  new_students <- new_students %>% mutate(DEPT = ifelse(is.na(DEPT), `Subject Code`, DEPT))
  
  # get rid of unnecessary fields
  # TODO: probably many more could be removed to improve effiency
  new_students <- new_students %>% select (-c(`Student First Name`,`Student Last Name`,`Confidentiality Indicator`,`Student Email Address`,`Student Preferred First Name`,`Student NetID`,`Street Line 1`,`Street Line 2`,`City`,`County`,`Zip Code`,`Nation`,`Phone Number`,`Visa Type`,`Student Population Code`,`Student Population`))
  
  # remove dupes after reducing number of fields from original data
  new_students <- new_students %>% distinct()

  message("done processing class list Excel file.") 
  
  # combine new data with old data
  message("combining new data with old data...")
  students <- rbind(old_students,new_students)
  
  # uncomment if resetting old data file...
  # students <- new_students
  
  
  message("saving feather file...") 
  write_feather(students,paste0(cedar_data_dir,"/processed/class_list.feather"))
  message("saved ",nrow(students) ," rows.")
  
  # IF data archiving enabled, archive downloaded file to archive folder (from config.R)
  if (!is.null(cedar_data_archive_dir)) {
    message("moving .xlsx file to archive folder...")
    
    filepath <- as.character(file)
    
    file.copy(to =   paste0(cedar_data_archive_dir,"class-lists/", basename(filepath)),
              from = filepath)
    file.remove(from = filepath)
    
    message("xlsx file archived.")
  } # end if archiving excel file
} # end process excel file

message("all done in parse-class-list.")