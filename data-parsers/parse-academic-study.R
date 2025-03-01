# this script loads downloaded excel file(s) from the academic study guided ad-hoc report 
# and produces data/processed/academic-study.feather

pacman::p_load(tidyverse, readxl, fs, data.table, lubridate,feather)

message("Welcome to parse-academic-study!")

# load basic includes 
source("load-parser-includes.R")

# get list of files in downdloads folder and process one by one
message("getting list of downloaded files...")
file_list <- dir_ls(paste0(cedar_data_dir,'downloads/academic-study'))

for (file in file_list) {
  
  # uncomment for studio testing and stepping through each file
  # file <- file_list[1]

  # recognize if rebuilding database by detecting no file
  # this reloads existing file even if parsing multiple files; not the most efficient but a good check.
  message("loading previous data...")
  oldfile <- paste0(cedar_data_dir,"processed/academic-study.feather")
  if (file.exists(oldfile)) {
    old_students <- read_feather(paste0(cedar_data_dir,"processed/academic-study.feather"))
    message("loaded ",nrow(old_students) ," rows.")
    rebuild <- FALSE
  }
  else {
    message("no previous academic study data found.")
    old_studdents <- tibble()
    rebuild <- TRUE
  }
  
  message("loading latest student data...")
  new_students <- read_xlsx(file, col_types = "text")
  message("loaded ",nrow(new_students) ," rows. processing...")
  
  if (!rebuild) {
    # get term code from new file and remove that term's data from previous data
    # assumes there is only one term per file, which is always true unless myreports changes
    message("removing old data duplicated in new file (by term)...")
    current_term <- new_students$`Academic Period`[1] 
    old_students <- old_students %>% filter (`Academic Period` != current_term)
  }
  
  # Extract date from filename, supplied by MyReports
  file_date <- str_extract(file, "[0-9]{4}[0-9]{2}[0-9]{2}")
  
  # Add column as_of_date for historic tracking
  new_students$as_of_date <- ymd(file_date)
  
  # remove noise in the xslx file 
  new_students <- new_students %>% drop_na(`Academic Year`)
  
  # add column for term_code based on Academic Period column
  # see lists.R for term_code and term_text definitions
  message("adding term_code column based on Academic Period column...")
  term_code_lookup <- data.frame(term_code = num.labs, `Academic_Period` = term_text)
  new_students <- (merge(new_students, term_code_lookup, by.x = 'Academic Period', by.y = 'Academic_Period'))
  
  # add term_type col based on term_code
  message("creating term type field...")
  new_students <- add_term_type_col (new_students, "term_code")
  
  # remove Pre from major and add boolean flag in separate column
  message("removing Pre and Pre- from Major field and setting boolean flag in 'pre' column")
  new_students$pre <- ifelse ( grepl("Pre", new_students$Major),TRUE,FALSE)
  new_students$Major <- str_remove(new_students$Major, "Pre ")
  new_students$Major <- str_remove(new_students$Major, "Pre-")
  
  # compute program and departments based on the Major column
  message("computing programs and departments based on Major, Second Major, First Minor, Second Minor...")
  new_students$major_PRGM <- major_to_program_map[new_students$Major]
  new_students$major_DEPT <- prgm_to_dept_map[new_students$major_PRGM]
  new_students <- new_students %>% mutate (major_DEPT = ifelse(is.na(major_DEPT), major_PRGM, major_DEPT))
  
  new_students$sec_major_PRGM <- major_to_program_map[new_students$`Second Major`]
  new_students$sec_major_DEPT <- prgm_to_dept_map[new_students$sec_major_PRGM]
  new_students <- new_students %>% mutate (sec_major_DEPT = ifelse(is.na(sec_major_DEPT), sec_major_PRGM, sec_major_DEPT))
  
  new_students$minor_PRGM <- major_to_program_map[new_students$`First Minor`]
  new_students$minor_DEPT <- prgm_to_dept_map[new_students$minor_PRGM]
  new_students <- new_students %>% mutate (minor_DEPT = ifelse(is.na(minor_DEPT), minor_PRGM, minor_DEPT))
  
  new_students$sec_minor_PRGM <- major_to_program_map[new_students$`Second Minor`]
  new_students$sec_minor_DEPT <- prgm_to_dept_map[new_students$sec_minor_PRGM]
  new_students <- new_students %>% mutate (sec_minor_DEPT = ifelse(is.na(sec_minor_DEPT), sec_minor_PRGM, sec_minor_DEPT))
  
  
  # see where majors did not get mapped to a program or department
  # we want to see there are no A&S Majors (in any col) that aren't getting mapped
  #message("The following are missing PRGM codes:")
  #cat(unique(new_students$Major[is.na(new_students$major_PRGM)]),sep="\n")
  
  #message("check for missing DEPT codes:")
  #cat(unique(new_students$Major[is.na(new_students$major_DEPT)]),sep="\n")
  
  # TODO: check First Minor also since some minors are not majors?
  
  
  message("done processing academic study Excel file.") 
  
  
  # TODO handle different number of columns
  # meanwhile, print out the diffs for some clue
  if (!rebuild) {
    message("cols in OLD not in NEW data")
    print(setdiff(names(old_students),names(new_students)))
    
    message("cols in NEW not in OLD data")
    print(setdiff(names(new_students),names(old_students)))
    
    # combine data if not rebuilding 
    message("combining new data with old data...")
    students <- rbind(old_students,new_students)
  } 
  else {
    message("no old data; no need to combine anything.")
    students <- new_students  
  }
  
  message("saving feather file...") 
  write_feather(students,paste0(cedar_data_dir,"/processed/academic-study.feather"))
  message("saved ",nrow(students) ," rows.")
  
  
  # if data archiving enabled, archive downloaded file to archive folder (from config.R)
  if (!is.null(cedar_data_archive_dir)) {
    message("moving .xlsx file to archive folder...")
    
    filepath <- as.character(file)
    file.copy(to =   paste0(cedar_data_archive_dir,"academic-study/", basename(filepath)),
              from = filepath)
    file.remove(from = filepath)
    
    message("xlsx file archived.")
  }
  
} # end process excel file

message("all done in parse-academic-study.")