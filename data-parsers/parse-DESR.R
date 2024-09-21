# NOTE: this script must be run from the data/parsers folder
# no params are accepted

pacman::p_load(tidyverse, readxl, fs, data.table, lubridate)

message("Welcome to parse-DESR!")

# load basic includes 
source("load-parser-includes.R")

# get list of files in downdloads folder and process one by one
message("getting list of downloaded files...")
file_list <- dir_ls(paste0(cedar_data_dir,'downloads/DESRs'))

for (file in file_list) {
  # uncomment for studio testing
  # file <- file_list[1]
  
  # load existing course data
  message("loading DESR data for previous terms...")
  load(paste0(cedar_data_dir,"/processed/courses_with_final_and_latest_enrollments.Rda")) # loads DF completed_and_ongoing_courses
  old_courses <- completed_and_ongoing_courses
  message("loaded ",nrow(old_courses) ," rows.")
  
  message("loading latest DESR...")
  new_courses <- read_xlsx(file)
  message("loaded ",nrow(new_courses) ," rows.")
  
  message("excel file loaded. processing data...")
  
  # remove old enrollment data for term of current DESR 
  # replace it with new data from most recent DESR
  message("filtering out current term data from old data...")
  DESR_term <- unique(new_courses$TERM)
  old_courses <- old_courses %>% filter (TERM != DESR_term)
  
  # filter by campus (abq and online)
  new_courses <- new_courses %>% filter (CAMP=="ABQ" | CAMP=="EA") 
  
  # Extract download date from filename, supplied by MyReports
  file_date <- str_extract(file, "[0-9]{4}[0-9]{2}[0-9]{2}")
  
  # Add column as_of_date so we know how recent data is
  new_courses$as_of_date <- ymd(file_date)
  
  # rename columns for easier reference
  new_courses <- rename(new_courses,"CRSE" = "CRSE#")
  new_courses <- rename(new_courses,"SECT" = "SECT#")
  new_courses <- rename(new_courses,"XL_CRSE" = "XL_CRSE#")
  new_courses <- rename(new_courses,"XL_ENRL" = "XL_TOTAL_ENROLLMENT")
  new_courses$crse_base <- new_courses$CRSE
  
  # add helper columns for easier display and filtering
  new_courses <- new_courses %>% unite(SUBJ_CRSE, c("SUBJ", "CRSE"), sep=" ", remove=FALSE)
  new_courses <- new_courses %>% unite(CRSE_SECT, c("CRSE", "SECT"), sep="-", remove=FALSE)
  new_courses <- new_courses %>% unite(CRSE_TITLE, c("CRSE", "SECT_TITLE"), sep=": ", remove=FALSE)
  new_courses <- new_courses %>% unite(INST_NAME, c("PRIM_INST_LAST","PRIM_INST_FIRST"), sep=", ", remove=FALSE)

  new_courses <- distinct(new_courses)
  
  # find labs via crse number (has an "L" and set new col flag)
  new_courses$lab <- ifelse ( grepl("[[:alpha:]]", new_courses$crse_base),TRUE,FALSE)
  
  # create new col with only crse NUMBER (no chars)
  new_courses$crse_base[new_courses$lab == TRUE] <- substring(new_courses$crse_base[new_courses$lab == TRUE],1, nchar(new_courses$crse_base[new_courses$lab == TRUE])-1)
  
  # adjust data types
  new_courses$XL_ENRL[is.na(new_courses$XL_ENRL), ] <- 0
  new_courses$XL_ENRL <- as.integer(new_courses$XL_ENRL)
  new_courses$ENROLLED <- as.integer(new_courses$ENROLLED)
  new_courses$crse_base <- as.integer(new_courses$crse_base)
  
  # make easy enrollment column that combines reg and xl sections
  # make sure this is done before crosslisting filtering
  message("adding total_enrl field...")
  new_courses <- new_courses %>% mutate(total_enrl = ifelse (ENROLLED > XL_ENRL, ENROLLED, XL_ENRL))
  new_courses$total_enrl <- as.numeric(new_courses$total_enrl)
  
  # add level column for easier filtering
  new_courses <- new_courses %>%
    mutate(level = case_when(
      crse_base < 300 ~ "lower",
      crse_base >= 1000 ~ "lower",
      crse_base >= 500 & crse_base < 700 ~  "grad",
      crse_base >= 300 & crse_base < 500 ~ "upper"
    ))  
  
  # add gen ed code
  # see includes/mappings.R for lists of courses in each gen ed area
  new_courses <- new_courses %>%
    mutate(gen_ed_area = case_when(
      SUBJ_CRSE %in% gen_ed_1_communication ~ 1,
      SUBJ_CRSE %in% gen_ed_2_math_stat ~ 2,
      SUBJ_CRSE %in% gen_ed_3_phys_nat_sci ~ 3,
      SUBJ_CRSE %in% gen_ed_4_soc_behav_sci ~ 4,
      SUBJ_CRSE %in% gen_ed_5_humanities ~ 5,
      SUBJ_CRSE %in% gen_ed_7_arts_design ~ 7
    ))  
  
  
  # map subjs to dept
  message("mapping subject codes to dept codes...")
  new_courses$DEPT <- subj_to_dept_map[new_courses$SUBJ]
  new_courses <- new_courses %>% mutate(DEPT = ifelse(is.na(DEPT), SUBJ, DEPT))
  
  # TODO: merge personnel data here instead of when loading?
  
  message("done processing latest DESR.") 
  
  # Fold new data into original courses.Rda (courses_from_completed_terms.Rda)
  # skip if resetting the Rda file
  message("Adding new DESR data to previous DESR data (most recent enrollments only)...")
  completed_and_ongoing_courses <- rbind(old_courses, new_courses)
  
  
  # uncomment for resetting Rda file (but don't reset unless we can get all old data from MyReports)
  # completed_and_ongoing_courses <- new_courses
  
  message("saving Rda file of completed and ongoing enrollments (by semester)...")
  save(completed_and_ongoing_courses,file=paste0(cedar_data_dir,"processed/courses_with_final_and_latest_enrollments.Rda"))
  #write.csv(completed_and_ongoing_courses,paste0(cedar_data_dir,"processed/courses_with_final_and_latest_enrollments.csv"))
  
  
  # IF data archiving enabled, archive downloaded file to archive folder (from config.R)
  if (!is.null(cedar_data_archive_dir)) {
    message("moving .xlsx file to archive folder...")
    
    filepath <- as.character(file)
    
    file.copy(to =   paste0(cedar_data_archive_dir,"DESRs/", basename(filepath)),
              from = filepath)
    file.remove(from = filepath)
    
    message("xlsx file archived.")
  } # end if archiving data files
} # end process excel file

message("all done in parse-DESR.")