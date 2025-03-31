parse <- function (new_courses) {

# function to merge HR data with course data
# this allows us to associate title with person in course listings
.merge_hr_data <- function (courses) {
  
  ############ merge personnel data with course data
  message("\nwelcome to merge_hr_data!")
  
  # get faculty data to associate title with person in course listings
  message("loading faculty data...")
  fac_by_term <- load_hr_data()

  message("adjusting data types...")
  courses$`PRIM_INST_ID` <- as.double(courses$`PRIM_INST_ID`)
  courses$`TERM` <- as.character(courses$`TERM`)
  
  # disregard as_of_date in HR data
  fac_by_term <- fac_by_term %>% select (-c(as_of_date))
  
  # merge faculty and course data
  # do not via DEPT fields because means an instructor w/o an appt % or admin appt with that DEPT 
  # will not have a title listed on the enrl reporting tools
  message("merging faculty data with course data...")
  courses <- merge(courses,fac_by_term,by.x=c("TERM","PRIM_INST_ID"),by.y=c("term_code","UNM ID"),all.x=TRUE)
  courses <- courses %>% select (-c(DEPT.y))
  courses <- courses %>% rename (DEPT = DEPT.x)
  
  message("done merging HR data.")
  
  return(courses)
} # end merge_hr_data


# rename columns for easier reference
message("renaming columns for easier reference...")
new_courses <- rename(new_courses,"CRSE" = "CRSE#")
new_courses <- rename(new_courses,"SECT" = "SECT#")
new_courses <- rename(new_courses,"XL_CRSE" = "XL_CRSE#")
new_courses <- rename(new_courses,"XL_ENRL" = "XL_TOTAL_ENROLLMENT")
new_courses$crse_base <- new_courses$CRSE

# add helper columns for easier display and filtering
message("adding helper columns that join existing columns...")
new_courses <- new_courses %>% unite(SUBJ_CRSE, c("SUBJ", "CRSE"), sep=" ", remove=FALSE)
new_courses <- new_courses %>% unite(CRSE_SECT, c("CRSE", "SECT"), sep="-", remove=FALSE)
new_courses <- new_courses %>% unite(CRSE_TITLE, c("CRSE", "SECT_TITLE"), sep=": ", remove=FALSE)
new_courses <- new_courses %>% unite(INST_NAME, c("PRIM_INST_LAST","PRIM_INST_FIRST"), sep=", ", remove=FALSE)

new_courses <- distinct(new_courses)

# find labs via crse number (has an "L" and set new col flag)
message("creating course base from lab sections...")
new_courses$lab <- ifelse ( grepl("[[:alpha:]]", new_courses$crse_base),TRUE,FALSE)

# create new col with only crse NUMBER (no chars)
new_courses$crse_base[new_courses$lab == TRUE] <- substring(new_courses$crse_base[new_courses$lab == TRUE],1, nchar(new_courses$crse_base[new_courses$lab == TRUE])-1)

# adjust data types
message("adjusting data types...")
new_courses$XL_ENRL[is.na(new_courses$XL_ENRL)] <- 0
new_courses$XL_ENRL <- as.integer(new_courses$XL_ENRL)
new_courses$ENROLLED <- as.integer(new_courses$ENROLLED)
new_courses$crse_base <- as.integer(new_courses$crse_base)

# make easy enrollment column that combines reg and xl sections
# make sure this is done before crosslisting filtering
message("adding total_enrl field...")
new_courses <- new_courses %>% mutate(total_enrl = ifelse (ENROLLED > XL_ENRL, ENROLLED, XL_ENRL))
new_courses$total_enrl <- as.numeric(new_courses$total_enrl)

# add level column for easier filtering
message("adding level field...")
new_courses <- new_courses %>%
  mutate(level = case_when(
    crse_base < 300 ~ "lower",
    crse_base >= 1000 ~ "lower",
    crse_base >= 500 & crse_base < 700 ~  "grad",
    crse_base >= 300 & crse_base < 500 ~ "upper"
  ))  

# add term_type col based on Academic_Period_Code
message("creating term type field...")
new_courses <- add_term_type_col (new_courses, "TERM")

# add gen ed code
# see includes/mappings.R for lists of courses in each gen ed area
message("add gen_ed_area field...")
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

# merge with HR data
# this is largely just to preserve existing functionality that depends on job_cat field in dept reporting
# needs to have less precarious HR data access to keep data updated
new_courses <- .merge_hr_data(new_courses)

message("done processing latest DESR.")
return (new_courses)

}
