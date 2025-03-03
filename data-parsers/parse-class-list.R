parse <- function (new_students) {
  
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
  
  # add term_type col based on Academic_Period_Code
  message("creating term type field...")
  new_students <- add_term_type_col (new_students, "Academic Period Code")

  # update old subject codes with modern ones
  message("updating subject codes...")
  new_students <- update_codes(new_students,"Subject Code")
  
  # create DEPT field that uses SUBJ and finds home DEPT
  message("creating DEPT field...")
  new_students$DEPT <- subj_to_dept_map[new_students$`Subject Code`]
  new_students <- new_students %>% mutate(DEPT = ifelse(is.na(DEPT), `Subject Code`, DEPT))
  
  # discard unnecessary fields
  new_students <- new_students %>% select (-c(`Primary Instructor Email`, `Primary Instructor Preferred First Name`,`Primary Instructor NetID`, `Student Name`, `Student First Name`,`Student Last Name`,`Confidentiality Indicator`,`Student Email Address`,`Student Preferred First Name`,`Student NetID`,`Street Line 1`,`Street Line 2`,`City`,`County`,`Zip Code`,`Nation`,`Phone Number`,`Visa Type`,`Registration User ID`))
  
  # remove dupes after reducing number of fields from original data
  new_students <- new_students %>% distinct()

  message("done processing class list Excel file.") 
  return(new_students)
}