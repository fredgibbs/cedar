parse <- function (new_students) {

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
  # discard unnecessary fields
  new_students <- new_students %>% select (-c(`Student First Name`,`Student Last Name`,`Confidentiality Indicator`,`Email Address`,`Preferred First Name`,`NetID`,`Street Line 1`,`Street Line 2`,`City`,`County`,`County Code`,`Zip Code`,`State/Province`,`Phone Number`))
  
  message("done processing academic study Excel file.") 
  
return(new_students)
}