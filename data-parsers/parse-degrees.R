parse <- function (new_students) {
  
  # remove extraneous fields
  new_students <- new_students %>% select (-c(`Student Preferred First Name`, `Student First Name`,`Student Last Name`,`Student Middle Initial`,`Confidentiality Indicator`,`Email Address`,`NetID`,`Street Line 1`,`Street Line 2`,`City`,`State/Province`,`Zipcode`,`Phone Number`,`Visa Type`,`Visa Type Code`)) 
  
  return (new_students)
  
}