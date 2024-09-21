# CROSSLIST FILTER
xlist_filter <- function(df,action) {
message("Welcome to xlist.R!")
  
  if (action == "exclude") {
    print("excluding cross-listed courses (by XL_CRN = 0)...")
    df <- df %>% filter(XL_CRN == "0")
    return(df)
  }
  
  
  # home will filter out all xled rows of a course except the one that matches dept filtering
  # best used when looking at a single dept's courses
  if (action == "home") {
    print("filter XL entries to match subj or dept filter")
    
    # NEED BETTER WAY OF FILTERING OUT COURSES THAT ORIGINATE FROM OUTSIDE A DEPT
    
    # probably best way is to use HR list to filter by appointment.
    # if not appointed in dept, then it shouldn't show up. 
    
    # ONE QUICK WAY IS TO keep only rows that aren't xled or if the xl subject is same as subject filter
    # this weeds out courses that are xled with only other depts
    # but it keeps courses xled within the dept, EVEN IF they are also xled outside
    # DONT DO THIS! Because it eliminates xled courses that aren't xled within "home" dept (most, except for ccs)
    df <- df %>% filter (XL_SUBJ == "0" | XL_SUBJ == SUBJ)
    return(df)
  }
  
  
  
  # compress will compress all the rows of a xled course into one, using the subj code that has highest enrollment
  # best used when looking at course lists across departments
  if (action == "compress") {
    print("compressing XL entries into single course...")
    
    # need to get XL_CODES that aren't 0 and set them aside to better filter XL courses
    non_xl <- df %>% filter (XL_CODE == "0")
    xl_only <- df %>% filter (XL_CODE != "0")
    
    # group by code and order to get highest enrolled listed first 
    # we'll consider this the "home" dept even though there isn't such a thing technically.
    xl_only <- xl_only %>% group_by(XL_CODE) %>% arrange(desc(ENROLLED))
    
    # grab non-dupes (paired by term and xl_code, since xl_codes are unique only within each term)
    xl_only <- xl_only[!duplicated(xl_only[ , c("TERM", "XL_CODE")]), ]  # Apply duplicated
    
    # output for inspection
    #xl_only  %>%  arrange(TERM,desc(total_enrl)) %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
    
    # now that i have just one entry for each xl'ed course, combine with all non-xled courses for full list
    print("combining XLed AND non-XL courses...")
    output <- rbind(xl_only, non_xl) %>% arrange(TERM,desc(total_enrl))
    return(output)
  }
}