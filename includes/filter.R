# this function inspects and parses a param to a vector (not list as it did originally)
# named vectors/lists should returned with their value
# commma separated strings should be converted to a list
# char vectors should be returned as is

# TODO: fix list vs vector throughout code
# TODO: handle a list/vector of named lists/vectors

convert_param_to_list <- function(param) {
  #message("\nWelcome to convert_param_to_list!")
  #print(str(param))
  
  # check if list type already; if so, return it
  if (is.list(param)) {
    #message("param is already list. returning it...")
    param_to_list <- param
    return(param_to_list)
  }
  #check for comma in param
  else if (length(param) == 1 && grepl(",", param)) {
    message("comma string detected...")
    param <- str_replace(param, ", ", ",")
    param <- strsplit(param, ",")[[1]]
    message("converting to list and returning...")
    param_to_list <- as.list(param)
    return(param_to_list)
  }
  # check if param is a named object (probably defined in includes/lists.R) 
  # TODO: need to be explicit about where to look; this sometimes finds an R-level entity
  else if (length(param) == 1 && exists(get("param"))) { 
    message("param already defined: ", get("param"))
    message(str(get(param)))
    if (param == "as" || param =="CJ") { # hack for now
     return (as.list(param))
    } 
    else return(get(param))
  }
  else if (is.character(param)) {
    #message("param is character. returning as list...")
    param_to_list <- as.list(param)
    return(param_to_list)
  }
  # quit if unsure what to do to prevent weird errors down the line
  else {
    stop(paste0("covert_param_to_list not sure what to do with supplied param: ", str(param)))
  }
}




#' Filter a data frame by a column and value(s)
#'
#' This function filters a data frame by a specified column and value(s). The column name is provided as a string,
#' and the value can be a vector, list, or comma-separated string. The function uses \code{convert_param_to_list}
#' to standardize the value input and then filters the data frame to rows where the column matches any of the values.
#'
#' @param data A data frame to filter.
#' @param col A string specifying the column name to filter by.
#' @param val The value(s) to filter for. Can be a vector, list, or comma-separated string.
#'
#' @return A filtered data frame containing only rows where \code{col} matches \code{val}.
#' @examples
#' filter_by_col(df, "Course Campus Code", "ABQ")
#' filter_by_col(df, "Course College Code", c("A", "B"))
#' filter_by_col(df, "SUBJ_CRSE", "MATH 1430,ENGL 1110")

filter_by_col <- function(data, col, val) {
  message("filtering by ",col, "=", val)
  
  param_to_list <- convert_param_to_list(val)
  message(param_to_list)
  
  ## use get instead of {{ }} because col is passed in as a string, rather than a variable
  data <- data %>% filter (get(col) %in% param_to_list)
    
  return(data)
}



filter_by_term <- function(data,term,term_col_name) {
  
  # if term is not a list, convert to string
  if (!is.list(term)) {
    term <- as.character(term)
  }
  
  message("term legnth: ", length(term))
  
  if (length(term) > 0 || !is.null(term)) { 
    message("processing term param: ", term)
    #message("term_col_name: ", term_col_name)
    print(str(term))
    # check for single string and dash to indicate range
    if (length(term) == 1 && grepl("-",term)) {
      message("parsing term code range...")
      terms <- unlist(str_split(term,"-"))
      message("terms: ",terms)
      
      # for terms like 202280-
      if (terms[2] == "") {
        term_str <- paste0("`",term_col_name,"` >= ",terms[1])
      }
      else {
        term_str <- paste0(term_col_name," >= ",terms[1], " & ", term_col_name , " <= ",terms[2])
      }
      
      message("term_str: ",term_str)
      
      data <- data %>% filter (!!rlang::parse_expr(term_str))
    } # end if not list
    
    else if (length(term) == 1 && term == "fall") {
      data <- data %>% filter (substring(get({{term_col_name}}),5,6) == 80)
    } 
    else if (length(term) == 1 && term == "spring") {
      data <- data %>% filter (substring(get({{term_col_name}}),5,6) == 10)
    }
    else if (length(term) == 1 && term == "summer") {
      data <- data %>% filter (substring(get({{term_col_name}}),5,6) == 60)
    }
    else {  # convert param to list and filter
      term_list <- convert_param_to_list(term)
      #message("filtering ", term_col_name, " by ",term_list)
      data <- data %>% filter (get(term_col_name) %in% term_list)
    }
  } # end if term is not null
  
  message("term filtering done. returning ",nrow(data)," rows.")
  return (data)
}



# this function filters a simple SUBJ_CRSE list according to opt params
# select_courses should be a 1xn tibble or list
filter_course_list <- function(all_courses,select_courses,opt) {
  message("welcome to filter_course list!")
  
  # for studio testing...
  #all_courses <- load_courses()
  #select_courses <- as_tibble(next_courses$SUBJ_CRSE)
  
  # filter all courses to just supplied selected 
  courses <- all_courses %>% filter (SUBJ_CRSE %in% unlist(select_courses))
  
  # get all enrollment data for course to
  enrls <- get_enrl(courses,opt)
  
  # grab just course list
  course_list <- unique(enrls$SUBJ_CRSE)
  
  message("all done in filter_course_list.")
  return(course_list)
}


# filter out summer from DF
filter_out_summer <- function (data,term_col_name) {
  data <- data %>% filter (substring(get({{term_col_name}}),5,6) != 60)
  return(data)
}


#' Generic filter for a MyReports data frame.
#'
#' @param df The data frame to filter (DESRs or class list).
#' @param opt The options list.
#' @param opt_col_map Named list mapping opt param names to column names in df.
#' @param special_filters (Optional) Named list of functions for special-case filtering.
#' @return Filtered data frame.
filter_data <- function(df, opt, opt_col_map, special_filters = list()) {
  for (opt_name in names(opt_col_map)) {
    message("Checking filter option: ", opt_name)
    col_name <- opt_col_map[[opt_name]]
    message("Column name: ", col_name)
    if (!is.null(opt[[opt_name]])) {
      message("Filtering by ", opt_name, " with value: ", opt[[opt_name]])
      # Use special filter if defined, otherwise default to filter_by_col
      if (opt_name %in% names(special_filters)) {
        message("Using special filter for ", opt_name)
        # Check if the special filter is a function
        if (!is.function(special_filters[[opt_name]])) {
          stop(paste0("Special filter for ", opt_name, " is not a function."))
        }
        # Call the special filter function with df and the option value
        # Check if the special filter function takes two arguments    
        if (length(formals(special_filters[[opt_name]])) == 2) {
          df <- special_filters[[opt_name]](df, opt[[opt_name]])
        } else {
          stop(paste0("Special filter for ", opt_name, " has an unexpected number of arguments."))
        }
        # Run the special filter function
        df <- special_filters[[opt_name]](df, opt[[opt_name]])
      } else {
        # Default to filter_by_col
        df <- filter_by_col(df, col_name, opt[[opt_name]])
      }
    }
  }
  # Display the number of rows after filtering
  message("Filtered data has ", nrow(df), " rows.")

  return(df)
}

# DESRs filter options map
opt_col_map_desr <- list(
  course_campus = "CAMP",
  course_college = "COLLEGE",
  course_status = "STATUS",
  dept          = "DEPT",
  subj          = "SUBJ",
  crn           = "CRN",
  course        = "SUBJ_CRSE",
  term          = "TERM",
  pt            = "PT",
  inst          = "PRIM_INST_LAST",
  gen_ed        = "gen_ed_area",
  level         = "level",
  im            = "INST_METHOD",
  job_cat       = "job_cat",
  uel          = ""
)

# Example usage for class lists
opt_col_map_classlist <- list(
  course_campus      = "Course Campus Code",
  course_college     = "Course College Code",
  student_campus     = "Student Campus Code",
  student_college    = "Student College Code",
  classification    = "Student Classification",
  course            = "SUBJ_CRSE",
  term              = "Academic Period Code",
  level             = "level",
  inst              = "Primary Instructor Last Name",
  pt                = "Sub-Academic Period Code",
  major             = "Major",
  crn               = "CRN",
  dept              = "DEPT",
  subj              = "Subject Code",
  gen_ed            = "Gen Ed Area",
  reg_status_code   = "Registration Status Code",
  im                = "Instruction Delivery Mode Code",
  uel               = ""
)


special_filters_desr <- list(
  term = function(df, value) filter_by_term(df, value, "TERM"),
  crosslist = function(df, value) .xlist_filter(df, value),
  enrl_min = function(df, value) df %>% filter(total_enrl >= as.integer(value)),
  enrl_max = function(df, value) df %>% filter(total_enrl <= as.integer(value)),
  uel = function(df, value) df %>% subset(!(SUBJ_CRSE %in% excluded_courses)),
)

special_filters_classlist <- list(
  term = function(df, value) filter_by_term(df, value, "Academic Period Code"),
  uel = function(df, value) df %>% subset(!(SUBJ_CRSE %in% excluded_courses))  
)

#' Filter DESRs based on provided options
filter_DESRs <- function(courses, opt) {
  message("Filtering DESRs with supplied options...")
  message("Starting with ", nrow(courses), " rows.")

  # Check for at least one filter option
  if (!length(opt)){
    print_help(opt_parser)
    stop("Error in filter_DESRs: Please supply at least one filter parameter", call.=FALSE)
  }
  
  # Use the generic filter_data function with the DESR mapping and special filters
  courses <- filter_data(courses, opt, opt_col_map_desr, special_filters_desr)
  
  # Set default groupings for output
  courses <- courses %>% group_by(TERM, SUBJ_CRSE, CRSE_TITLE, PT, INST_METHOD, level, INST_NAME)
  
  # Dedupe based on CRN
  courses <- courses %>% distinct(CRN, .keep_all = TRUE)
    
  message("done filtering DESRs. returning ", nrow(courses), " courses...\n")
  
  return(courses)
}

# Filter class lists based on provided options 
filter_class_list <- function(students, opt) {
  message("Filtering class lists with supplied options...")
  message("Starting with ", nrow(students), " rows.")

  # Check for at least one filter option
  if (!length(opt)){
    print_help(opt_parser)
    stop("Error in filter_class_list: Please supply at least one filter parameter", call.=FALSE)
  }
  
  # Use the generic filter_data function with the DESR mapping and special filters
  students <- filter_data(students, opt, opt_col_map_classlist, special_filters_classlist)
    
  message("done filtering class lists. returning ", nrow(students), " students...\n")
  
  return(students)
}
  

# CROSSLIST FILTER
.xlist_filter <- function(df,action) {
  message("Welcome to xlist.R!")
  
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
  else if (action == "compress") {
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

  else if (action == "exclude") {
    print("excluding cross-listed courses (by XL_CRN = 0)...")
    df <- df %>% filter(XL_CRN == "0")
    return(df)
  }
}