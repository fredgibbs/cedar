# this file provides miscellaneous functions used across CEDAR

convert_param_to_list <- function(param) {
  message("converting param to list...")
  # message("converting param (",param,") to list...")
  
  # check if actual list already; if so, return it
  if (is.list(param)) {
    message("param is already list.")
    #print(param)
    param_to_list <- param
    return(param_to_list)
  } else {
    # make into string
    param <- as.character(param)
  }
    
  # check if named list (probably defined in includes/lists.R)  
  if (exists(get("param"))) { 
    message("found named list (", get("param"), ") object.")
    
    if (is.list(get(param))) {
      message("list type confirmed.")
      #print(get(param))
      param_to_list <- as.list(get(param))
    } else {
      message("named object found, but not it's not a list. attempting to convert param to string and then to list... ")
      param_to_list <- as.list( as.character(param))
    }
  
    # check for comma in list  
  } else if (grepl(",", param)) {
    message("comma string detected...")
    param <- str_replace(param, ", ", ",")
    param_to_list <- as.list(strsplit(param, ",")[[1]])
    
    # default to simple string
  } else {
    message("simple string object detected...")
    param_to_list <- as.list(param)
  }
  message("convert_param_to_list returning: ",param_to_list)
  return(param_to_list)
}



filter_by_term <- function(data,term,term_col_name) {
  if (!is.list(term)) {
    term <- as.character(term)
  }
  
  if (length(term) > 0 || !is.null(term)) { 
    message("processing term param: ", term)
    #message("term_col_name: ", term_col_name)
    
    # check for dash to indicate range
    if (grepl("-",term)) {
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
    } # end if "-"
    else if (term == "fall") {
      data <- data %>% filter (substring(get({{term_col_name}}),5,6) == 80)
    } 
    else if (term == "spring") {
      data <- data %>% filter (substring(get({{term_col_name}}),5,6) == 10)
    }
    else if (term == "summer") {
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


#TODO: extend this course_list to grab a CSV file (created by CEDAR or externally)

# get a course list based on opt params 
# increasingly useful as more funcs become vectorized and can take a course list
get_course_list <- function(courses,opt) {
  
  # for testing
  # opt <- list()
  # opt$aggregate <- "course"
  # opt$term <- "202160"
  # opt$level <- "lower"
  # opt$uel <- TRUE

  # TODO: set standard opts if null
    
  enrls <- get_enrl(courses,opt)
  course_list <- unique(enrls$SUBJ_CRSE)
  
  return(course_list)
}


# this function filters a simple SUBJ_CRSE list according to opt params
# select_courses should be a 1xn tibble or list
filter_course_list <- function(all_courses,select_courses,opt) {
  message("welcome to filter_course list")
  #print(head(all_courses))
  #print(select_courses)
  #print(as.list(select_courses))
  # studio testing...
  #all_courses <- load_courses(opt)
  #select_courses <- as_tibble(next_courses$SUBJ_CRSE)
  
  # filter all courses to just supplied selected 
  courses <- all_courses %>% filter (SUBJ_CRSE %in% unlist(select_courses))
  
  # get all enrollment data for course to
  enrls <- get_enrl(courses,opt)
  
  # grab just course list
  course_list <- unique(enrls$SUBJ_CRSE)
  
  message("all done in filter_course_list!")
  return(course_list)
}


# filter out summer from DF
filter_out_summer <- function (data,term_col_name) {
  data <- data %>% filter (substring(get({{term_col_name}}),5,6) != 60)
  return(data)
}



update_codes <- function(df,col) {
  df[col][df[col] == "CCS"] <- "CCST"
  df[col][df[col] == "PSY"] <- "PSYC"
  df[col][df[col] == "SOC"] <- "SOCI"
  return(df)
}



# use term codes to add academic year columns
add_acad_year <- function(df, term_col) {
  df <- df %>%
    mutate(acad_year = case_when(
      str_detect(as.character(get({{term_col}})), "60") ~ paste0(as.integer(substring(get({{term_col}}),1,4))-1,"-",substring(get({{term_col}}),1,4)),
      str_detect(as.character(get({{term_col}})), "10") ~ paste0(as.integer(substring(get({{term_col}}),1,4))-1,"-",substring(get({{term_col}}),1,4)),
      str_detect(as.character(get({{term_col}})), "80") ~ paste0(as.integer(substring(get({{term_col}}),1,4)),"-",as.integer(substring(get({{term_col}}),1,4))+1)
    ))  
  return(df)
}


load_forecasts <- function(opt) {
  message("welcome to load_forecasts!")
  
  forecast_rda_file <- paste0(cedar_data_dir,"processed/forecasts.Rda")
  message("looking for: ",forecast_rda_file,"...")
  
  if (file.exists(forecast_rda_file)) {
    message("loading forecast data...")
    load(forecast_rda_file) # loads forecast_data
    
    if (!exists("forecast_data")) {
      message("no forecast data available!")
      forecast_data <- tibble()
    }
    else {
      message("processing data...")
    }
  } else { # no forecasts.Rda file; return empty tibble
    forecast_data <- tibble()
  } 
  message("returning forecast data...")
  return(forecast_data)
}


load_courses <- function(opt) {
  # load data
  message("loading course data...")
  load(paste0(cedar_data_dir,"processed/courses_with_final_and_latest_enrollments.Rda"))
  courses <- completed_and_ongoing_courses # rename DF from file
  
  message("processing data...")

  # merge HR data
  courses <- merge_hr_data(courses)
  
  return(courses)
}


load_students <- function(opt) {
message("loading class list data...")
students <- read_feather(paste0(cedar_data_dir,"processed/class_list.feather"))

message("processing data...")

message("done loading student data.")
return(students)
}


load_academic_study <- function() {
  message("loading academic-study.feather...")
  headcount <- read_feather(paste0(cedar_data_dir,"processed/academic-study.feather"))
  
  message("processing data...")
  
  message("done loading academic study data.")
  return(headcount)
}


# add prev term col
add_prev_term_col <- function (df,term_col_name,summer=F) {
  message("adding prev_term col...")
  if (summer) {
    df <- df %>%
      mutate(prev_term = case_when(
        substring(get({{term_col_name}}),5,6) == 80 ~ as.integer(get({{term_col_name}})) - 20,
        substring(get({{term_col_name}}),5,6) == 10 ~ as.integer(get({{term_col_name}})) - 30,
        substring(get({{term_col_name}}),5,6) == 60 ~ as.integer(get({{term_col_name}})) - 50
      ))
  } else {
  df <- df %>%
    mutate(prev_term = case_when(
      substring(get({{term_col_name}}),5,6) == 80 ~ as.integer(get({{term_col_name}})) - 170,
      substring(get({{term_col_name}}),5,6) == 10 ~ as.integer(get({{term_col_name}})) - 30,
      substring(get({{term_col_name}}),5,6) == 60 ~ as.integer(get({{term_col_name}})) - 50
    ))
  }
  return (df)
}


add_next_term_col <- function (df,term_col_name,summer=F) {
  message("adding prev_term col...")
  if (summer) {
    df <- df %>%
      mutate(next_term = case_when(
        substring(get({{term_col_name}}),5,6) == 80 ~ as.integer(get({{term_col_name}})) + 30,
        substring(get({{term_col_name}}),5,6) == 10 ~ as.integer(get({{term_col_name}})) + 50,
        substring(get({{term_col_name}}),5,6) == 60 ~ as.integer(get({{term_col_name}})) + 20
      ))
  } else {
    df <- df %>%
      mutate(next_term = case_when(
        substring(get({{term_col_name}}),5,6) == 80 ~ as.integer(get({{term_col_name}})) + 30,
        substring(get({{term_col_name}}),5,6) == 10 ~ as.integer(get({{term_col_name}})) + 70,
        substring(get({{term_col_name}}),5,6) == 60 ~ as.integer(get({{term_col_name}})) + 20
      ))
  }
  return (df)
}




# find the previous semester code
# default to ignoring summer in calcs
subtract_term <- function (term_code, summer = FALSE) {
  
  # separate year and semester code from given term code
  year <- as.integer(substring(term_code,1,4))
  term <- as.integer(substring(term_code,5,6))
  
  if (term == 80) {
    if (summer) {
      prev_term <- as.integer(paste0(year,"60"))
    }
    else {
      prev_term <- as.integer(paste0(year,"10"))
    }
  } 
  else if (term == 60) {
    prev_term <- as.integer(paste0(year,"10"))
  } 
  else if (term == 10) {
    prev_term <- as.integer(paste0(year-1,"80"))
  }
  
  return(prev_term)
  
} # end subtract_term


# determine next term code; default is to ignore summer terms
add_term <- function (term_code,summer=F) {
  term <- as.integer(substring(term_code,5,6))
  year <- as.integer(substring(term_code,1,4))
  
  if (summer) {
    if (term == 80) {
      term_m1 <- as.integer(paste0(year+1,"10",sep=""))
    } 
    else if (term == 60) {
      term_m1 <- as.integer(paste0(year,"80",sep=""))
    } 
    else if (term == 10) {
      term_m1 <- as.integer(paste0(year,"60",sep=""))
    }
  }
  else {
    if (term == 80) {
      term_m1 <- as.integer(paste0(year+1,"10",sep=""))
    } else {
      term_m1 <- as.integer(paste0(year,"80",sep=""))
    }
  }

  return(term_m1)
}


# add term type col
add_term_type_col <- function (df,term_col_name) {
  message("adding term type col...")
  df <- df %>%
    mutate(term_type = case_when(
      substring(get({{term_col_name}}),5,6) == 80 ~ "fall",
      substring(get({{term_col_name}}),5,6) == 10 ~ "spring",
      substring(get({{term_col_name}}),5,6) == 60 ~ "summer"
    ))  
  return (df)
}


# determine term type from term code
get_term_type <- function (term_code) {
  term_type <- case_when(
    substring(term_code,5,6) == 80 ~ "fall",
    substring(term_code,5,6) == 10 ~ "spring",
    substring(term_code,5,6) == 60 ~ "summer"
  )
  return (term_type)
}



# useful for creating linear models over time
add_term_bins <- function(df,term_col_name) {
  
  # remove summer from termcode list
  #bins <- num.labs[!grepl('60', num.labs)]
  
  bins <- num.labs
  
  # add basic time series for term codes
  df <- df %>% mutate (term_bin = match(get({{term_col_name}}),bins))
  min_bin <- min(df$term_bin)
  
  df <- df %>% mutate (term_bin = term_bin - min_bin + 1)
  
  return(df)
}


# covert term codes to term strings (see mappings.R)
term_code_to_str <- function (term_code) {
  term_str <- term_text[which(num.labs == term_code)]
  ifelse (length(term_str) == 0,term_code,term_str)
}


# see lists.R for num.labs (term codes, like 202410) and term_text (Spring 2024) definitions
# this can be used by merging this DF with any MyReports DF with only names instead of codes
# example: merged <- (merge(term_code_lookup, table_w_term_text, by = 'term_code'))
term_code_lookup <- data.frame(term_code=num.labs,Semester=term_text)


# convert a term code to a date object
# expects something like 202280 or 2023101H or 2020602H
code_to_date <- function(term) {
  year <- substring(term,1,4)
  semester <- substring(term,5,6)
  month <- case_when(
    semester == "80" ~ "09",
    semester == "10" ~ "02",
    semester == "60" ~ "06"
  )
  
  #always use 10th of month; maybe there's a better date for employment or enrollment checking
  date <- make_date(year,month,10)
}


# extract dept from course param
get_dept_from_course <- function (course) {
  dept <- subj_to_dept_map[[substring(course, 1, gregexpr(pattern = " ",course)[[1]] -1 ) ]]
  message("returning dept: ", dept)
  return(dept)
}



compress_aop_pairs <- function (courses,opt) {
  message("compressing AOP courses into single row...")
  
  # for testing...
   # courses <- load_courses(opt)
   # opt <- list()
   # #opt[["course"]] <- "BIOL 2305"
   # opt[["term"]] <- "202210"
   # courses <-  filter_DESRs(courses,opt)
  
  # for clarity, combine aop and twin courses into single entry
  # test to see if we're filtering by dept
  courses <- courses %>%  group_by(TERM, XL_CODE)
  
  # get just AOP courses
  courses_aop <- courses %>% filter (INST_METHOD == "MOPS")
  
  # AOP sections don't necessarily have a partner, so remove those without one
  # TODO: handle case of AOP course having partner, but not being crosslisted
  # might be able to check on course title
  courses_aop <- courses_aop %>% filter (XL_CODE != "0")
  
  # get pairs of aop and twin section
  aop_pairs <- courses_aop %>% filter (XL_CODE %in% courses_aop$XL_CODE) %>% 
    distinct(CRN, .keep_all = TRUE) %>% 
    group_by(TERM,XL_CODE)
  
  # to collapse the aop and online section into one row, get each section's enrollment
  aop_pairs <- aop_pairs %>% mutate (sect_enrl = ENROLLED, pair_enrl = total_enrl - ENROLLED)
  
  # arrange by inst_method, and take first row of group
  aop_single <- aop_pairs %>% arrange(INST_METHOD) %>% filter (row_number() == 1)
  # message("aop sections:")
  # print(aop_single)
  
  # since compressing two sections into one, change ENROLLED to mimic total_enrl
  # otherwise, compressing effectively deletes the non-aop section enrollment
  aop_single <- aop_single %>% mutate (ENROLLED = total_enrl)
  
  # remove all pairs from orig course list
  courses <- courses %>% filter (!(XL_CODE %in% courses_aop$XL_CODE)) %>% distinct(CRN, .keep_all = TRUE) %>% 
    group_by(TERM,XL_CODE)
  
  # add all single rows
  courses <- rbind(courses,aop_single)
  
  message("returning compressed aop rows...")
  
  return(courses)
} # end compress_aop_pairs


merge_hr_data <- function (courses) {
  
  ############ merge personnel data with course data
  message("welcome to merge_hr_data!")
  
  # get faculty data to associate title with person in course listings
  file_name <- paste0(cedar_data_dir,"processed/fac_by_term.Rda")
  message("loading ",file_name,"...")
  load(file_name)
  
  message("adjusting data types...")
  courses$`PRIM_INST_ID` <- as.double(courses$`PRIM_INST_ID`)
  courses$`TERM` <- as.character(courses$`TERM`)
  
  # disregard as_of_date in HR data
  fac_by_term <- fac_by_term %>% select (-c(as_of_date))
  
  # merge faculty and course data
  # merging DEPT fields means an instructor w/o an appt % or other kind of admin appt with that DEPT 
  # will not have a title listed on the enrl reporting tools
  message("merging faculty data with course data...")
  courses <- merge(courses,fac_by_term,by.x=c("TERM","PRIM_INST_ID"),by.y=c("term_code","UNM ID"),all.x=TRUE)
  courses <- courses %>% select (-c(DEPT.y))
  courses <- courses %>% rename (DEPT = DEPT.x)
  
  message("done merging HR data.")
  
  return(courses)
} # end merge_hr_data



# this function is called when looping through a list of depts (even if just one)
create_report <- function(opt, d_params) {
  message("welcome to create_report!")
  
  output <- opt$output
  
  if (!is.null(output)) {
    message("setting output format to: ", output)
    prefix <- ifelse (output=="aspx","aspx/","html/")
    suffix <- ifelse (output=="aspx","-report.aspx",".html")
  } else {
    message("defaulting to HTML output...")
    prefix <- "html/"
    suffix <- ".html"
  }
  
  # TRICKY! the paths for the output file are relative to the Rmd file, not the current working directory.
  if (opt[["onedrive"]]) {
    output_filename <- paste0(cedar_onedrive_dir,"/",d_params$output_filename,".aspx")
  } 
  else {
    output_filename <- paste0(d_params$output_dir_base,prefix,d_params$output_filename,suffix)
  }
  
  message("working dir: ",getwd())
  message("output file set to: ", output_filename)
  message("rendering report for ", d_params$dept_code)
  
  Sys.setenv(RSTUDIO_PANDOC=rstudio_pandoc)
  
  # xfun::Rscript_call(
  #   rmarkdown::render,
  #   list(input = 'dept-reports/dept-report.Rmd', output_format = 'html_document',output_file = output_file,params = d_params)
  # )
  # 
  
  rmarkdown::render(d_params$rmd_file,
                    output_file = output_filename,
                    params = d_params)
  
  message("done rendering.")
} # end create report



# dormant: used originally for 2022-23 dept data reports
# create linear model and use slope to find end point
repel_row <- function(sum_table,name) {
  #check if sum_table has any rows
  if(!nrow(sum_table) == 0){
    sum_table$term_bin<-1:nrow(sum_table)
    fm <- as.formula (paste(names(sum_table)[2],"~","term_bin"))
    model <- lm( fm, data = sum_table)
    ends <- predict(model, newdata=data.frame(term_bin=c(1,nrow(sum_table))))
    start <- ends[[1]]
    end <- ends[[2]]
    if (start < 0) start = 0
    slope <- coef(model)[2]
    result <- c(level=name, y=end, x="202280", slope=slope, per_change = end/start-1)
  } 
  else {
    result <- c(level=name, y=0, x="202280", slope=0, per_change = 0)
  }
}  