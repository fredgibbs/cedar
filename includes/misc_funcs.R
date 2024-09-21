# this file provides miscellaneous functions used across CEDAR

convert_param_to_list <- function(param) {
  message("converting param (",param,") to list...")
  
  # check if actual list already; if so, return it
  if (is.list(param)) {
    message("using list object...")
    param_to_list <- param
    return(param_to_list)
  } else {
    # make into string
    param <- as.character(param)
  }
    
  # check if named list (probably defined in includes/lists.R)  
  if (exists(get("param"))) { 
    message("found ", get("param"), " object.")
    message("using named list object...")
    
    if (is.list(get(param))) {
      message("list type confirmed.")
      print(get(param))
      param_to_list <- as.list(get(param))
    } else {
      stop("object found, but not it's not a list.")
    }
  
    # check for commma list  
  } else if (grepl(",", param)) {
    message("comma string detected...")
    param <- str_replace(param, ", ", ",")
    param_to_list <- as.list(strsplit(param, ",")[[1]])
    
    # default to simple string
  } else {
    message("simple string object detected...")
    param_to_list <- as.list(param)
  }
  message("convert_param_to_list returning ",param_to_list)
  return(param_to_list)
}



filter_by_term <- function(data,term,term_col_name) {
  message("filtering by term!")
  
  term <- as.character(term)
  
  if (length(term) > 0 || !is.null(term)) { 
    message("processing term param: ", term)
    message("term_col_name: ", term_col_name)
    
    # check for dash to indicate range
    if (grepl("-",term)) {
      message("parsing term code range...")
      terms <- unlist(str_split(term,"-"))
      message("terms: ",terms)
      
      term_str <- paste0(term_col_name," >= ",terms[1], " & ", term_col_name , " <= ",terms[2])
      message("term_str: ",term_str)
      
      data <- data %>% filter (!!rlang::parse_expr(term_str))
    }
    else if (opt$term == "fall") {
      data <- data %>% filter (substring(get({{term_col_name}}),5,6) == 80)
    } 
    else if (opt$term == "spring") {
      data <- data %>% filter (substring(get({{term_col_name}}),5,6) == 10)
    }
    else if (opt$term == "summer") {
      data <- data %>% filter (substring(get({{term_col_name}}),5,6) == 60)
    }
    else {  # convert param to list and filter
      term_list <- convert_param_to_list(term)
      message("filtering ", term_col_name, " by ",term_list)
      data <- data %>% filter (get(term_col_name) %in% term_list)
    }
  } # end if term is not null
  
  return (data)
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
  # load data
  message("loading forecast data...")
  forecast_rda_file <- paste0(cedar_data_dir,"processed/forecasts.Rda")
  
  load(forecast_rda_file) # loads forecast_data

  if (!exists("forecast_data")) {
    message("no forecast data available!")  
  }
  else {
  message("processing data...")
  }
  
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



subtract_term <- function (term_code, summer=F) {
  term <- as.integer(substring(term_code,5,6))
  year <- as.integer(substring(term_code,1,4))
  
  if (summer) {
    if (term == 80) {
      term_m1 <- as.integer(paste0(year,"60",sep=""))
    } 
    else if (term == 60) {
      term_m1 <- as.integer(paste0(year,"10",sep=""))
    } 
    else if (term == 10) {
      term_m1 <- as.integer(paste0(year-1,"80",sep=""))
    }
  }
  else if (!summer) {
    if (term == 80) {
      term_m1 <- as.integer(paste0(year,"10",sep=""))
    } else {
      term_m1 <- as.integer(paste0(year-1,"80",sep=""))
    }
  }

  return(term_m1)
}


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



# determine term type from term code
get_term_type <- function (term_code) {
  term_type <- case_when(
    substring(term_code,5,6) == 80 ~ "Fall",
    substring(term_code,5,6) == 10 ~ "Spring",
    substring(term_code,5,6) == 60 ~ "Summer"
  )
  return (term_type)
}


# covert term codes to term strings (see mappings.R)
term_code_to_str <- function (term_code) {
  term_str <- term_text[which(num.labs == term_code)]
  ifelse (length(term_str) == 0,term_code,term_str)
}


# create dataframe for merging data. way faster than the term_code_to_str lookup function
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



# TODO: phase out in favor of dataframe approach
# define function to help parse term codes
# for instance, converts Spring 2023 to 202310
term_to_code <- function(term) {
  
  parts <- unlist(str_split(term," "))
  year <- parts[[2]]
  semester <- parts[1]
  
  sem_code <- case_when(
    semester == "Fall" ~ "80",
    semester == "Spring" ~ "10",
    semester == "Summer" ~ "60"
  )
  
  term_code <- unlist(paste(year,unlist(sem_code),sep=""))
  
  return(term_code)
}



compress_aop_pairs <- function (courses,opt) {
  message("compressing AOP courses into single row...")
  
  # for clarity, combine aop and twin courses into single entry
  # test to see if we're filtering by dept
  courses <- courses %>%  group_by(TERM, XL_CODE)
  
  # get just AOP courses
  courses_aop <- courses %>% filter (INST_METHOD == "MOPS")
  
  # AOP sections don't necessarily have a partner, so remove those without one
  courses_aop <- courses_aop %>% filter (XL_CODE != "0")
  
  # get pairs of aop and twin section
  aop_pairs <- courses_aop %>% filter (XL_CODE %in% courses_aop$XL_CODE) %>% distinct(CRN, .keep_all = TRUE) %>% 
    group_by(TERM,XL_CODE)
  
  # to collapse the aop and online section into one row, get each section's enrollment
  aop_pairs <- aop_pairs %>% mutate (sect_enrl = ENROLLED, pair_enrl = total_enrl - ENROLLED)
  
  # arrange by inst_method, and take first row of group
  aop_single <- aop_pairs %>% arrange(INST_METHOD) %>% filter (row_number() == 1)
  message("aop sections:")
  print(aop_single)
  
  if (opt$output) {
    message("saving aop_summary.csv...")
    filename <- paste0(cedar_output_dir,"enrl/aop_summary.csv")
    write.csv(aop_single,file=filename)
  }
  
  # since compressing two sections into one, change ENROLLED to mimic total_enrl
  # otherwise, compressing effectively deletes the non-aop section enrollment
  aop_single <- aop_single %>% mutate (ENROLLED = total_enrl)
  
  # remove all pairs from orig course list
  courses <- courses %>% filter (!(XL_CODE %in% courses_aop$XL_CODE)) %>% distinct(CRN, .keep_all = TRUE) %>% 
    group_by(TERM,XL_CODE)
  
  # add all single rows
  courses <- rbind(courses,aop_single)
  
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
  
  if (!is.null(opt$output_format)) {
    message("setting output format to ",opt$output_format)
    prefix <- ifelse (opt$output_format=="aspx","aspx/","html/")
    suffix <- ifelse (opt$output_format=="aspx","-report.aspx","-report.html")
  } else {
    message("defaulting to HTML output...")
    prefix <- "html/"
    suffix <- "-report.html"
  }
  
  # TRICKY! the paths for the output file are relative to the Rmd file, not the current working directory.
  output_filename <- paste0(d_params$output_dir_base,prefix,d_params$output_filename,suffix)
  
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