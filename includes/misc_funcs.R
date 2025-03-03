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
  #all_courses <- load_courses()
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


load_datafile <- function(filename) {
  message("loading data for: ",filename,"...")
  
  # check for cloud data
  if (exists("cedar_cloud_data_urls") && !is.null(cedar_cloud_data_urls[[filename]])) {
    message("getting data from the cloud...")
    data <- readRDS(url(cedar_cloud_data_urls[[filename]] ))
  } else {
    message("getting data from local file...")
    data <- readRDS(paste0(cedar_data_dir,"processed/",filename,".Rds"))  
  }
  
  message("returning data...")
  return(data)
}


load_students <- function() {
  data <- load_datafile("class_lists")
  return(data)
}

load_courses <- function() {
  data <- load_datafile("desrs")
  return(data)
}

load_degrees <- function() {
  data <- load_datafile("degrees")
  return(data)
}

load_academic_studies <- function() {
  data <- load_datafile("academic_studies")
  return(data)
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






# this function is called when looping through a list of depts (even if just one)
create_report <- function(opt, d_params) {
  message("welcome to create_report! (in misc_funcs.R")
  
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
  if (!is.null(opt[["onedrive"]]) && opt[["onedrive"]]) {
    output_filename <- paste0(cedar_onedrive_dir,"/",d_params$output_filename,".aspx")
  } 
  else {
    output_filename <- paste0(d_params$output_dir_base,prefix,d_params$output_filename,suffix)
  }
  
  message("working dir: ",getwd())
  message("output file set to: ", output_filename)
  message("rendering report for ", d_params$dept_code)
  
  Sys.setenv(RSTUDIO_PANDOC=rstudio_pandoc)
  
  # need to run as Rscript call from shiny app (or within Rstudio)
  if (!is.null(opt[["shiny"]]) && opt[["shiny"]] == TRUE) {
    rmd_output <- xfun::Rscript_call(
      rmarkdown::render,
      list(input = d_params$rmd_file, output_format = 'html_document', output_file = output_filename, params = d_params)
    )
  } else {
    rmd_output <- rmarkdown::render(d_params$rmd_file,
                      output_file = output_filename,
                      params = d_params)
  }

  message("done rendering.")
  return (rmd_output)
} # end create report

