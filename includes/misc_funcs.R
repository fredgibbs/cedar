# this file provides miscellaneous functions used across CEDAR

# create controller function for running in RStudio
# emulate CLI functionality from within RS
# all code and env variables should have been loaded by .Rprofile
cedar <- function(func="guide",...) {
  opt <- list(...)
  opt$func <- func
  
  if (is.null(opt$guide)) {
    opt$guide <- FALSE
  }
  
  # display command line params
  print(opt)  
  
  message("loading external functions...")
  source("includes/config.R")
  source("includes/load_funcs.R")
  load_funcs("./")
  
  resolve_conflicts() # defined in misc_funcs
  
  message("processing function: ", opt$func, "...")
  msg <- process_func(opt)
  if (is.character(msg)) { 
    message(msg)
  }
  
  message("cedar function done!")
}


resolve_conflicts <- function() {
  conflicted::conflicts_prefer(dplyr::filter())
  conflicted::conflicts_prefer(dplyr::lag())
  conflicted::conflicts_prefer(plotly::layout)
}



# TODO: way too hacky. 
update_codes <- function(df,col) {
  df[col][df[col] == "CCS"] <- "CCST"
  df[col][df[col] == "PSY"] <- "PSYC"
  df[col][df[col] == "SOC"] <- "SOCI"
  return(df)
}


# get a course list based on opt params 
# increasingly useful as more funcs become vectorized and can accept course list as input
get_course_list <- function(courses,opt) {
  
  # for testing
  # opt <- list()
  # opt$group_by <- "course"
  # opt$term <- "202160"
  # opt$level <- "lower"
  # opt$uel <- TRUE

  # TODO: set standard opts if null
  # TODO: extend to grab a CSV file (created by CEDAR or externally)
  
  enrls <- get_enrl(courses,opt)
  course_list <- unique(enrls$SUBJ_CRSE)
  
  return(course_list)
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


# load_forecasts <- function(opt) {
#   message("welcome to load_forecasts!")
#   
#   forecast_rda_file <- paste0(cedar_data_dir,"processed/forecasts.Rda")
#   message("looking for: ",forecast_rda_file,"...")
#   
#   if (file.exists(forecast_rda_file)) {
#     message("loading forecast data...")
#     load(forecast_rda_file) # loads forecast_data
#     
#     if (!exists("forecast_data")) {
#       message("no forecast data available!")
#       forecast_data <- tibble()
#     }
#     else {
#       message("processing data...")
#     }
#   } else { # no forecasts.Rda file; return empty tibble
#     forecast_data <- tibble()
#   } 
#   message("returning forecast data...")
#   return(forecast_data)
# }


load_global_data <- function(opt) {
  message("loading global data...")
  
  .GlobalEnv$courses <- load_courses()
  
  # special exception to speed up common use
  if (is.null(opt) || opt[["func"]] != "enrl") {
    .GlobalEnv$students <- load_students()
    .GlobalEnv$academic_studies <- load_academic_studies()
  }
  
  
  # don't make forecasts global b/c it changes too often 
}


load_datafile <- function(filename) {
  message("loading data for: ", filename,"...")
  
  # temp hack until external data persistence 
  # when shiny starts, forecasts is loaded from online location but saved immediately locally for session updates
  if (filename == "forecasts" && as.logical(Sys.getenv("shiny"))) {
    message("trying to load forecasts in Shiny...")
    data <- readRDS("forecasts.Rds")
    return(data)
  }
  
  # check for cloud data
  if (exists("cedar_cloud_data_urls") && !is.null(cedar_cloud_data_urls[[filename]])) {
    message("getting data from the cloud...")
    data <- readRDS(url(cedar_cloud_data_urls[[filename]] ))
  } 
  else {
    message("getting data from local file...")
    localfile <- paste0(cedar_data_dir,"processed/",filename,".Rds")
    
    if (file.exists(localfile)) {
      data <- readRDS(paste0(cedar_data_dir,"processed/",filename,".Rds"))  
    }
    else {
      message("no data file found.")
      data <- tibble()
    }
  }
  
  message("returning data...")
  return(data)
}



# TODO: replace these specific loads with load_datafile across app
load_hr_data <- function() {
  data <- load_datafile("hr_data")
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

load_forecasts <- function() {
  data <- load_datafile("forecasts")
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


# add_next_term_col <- function (df,term_col_name,summer=F) {
#   message("adding next_term col...")
#   if (summer) {
#     df <- df %>%
#       mutate(next_term = case_when(
#         substring(get({{term_col_name}}),5,6) == 80 ~ as.integer(get({{term_col_name}})) + 30,
#         substring(get({{term_col_name}}),5,6) == 10 ~ as.integer(get({{term_col_name}})) + 50,
#         substring(get({{term_col_name}}),5,6) == 60 ~ as.integer(get({{term_col_name}})) + 20
#       ))
#   } else {
#     df <- df %>%
#       mutate(next_term = case_when(
#         substring(get({{term_col_name}}),5,6) == 80 ~ as.integer(get({{term_col_name}})) + 30,
#         substring(get({{term_col_name}}),5,6) == 10 ~ as.integer(get({{term_col_name}})) + 70,
#         substring(get({{term_col_name}}),5,6) == 60 ~ as.integer(get({{term_col_name}})) + 20
#       ))
#   }
#   return (df)
# }

add_next_term_col <- function(df, term_col_name, summer = FALSE) {
  message("adding next_term col...")
  term_col <- rlang::ensym(term_col_name)
  df <- df %>%
    mutate(
      term_str = as.character(!!term_col),
      term_part = substr(term_str, 5, 6),
      next_term = case_when(
        summer & term_part == "80" ~ as.integer(term_str) + 30,
        summer & term_part == "10" ~ as.integer(term_str) + 50,
        summer & term_part == "60" ~ as.integer(term_str) + 20,
        !summer & term_part == "80" ~ as.integer(term_str) + 30,
        !summer & term_part == "10" ~ as.integer(term_str) + 70,
        !summer & term_part == "60" ~ as.integer(term_str) + 20,
        TRUE ~ NA_integer_
      )
    ) %>%
    select(-term_str, -term_part)
  df <- df %>%
    mutate(next_term = as.character(next_term)) # convert
  return(df)
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



# output_data is going to be a df/tibble or list
process_output <- function(output_data,filename,opt) {
  message("\nWelcome to process_output!")
  
  output_list <- list()
  
  if (is_tibble(output_data)) {
    message("incoming output_data is tibble.")
    output_list[[filename]] <- output_data
  } 
  else if (is.list(output_data )) {
    message("incoming output_data is list.")
    output_list <- output_data
  }
  
  # process each element of list
  for (i in 1: length(output_list)) {
    cur_name <- names(output_list)[i]
    message("current output list name: ", cur_name)
    
    cur_item <- as_tibble(output_list[[i]])
    
    # if arrange param set, use it
    if (!is.null(opt[["arrange"]])) {
      arrange_col <- opt[["arrange"]]
      cur_item <- cur_item %>%  arrange(get({{arrange_col}}) )
    }
    
    # if output csv flag set, print 5 rows as sample and save file 
    if (!is.null(opt[["output"]]) && opt[["output"]] == "csv") {
      
      message("output for ",cur_name,":")
      cur_item %>% tibble::as_tibble() %>% print(n = 5, width=Inf)
      
      filename <- paste0(cedar_output_dir,"csv/",cur_name,".csv")  
      message("saving CSV file with name: ", filename, "...")
      write.csv(cur_item, file = filename)
      message("file saved.")
    }
    else { # if not saving CSV, print all rows to terminal
      cur_item %>% tibble::as_tibble() %>% print(n = nrow(cur_item), width=Inf)  
    }
  }
  message("all done in process_output!\n")
}




# generic Rmd report creator
create_report <- function(opt, d_params) {
  message("\nWelcome to create_report! (in misc_funcs.R)")
  
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
  
  # CAREFUL! the paths for the output file are relative to the Rmd file, not the current working directory
  if (!is.null(opt[["onedrive"]]) && opt[["onedrive"]]) {
    output_filename <- paste0(cedar_onedrive_dir,"/",d_params$output_filename,".aspx")
  } 
  else {
    output_filename <- paste0(d_params$output_dir_base,prefix,d_params$output_filename,suffix)
  }
  
  message("working dir: ",getwd())
  message("output file set to: ", output_filename)
  message("rendering report for dept_code: ", d_params$dept_code)
  
  
  # need to run as Rscript call from shiny app (or within Rstudio)
  if (!is.null(opt[["shiny"]]) && opt[["shiny"]] == TRUE) {
    output_filename <- "output.html"
    rmd_output <- xfun::Rscript_call(
      rmarkdown::render,
      list(input = d_params$rmd_file, output_format = 'html_document', output_file = output_filename, params = d_params)
    )
  } else {
    Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc) # from config.R
    
    rmd_output <- rmarkdown::render(d_params$rmd_file,
                      output_file = output_filename,
                      params = d_params)
  }

  message("done rendering.")
  return (rmd_output)
} # end create report

