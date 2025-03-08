#!/usr/bin/env Rscript
# This script expects to be run from the cedar_base_dir (defined in includes/config.R)

#install.packages("pacman", repos='http://cran.us.r-project.org')
pacman::p_load(tidyverse,fs,data.table, optparse, plotly)
conflicted::conflicts_prefer(dplyr::filter())
conflicted::conflicts_prefer(dplyr::lag())


set_option_list <- function() {
  message("setting up option list...")
  
  # set up option parser
  option_list = list(
    make_option(c("-f","--func"), default="guide",
                help="specifies what function to execute. specify `guide` to see available options."), 
    
    make_option(c("--guide"), default=FALSE, action="store_true",
                help="show instructions and options for specified function."),
    
    # basic filtering params
    make_option(c("--campus"), type="character",
                help="campus", metavar="character"),
    
    make_option(c("--classification"), type="character",
                help="student classification [default= %default]", metavar="character"),
    
    make_option(c("--college"), type="character", # for filtering DESRs
                help="college offering course"), 
    
    make_option(c("--studentcollege"), type="character", # for filtering class lists
                help="college (code) student is enrolled in"), 
    
    make_option(c("--coursecollege"), type="character", # for filtering class lists
                help="college (code) offering course"), 
    
    make_option(c("-c", "--course"), type="character",
                help="course SUBJ and NUMBER (=SUBJ and CRSE in DESRs); this can be like HIST 491 or a comma-separated list, or a named list."),
    
    make_option(c("--crn"), type="character",
                help="course CRN"),
    
    make_option(c("-d", "--dept"), type="character", 
                help="filter by 4-letter department code", metavar="character"),
    
    make_option(c("--enrl_min"), type="character", 
                help="course enrollment minimum", metavar="character"),
    
    make_option(c("--enrl_max"), type="character", 
                help="course enrollment maximum", metavar="character"),
    
    make_option(c("--gen_ed"), type="character",
                help="gen ed area number"),

    make_option(c("--group_cols"), type="character",
                help="column names to aggregate results by"),
    
    make_option(c("-i","--inst"), type="character", 
                help="instructor", metavar="character"),
    
    make_option(c("--im"), type="character",
                help="instruction method ", metavar="character"),
    
    make_option(c("--job_cat"), type="character", 
                help="job category", metavar="character"),
    
    make_option(c("-l", "--level"), type="character", 
                help="course level: undergrad | grad | lower | upper", metavar="character"),
    
    make_option(c("-m", "--major"), type="character", 
                help="student major", metavar="character"),
    
    make_option(c("--pt"), type="character",
                help="part of term [1, 1H, 2H] ", metavar="character"),
    
    make_option(c("--registration_status"), type="character", #default="Student Registered",
                help="student registration status", metavar="character"), 
    
    make_option(c("-s", "--subj"), type="character", 
                help="subject code", metavar="character"),
    
    make_option(c("--status"), type="character", default="A", 
                help="status [default = %default]", metavar="character"), 
    
    make_option(c("--summer"), default=FALSE, action="store_true",
                help="include summer terms"),
    
    make_option(c("-t", "--term"), type="character", 
                help="term code (i.e. 202410)", metavar="character"),
    
    make_option(c("--uel"), action="store_true",
                help="Use Exclude List", metavar="character"),
    
    make_option(c("--aop"), type="character",
                help="compress makes single DESR row from the AOP and ONL rows for same course", metavar="character"),
    
    make_option(c("-x", "--crosslist"), type="character",
                help="exclude | compress :: exclude removes XLed courses; compress flattens all XLed sections into one, and uses subject code of the largest section", metavar="character"),
    
    
    # non-filtering options used with various flags
    make_option(c("-a", "--aggregate"), type="character",
                help="aggreagte: specify how; see guide for function for options.", metavar="character"),
    
    make_option(c("--arrange"), type="character",
                help="arrange: specify a column name to arrange by.", metavar="character"),
    
    make_option(c("--output"), type="character",
                help="csv, html, aspx"), 
    
    make_option(c("--nso"), default=FALSE, action="store_true",
                help="use nso data for forecasting"), 
    
    make_option(c("--forecast_method"), 
                help="forecasting method to use: conduit, major, all", metavar="character"),
    
    make_option(c("--forecast_conduit_term"), 
                help="term to use as basis for projections if not deafult conduit of term before target term", metavar="character"),
    
    make_option(c("--onedrive"),  default=FALSE, action="store_true",
                help="us to automatically save file to ondrive directory as specified in config.R", metavar="character")
  ); 
  
  
  return (option_list)
}


load_data <- function() {
  message("loading data...")
  .GlobalEnv$courses <- load_courses()
  .GlobalEnv$students <- load_students()
}


cedar_init <- function() {
  message("loading external functions...")
  source("includes/config.R")
  source("includes/load_funcs.R")
  load_funcs("./")
  load_data()
}


# run below when cedar.R is called from command line
message("Welcome to CEDAR! starting timer...")
start.time <- Sys.time()

options("width"=300)

cedar_init()

option_list <- set_option_list()
opt_parser <- OptionParser(option_list = option_list);
opt <- parse_args(opt_parser);
print(opt)
process_func(opt)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
message("all done. completed in ",time.taken," seconds.","\n")
