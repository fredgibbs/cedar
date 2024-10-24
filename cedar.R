#!/usr/bin/env Rscript
# This script expects to be run from the cedar_base_dir (defined in includes/config.R)

#install.packages("pacman", repos='http://cran.us.r-project.org')
pacman::p_load(tidyverse, readxl,rvest,fs,data.table, optparse, feather, plotly)

load_funcs <- function() {
  source("includes/config.R")
  source("includes/mappings.R")
  source("includes/lists.R")
  source("includes/excluded_courses.R")
  source("includes/gen_ed_courses.R")
  
  source("includes/misc_funcs.R")
  
  source("includes/filter_class_list.R")
  source("includes/filter_DESRs.R")
  
  source("cones/enrl/enrl.R")
  source("cones/headcount/headcount.R")
  source("cones/waitlist/waitlist.R")
  source("cones/lookout/lookout.R")
}

options("width"=300)
option_list = list(
  make_option(c("-f","--func"), default="guide",
              help="specifies what function to execute. specify `guide` to see available options."), 
  
  make_option(c("--guide"), default=FALSE, action="store_true",
              help="show instructions and options for specified function."),
  
  
  # basic filtering params
  make_option(c("-t", "--term"), type="character", 
              help="term code (i.e. 202410)", metavar="character"),
  
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
  
  make_option(c("-i","--inst"), type="character", 
              help="instructor", metavar="character"),

  make_option("--not_inst_list", action = "store",
              help="name of a list of instructors", metavar="character"),
  
  make_option(c("--notinst"), type="character", 
              help="NOT instructor", metavar="character"),
  
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
  
  make_option(c("--summer"), default=TRUE, action="store_true",
              help="include summer terms"),
  
  make_option(c("--title"), type="character", 
              help="academic title", metavar="character"),
  
  make_option(c("--uel"), action="store_true",
              help="Use Exclude List", metavar="character"),
  
  make_option(c("--aop"), type="character",
              help="compress makes single DESR row from the AOP and ONL rows for same course", metavar="character"),
  
  make_option(c("-x", "--crosslist"), type="character",
              help="exclude | compress :: exclude removes XLed courses; compress flattens all XLed sections into one, and uses subject code of the largest section", metavar="character"),
  
  
  #TODO: standardize agg_by inputs and behavior
  # non-filtering options used with various flags
  make_option(c("-a", "--aggregate"), type="character",
              help="aggreagte: specify how; see guide for function for options.", metavar="character"),
  
  make_option(c("--output"), default=FALSE, action="store_true",
              help="create CSV files of output (very spotty implementation)"), 
  
  make_option(c("--nso"), default=FALSE, action="store_true",
              help="use nso data for forecasting"), 
  
  make_option(c("--forecast_method"), 
              help="forecasting method to use: fall-to-spring, conduit, major, report", metavar="character"),
  
  make_option(c("--output_format"), type="character", default="html", 
              help="for use with dept-reports. use html for general use or aspx if loading to onedrive", metavar="character")
  
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);


if (is.null(opt$func)){
  stop("No function (-f or --func) specified. Specify '-f guide' to see options. ", call.=FALSE)
}

message("starting timer...")
start.time <- Sys.time()

message("loading external functions...")
load_funcs()


########### PROCESS SPECIFIED FUNCTION  REQUEST  ##############
if (opt$func == "guide") {
  message("
          Available functions: course-report, credit-hours, data-status, dept-report, enrl, enrl-cl, forecast, gradebook, headcount, lookout, rollcall, seatfinder-report, sfr, waitlist.
          Specify -f FUNC (where FUNC is one of the terms above) --guide for instructions on each function, like '-f enrl --guide'")  
  stop("no error")
} else {
  message("looking up function: ", opt$func)  
}



############### CREDIT HOURS ############### 
if (opt$func == "credit-hours") {
  message("CREDIT HOURS!")
  if (opt$guide == TRUE) {
    message ("
            credit-hours uses specified filter params to create a simple report of earned credit hours.
            
            Usually, filter for a single dept (-d), but can also filter by college (with --coursecollege AS).
            
            It generates 3 files (in CEDAR_OUTPUT_DIR/credit-hours): 
             - CSV file of credit hours by college, dept, level, and term 
             - CSV file of credit hours by dept, level, and academic year
             - PNG plot (which may be more or less useful depending how specific the filter params are.)
            
            Use -h to see filtering params. ")
    stop("no error")
  }
  
  source("cones/credit-hours/credit-hours.R")
  
  students <- load_students(opt)  
  filtered_students <- filter_class_list(students, opt)
  credit_hours_data <- get_credit_hours(filtered_students)
  
  # print for inspection
  message("credit_hours_data:")
  credit_hours_data %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
  
  # save csv files and plot
  save_credit_hours(credit_hours_data,opt)
}



############### COURSE REPORT ############### 
if (opt$func == "course-report") {
  if (opt$guide == TRUE || ( is.null(opt$course))) {
    message ("
            course-report provides a broad overview of a course over time.
            Required params: 
            -c: a course like 'HIST 413' (WITH single quotes because of the space).
            
            Optional params:
            -t term code to forecast for. Default is term following current term (specified in config.R).
            --output-format: aspx or html. Use html (default) unless needing to publish via OneDrive.
            
            Reports are saved in CEDAR_OUTPUT_DIR/course-reports. 
            ")
    stop("no error")
  }
  
  source("cones/course-report/course-report.R")
  source("cones/forecast/forecast.R")
  source("cones/gradebook/gradebook.R")
  source("cones/credit-hours/credit-hours.R")
  source("cones/nosedive/nosedive.R")
  source("cones/rollcall/rollcall.R")
  
  # if no term specified, use next term
  if (is.null(opt[["term"]])) {
    next_term <- add_term(cedar_current_term)
    message("no opt$term param found. setting opt$term to: ",next_term)
    opt[["term"]] <- next_term
  }  
  
  # extract dept from course param
  dept <- subj_to_dept_map[[substring(opt$course, 1, gregexpr(pattern = " ",opt$course)[[1]] -1 ) ]]
  message("setting opt$dept to: ", dept)
  opt$dept <- dept
  
  courses <- load_courses(opt)
  students <- load_students(opt)
  course_report(students,courses,opt)
}


############### DATA STATUS  ############### 
if (opt$func == "data-status") {
  if (opt$guide == TRUE) {
    message ("
            data-status reports on how recent data has been updated. No required params. ")
    stop("no error")
  }
  source("cones/data-status/data-status.R")
  
  # no return value; everything is terminal output
  get_data_status(opt)
  
  # TODO: make status able to return values for display in various reports
}




############### DEPARTMENT REPORT ############### 
if (opt$func == "dept-report") {
  if (!is.null(opt$guide) && is.null(opt$dept)) {
    message("Specify at least -d DEPT. In addition, you can ALSO use --prog PROG to focus on a particular degree program. Otherwise data reflects aggregate of all degree programs.
            -d can also specify a pre-set list of depts, defined in includes/lists.R.
            Specify --output_format as html (default) or aspx.")
    quit()
  }
  
  source("cones/dept-report/dept-report.R")
  source("cones/credit-hours/credit-hours.R")
  source("cones/degrees/degrees.R")
  source("cones/gradebook/gradebook.R")
  source("cones/sfr/sfr.R")
  
  students <- load_students(opt)
  courses <- load_courses(opt)
  dept_report(students,courses,opt)
}




############### ENROLLMENT FROM DESR ############### 
if (opt$func == "enrl") {
  if (opt$guide == TRUE) {
    message ("
             -a, --aggregate: [course, course_type, dept, dept_level, college_level]
             --aop: compress makes single DESR row from the AOP and ONL rows for same course
             -c, --course: specify SUBJ NUMB in single quotes, like 'MATH 1220'
                OR a comma-separated list like 'MATH 1215,MATH 1220'
                OR a named list as defined in lists.R 
             --campus: use standard abbreviation, like 'ABQ'
             --classification: student classification
             --college: use standard abbreviation, like 'AS'
             --crn: course reference number
             -d, --dept: department code, like HIST
             --gen-ed: gen ed area number
             -i, --inst: instructor LAST name
             --im: instruction method [0, ENH, MOP, HYB, ONL, f2f] 
             -l, --level: [undergrad, grad, lower, upper]
             --job_cat: job category
             --pt: part of term [1, 1H, 2H]
             --registration_status: student registration status
             -s, --subj: subject code, like GRMN
             -t, --term: term code (ie 202480)
                OR a comma-separated list like '202380,202410'
                OR a named list as defined in lists.R (like 'tl_springs') 
             --uel: use exclude course list (in includes/lists.R)
             -x, --crosslist: 'exclude' removes XLed courses; 'compress' compresses all XLed sections into a single row, with the subject code of the largest section
             ")
    stop("no error")
  }
  
  courses <- load_courses(opt)
  get_enrl_out <- get_enrl(courses,opt)
  message("results from get_enrl:")
  get_enrl_out %>% tibble::as_tibble() %>% print(n=nrow(get_enrl_out), width=Inf)
}



############### FORECAST ############### 
if (opt$func == "forecast") {
  
  # if not supplying sufficient parameters, default to basic reporting
  if (is.null(opt$forecast_method) && is.null(opt$course)) {
    message("setting opt$forecast_method  to 'method'...")
    opt$forecast_method <- "report"
  }
  
  # if course, but no term, default to recent terms
  if (is.null(opt$term) && !is.null(opt$course)) {
    message("setting opt$term  to 'tl_recents'...")
    opt[["term"]] <- "tl_recents"
  }
  
  if (opt$guide){
    message("Forecasting 
      Required params: -c (course) AND  -t (term).
            Both can be either single values, comma-separated strings, or named lists.  
            Output data is basic forecast data for methods specified.
            ")
    quit()
  }
  
  source("cones/forecast/forecast.R")
  source("cones/nosedive/nosedive.R") 
  source("cones/rollcall/rollcall.R") 
  
  # load courses
  courses <- load_courses(opt)
  
  # if method is report, calc accuracy
  if (!is.null(opt$forecast_method)) {
    if (opt$forecast_method == "report") {
      forecast_data <- calc_forecast_accuracy(courses,opt)
      forecast_data %>% tibble::as_tibble() %>% print(n=nrow(.), width=Inf)
    }
  }
   else {
     # load students
     students <- load_students(opt)
     
     # forecast
     forecast_out <- forecast(students,courses,opt)
     
     # display forecast report for opt params
     forecast_data <- calc_forecast_accuracy(courses,opt)
     forecast_data %>% tibble::as_tibble() %>% print(n=nrow(.), width=Inf)
     
   }
  
  
}



########### GRADEBOOK ##############
if (opt$func == "gradebook") {
  if (opt$guide == TRUE) {
    message ("
            Gradebook uses specified filter params to show student grades and DFW rates, but also more specifically the number of drops, withdraws, and fails.
            
            Usually, you'll at least want to filter for a class or small subset of classes.
            Example: -f gradebook -c 'MATH 1130'
            
            use the -a param to aggregate by some combination of course, term, and instructor
            options: course, course_term_avg, course_avg, all (default)
            the all param doesn't return anything, but prints everything in the terminal
            
            Use -h (instead of --guide) to see course filtering params. ")
    stop("no error")
  }
  # example standard output for a specific course 
  # summarized by course WITHOUT instructor data
  # `Academic Period Code` SUBJ_CRSE level `Long Course Title`    `DFW %`   `A+`    A   `A-`  `B+`    B  `B-`   `C+`    C  `C-`  `D+`     D     W passed failed dropped
  # 1 202110                 HIST 1105 lower Making History        36.8      2     6     4     3     5     1     2     0     1     0     0    14     24     14       8
  # 2 202180                 HIST 1105 lower Making History        27.6      0     1     1     2     5     0     3     4     4     1     0     8     21      8       2
  # 3 202210                 HIST 1105 lower Making History         9.52     2    10     2     1     0     1     0     2     0     0     1     2     19      2       3
  # 
  
  source("cones/gradebook/gradebook.R")
  students <- load_students(opt)
  grades_out <- get_grades(students,opt)  
  grades_out %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
}




########### HEADCOUNT ##############
if (opt$func == "headcount") {
  if (opt$guide == TRUE) {
    message ("
            headcount uses specified filter params to show number of students in a unit.
            
            Optional param -a (aggregate) can be set to 'level' to provide undergrad and grad totals for all degrees.

            Usually, you'll at least want to filter for a dept.
            Example: -f headcount -d ECON -a level")
    stop("no error")
  }
  
  # this outputs some rows for inspection
  headcount_out <- count_heads_in_college(opt)
  
  message("headcount_filtered:")
  headcount_out %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
  
  
  # term_code `Academic Year` `Student Level`       Degree               Major                       majors minors major_PRGM major_DEPT
  # <chr>     <chr>           <chr>                 <chr>                <chr>                        <dbl>  <dbl> <chr>      <chr>
  # 1 201980    2019-2020       Associate/Certificate Associate of Arts    Criminology                      1      0 CRIM       SOCI
  # 2 201980    2019-2020       Graduate/GASM         Doctor of Philosophy American Studies                29      0 AMST       AMST
  # 
  
  # TODO: provide better aggregating and filtering based on opt params
}




########### LOOKOUT ##############
if (opt$func == "lookout") {
  if (opt$guide == TRUE) {
    message ("
            lookout uses specified filter params to display for a specific course the most popular:
            - other courses that students are in at the same time
            - courses that students take the semester prior
            - courses that students take the next semester
            when run via CEDAR, it provides terminal output and saves output to 3 .Rda files in CEDAR_OUTPUT_DIR.
            
            This functionality is also used by dept-report and course-report.
            
            Required params: -c (course)
            For example: -c 'ENGL 1120'
             ")
    stop("no error")
  }
  
  # load class list data
  students <- load_students(opt)
  
  # lookout controller doesn't yet return values, but saves 3 Rda files in the output/lookout folder
  lookout_out <- lookout(students, opt)
}



########### NOSEDIVE ##############
if (opt$func == "nosedive") {
  source("cones/nosedive/nosedive.R")
  source("cones/forecast/forecast.R")
  
  # load class list data  
  courses <- load_courses(opt)
  students <- load_students(opt)
  
  nosedive_out <- nosedive(courses, students, opt)  
}


########### ROLLCALL ##############
if (opt$func == "rollcall") {
  if (opt$guide == TRUE) {
    message ("
            rollcall uses specified filter params to show the classification (junior,senior,etc) and major of students.

            Use the -a flag to control summary groups and output: 
            course_major, course_classification, course_classification_major, major, major_wide, classification_wide, all (default)

            Usually, you'll at least want to filter for a class or small subset of classes.
            Example: -f rollcall -c 'MATH 1130'
            
            Use -h (instead of --guide) to see course filtering params. ")
    stop("no error")
  }
  
  source("cones/rollcall/rollcall.R")
  students <- load_students(opt)
  rollcall_out <- rollcall(students, opt)  
  rollcall_out  %>% tibble::as_tibble() %>% print(n = 20, width=Inf)
}



########### SEATFINDER REPORT ##############
if (opt$func == "seatfinder-report") {
  if (opt$guide == TRUE || is.null(opt[["term"]]) ) {
    message ("seat_finder_report compares course offerings from two semesters and creates a simple report about similarities and differences and where there are open seats.\n
              Required params: -t param should be set with a pair of semester codes like  '202380,202480'.
              
              You should also set some filtering params, like level, dept, gen_ed, to make the comparison report readable.
             Use -h to see filtering params. ")
    stop("no error")
  }
  source("cones/seatfinder-report/seatfinder-report.R")
  courses <- load_courses(opt)
  seatfinder_report(courses,opt)  
}



############### WAITLIST  ############### 
if (opt$func == "waitlist") {
  if (opt$guide == TRUE || is.null(opt$course)) {
    message ("
            waitlist uses specified filter params to find the number of students waitlisted for a course who are not also registered.
            Usually, filter by a specific course and term, like:
            -f waitlist -c 'ENGL 1110' -t 202480
            
            Most of the output is terminal only. Students waiting and NOT registered are saved in waitlist_demand.csv
            Use -h to see filtering params. ")
    stop("no error")
  }
  
  students <- load_students(opt)
  waitlist_out <- inspect_waitlist(students,opt)
  waitlist_out %>% tibble::as_tibble() %>% print(n=nrow(waitlist_out), width=Inf)
}


###################################################
################ MISC CONES #######################


############### SALARY BANDS  ############### 
if (opt$func == "salary-bands") {
  source("budget/salary-bands/salary-bands.R")
  salary_bands_out <- salary_bands(courses,opt)
}


########### FEE REPORT ##############
if (opt$func == "fee-report") {
  source("budget/course-fees/course-fees.R")
  courses <- load_courses(opt)
  fee_report(courses, opt)  
}


############### SUMMER REPORT (under development) ##########
# currently requires a dept param 

if (opt$func == "summer-report") {
  courses <- load_courses(opt)
  students <- load_students(opt)
  create_summer_report(courses,students,opt)
} #end summer report


########### WORKLOAD REPORT (under development) ##############
if (opt$func == "workload-report") {
  source("cones/workload-report/workload.R")
  all_courses <- load_courses(opt)
  
  # filter out all 0 enrollments
  all_courses <- all_courses %>% filter (total_enrl > 0)
  
  # produce raw DESRs for ref
  # opt <- list()
  # opt$dept <- "AFST"
  # opt$term <- "202280-202410"
  # 
  # afst_courses_desr <- filter_DESRs(all_courses,opt)
  # write.csv(afst_courses_desr,file="workload/afst_courses_desr.csv")
  # 
  # opt <- list()
  # opt$inst_list <- "AFST_fac"
  # opt$term <- "202280-202410"
  # 
  # afst_fac_courses_desr <- filter_DESRs(all_courses,opt)
  # write.csv(afst_fac_courses_desr,file="workload/afst_fac_courses_desr.csv")
  
  # for studio testing
  # opt <- list()
  # #opt$dept <- "PADM"
  # opt$term <- "202280-202410"
  # opt$inst_list <- "chairs_fac" # TODO: make this not required, or auto gen from HR report; require _fac?
  # opt$uel <- TRUE
  # opt$crosslist <- "compress"
  # opt$summer <- TRUE
  # opt$status = "A"
  # 
  workload_out <- workload_func(all_courses, opt)  
  workload_out <- workload_inst_func(all_courses, opt)  
  #workload %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)
}


############ END PROCESS FUNCTION PARAM ################

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
message("all done. completed in ",time.taken," seconds.","\n")
