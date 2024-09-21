# this function provides data on the major and classification of students in a course or set of courses
# no params required, but then too much data is outputted
# best to specify a course (or course_list)

create_summer_report <- function (courses,students,opt) {
  
  # for studio testing...
  opt <- list()
  opt$status <- "A"
  # opt$college <- "AS" # needs to be commented out so calling rollout doesn't think we're trying to aggregate at college level. NEED TO FIX!
  opt$uel <- TRUE
  opt$func <- "summer-report"
  opt$dept <- "CHEM"
  opt$term <- "202260,202360"
  opt$aggregate <- TRUE
  opt$aggregate_by <- "term"
  opt$summer <- TRUE
  
  
  # get enrollment 
  # TODO: use better method than saving multiple CSV files
  get_enrl(opt, courses)
  filename <- paste0("enrl/summaries/",opt$dept,"_enrollment_summary_across_terms.csv")
  message("loading rollout output file: ",filename) # DF is named summary
  summary_across_terms <- read_csv(filename)
  
  filename <- paste0("enrl/summaries/",opt$dept,"_enrollment_summary_by_term.csv")
  message("loading rollout output file: ",filename) # DF is named summary
  summary_by_term <- read_csv(filename)
  
  
  # rollcall
  rollcall(students, opt)
  filename <- paste0("rollcall/",opt$dept,"_agg_by_major.csv")
  message("loading rollcall output file: ",filename) # DF is named summary
  agg_by_major <- read_csv(filename)
  
  filename <- paste0("rollcall/",opt$dept,"_agg_by_class.csv")
  message("loading rollcall output file: ",filename) # DF is named summary
  agg_by_class <- read_csv(filename)
  
  # grades
  grades_out <- get_grades(students,opt)
  
  
  # payload
  d_params <- list("dept" = opt$dept,
                   "term" = opt$term,
                   "tables" = list(
                     "summary_by_term" = summary_by_term,
                     "summary_across_terms" = summary_across_terms,
                     "agg_by_major" = agg_by_major,
                     "agg_by_class" = agg_by_class,
                     "grades_out" = grades_out
                   )
  )
  
  unloadNamespace("kableExtra")  
  envir = new.env()
  output_file <- paste0(base_dir,"beehive/summer-report/reports/",opt$dept,'-summer-report.html')
  
  message("rendering summer report for ", opt$dept)
  # rmarkdown::render('course-report/course-report.Rmd', 
  #                   output_format = 'html_document',
  #                   output_file = output_file,                  
  #                   params = d_params)
  # 
  
  xfun::Rscript_call(
    rmarkdown::render,
    list(input = 'summer-report/summer-report.Rmd', output_format = 'html_document',output_file = output_file,params = d_params)
  )
  
  message("done rendering.")
  
}