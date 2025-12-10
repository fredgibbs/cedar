load_funcs <- function(cedar_base_dir) {
  message("[load_funcs.R] Welcome to load_funcs! Loading R files...")
  
  # Save current working directory
  original_wd <- getwd()
  
  # Temporarily change to R directory for sourcing
  setwd(file.path(cedar_base_dir, "R"))
  
  source("lists/drop_cols.R")
  source("lists/excluded_courses.R")
  source("lists/gen_ed_courses.R")
  source("lists/grades.R")
  source("lists/mappings.R")
  source("lists/terms.R")
  
  source("branches/cache.R")
  source("branches/changelog.R")
  source("branches/data.R")
  source("branches/datatable_helpers.R")
  source("branches/filter.R")
  source("branches/logging.R")
  source("branches/majors.R")
  source("branches/misc_funcs.R")
  source("branches/process_func.R")
  source("branches/reporting.R")
  

  source("cones/course-report.R")
  source("cones/credit-hours.R")
  source("cones/degrees.R")
  source("cones/dept-report.R")
  source("cones/enrl.R")
  source("cones/gradebook.R")
  source("cones/headcount.R")
  source("cones/lookout.R")
  # source("cones/offramp.R") # still in development
  source("cones/outcomes.R")
  source("cones/regstats.R")  
  source("cones/rollcall.R")
  source("cones/seatfinder.R")
  source("cones/sfr.R")
  source("cones/waitlist.R")

  # forecasting functions
  source("cones/forecast/forecast.R")
  source("cones/forecast/forecast-stats.R")
  
  
# Restore original working directory
  setwd(original_wd)
  message("[load_funcs.R] Restored working directory to: ", getwd())
}
