load_funcs <- function(cedar_base_dir) {
  message("loading CEDAR cones...")
  setwd(cedar_base_dir)
  source("includes/config.R")
  
  source("includes/mappings.R")
  source("includes/lists.R")
  source("includes/excluded_courses.R")
  source("includes/gen_ed_courses.R")
  source("includes/misc_funcs.R")
  
  source("includes/filter_class_list.R")
  source("includes/filter_DESRs.R")
  
  source("cones/enrl/enrl.R")
  source("cones/forecast/forecast.R")
  source("cones/forecast-report/forecast-report.R")
  
  source("cones/course-report.R")
  source("cones/dept-report.R")
  
  source("cones/lookout.R")
  source("cones/rollcall.R")
  source("cones/gradebook.R")
  source("cones/seatfinder.R")
  source("cones/regstats.R")
  
  source("cones/headcount.R")
  source("cones/degrees.R")
  source("cones/credit-hours.R")
  source("cones/sfr.R")
  source("cones/waitlist.R")
  source("cones/data-status.R")
  
}
