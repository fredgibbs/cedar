###############################################
# new code for processing faculty employment history for use in CEDAR
# this should be run from the data/parsers directory
# it creates fac_by_term.Rda in the data/processed folder

# Raw data comes from HR Reports (from MyUNM), Employee Reports.
# To get new data, Log into HR Reports
# Click on the hamburger menu (3 parallel lines) in the upper left to select "Employees by Date Range"
# Click Select Criteria text box, then "Select By Level 3 Org", then make sure all the depts are on the right
# start date: 1 sep 2019 (could be anything, but i generally haven't been going beyond 2019 for data reporting)
# end date: current date
# Run Report
# Click Actions button to "Select Columns"
# ADD cols: add Appt %, Home Org, Home Org Desc
# once refreshed, click Actions button to Download. 
# save file as CSV, put it in cedar/data/HRreports
# keep original filename that is datestamped.

# A note about JOB CODES
# N1 = non-standard pay 
# T1 = teaching overload
# S1 = SAC
# FTR = summer research
# A1 = summer admin

###############################################

pacman::p_load(tidyverse, readxl,dplyr,rvest,lubridate,fs,stringr, data.table,forcats)


# load basic includes 
source("load-parser-includes.R")

# load master csv file from HRreports
data_dir <- paste0(cedar_data_dir,"downloads/HRreports")

message("looking for new HR data in ", data_dir)
file_list <- dir_ls(data_dir)
file <- file_list[1]
all_emps <- read_csv(file)

message("CSV file loaded. processing...")

# remove summer research, since this isn't relevant to teaching duties
all_emps <- all_emps %>% filter (`Job Title` != "Summer Research" & `Job Title` != "#Summer Research")

# find " Department" and remove it for easier conversion to subject codes
message("removing 'Department' from values in 'Org Description' col...")
all_emps$`Org Description` <- str_replace(all_emps$`Org Description`," Department", "")

# add department code for easier filtering later
message("adding DEPT code...")
all_emps$DEPT <- hr_org_desc_to_dept_map[all_emps$`Org Description`]

# print any missing DEPTS 
message("rows missing DEPT:")
subj_nas <- all_emps$'Org Description'[is.na(all_emps$DEPT)]
unique(subj_nas) %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)

# drop any rows without an assigned department
all_emps <-all_emps %>% drop_na(DEPT)

# the start date is original hire (not current job title) but there doesn't seem a more specific alternative
# if end date is blank, use contract date
# some job end dates will remain NA for emertius, retired employeee, widower
all_emps <-  all_emps %>% mutate(`Job End Date` = coalesce(`Job End Date`,`Contract End Date`))

# filter out rows without and End Date
all_emps <- all_emps %>% filter(!is.na(`Job End Date`))

# add new date columns for easier processing
all_emps <- all_emps %>% mutate (begin_date = mdy(`Job Begin Date`))
all_emps <- all_emps %>% mutate (job_end_date = mdy(`Job End Date`))

# all dfs for each term will be stored here
dfs_by_term <- list()

# for each term defined in num.labs, find active employees
# that means start date is before, and end date is after
# only get rows if job suffix is 00, for primary appointment
# better to do term by term rather than filter everything at once

# num.labs defined in includes/mappings.R. 
for (p in num.labs) {
  print (paste("processing: ", p))
  p_date <- ymd(code_to_date(p))
  print(p_date)
  term_emps <- all_emps %>% filter (begin_date <= p_date & job_end_date >= p_date & `Job suffix` == "00")
  term_emps$term_code <-p
  dfs_by_term <- rbind(dfs_by_term,term_emps)
}

#TODO: use Home Org Description from HRreport?

# set all job cats to NA and gradually fill them in
dfs_by_term$job_cat <- as.character(NA)

# job title covers specific duties (admin, coordinator, etc)
# job title is always CURRENT title, not from date under evaluation; use Academic Title
# academic title better represents standard job title related to teaching (associate professor)
# NOTE: academic title lists term teacher all the time even for row terms when that person wasn't a term teacher

# TODO: maybe get all job titles for a given semester and combine into one string?

#if academic title is NA, use Job Title
dfs_by_term[is.na(dfs_by_term$`Academic Title`),]$`Academic Title` = dfs_by_term[is.na(dfs_by_term$`Academic Title`),]$`Job Title`

# correct some outliers; prolly not a complete list!
dfs_by_term[dfs_by_term$`Academic Title` == "Rank of Discipline",]$`Academic Title` = dfs_by_term[dfs_by_term$`Academic Title` == "Rank of Discipline",]$`Job Title`
dfs_by_term[dfs_by_term$`Academic Title` == "Director of Africana Studies",]$`Academic Title` = dfs_by_term[dfs_by_term$`Academic Title` == "Director of Africana Studies",]$`Job Title`
dfs_by_term[dfs_by_term$`Academic Title` == "professor of Mathematics & Statistics",]$`Academic Title` = dfs_by_term[dfs_by_term$`Academic Title` == "professor of Mathematics & Statistics",]$`Job Title`

# for consistency, need to remove "of x" and "in x" in titles
dfs_by_term$`Academic Title` <- sub(" (in|of) .*", "", dfs_by_term$`Academic Title`)


# standardize job titles for better reporting
message("standardizing job titles...")
dfs_by_term[dfs_by_term$`Academic Title` == "Professor",]$job_cat = "Professor"
dfs_by_term[dfs_by_term$`Academic Title` == "Professor Emeritus",]$job_cat = "Professor Emeritus"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Assistant Professor",]$job_cat = "Assistant Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Research Asst Professor",]$job_cat = "Assistant Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Assistant Prof",]$job_cat = "Assistant Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Associate Professor",]$job_cat = "Associate Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Research Associate Professor",]$job_cat = "Associate Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Assoc Professor",]$job_cat = "Associate Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Assoc  Professor",]$job_cat = "Associate Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Associate  Professor",]$job_cat = "Associate Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Associate Profesor",]$job_cat = "Associate Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Distinguished Professor",]$job_cat = "Professor"
dfs_by_term[dfs_by_term$`Academic Title` == "Research Assistant Professor",]$job_cat = "Assistant Professor"
dfs_by_term[dfs_by_term$`Academic Title` == "Research Associate Professor",]$job_cat = "Associate Professor"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Lecture",]$job_cat = "Lecturer"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Term Teaching",]$job_cat = "Term Teacher"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Part Time Faculty",]$job_cat = "TPT"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Visiting Professor",]$job_cat = "TPT"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Visiting Instructor",]$job_cat = "TPT"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Adjunct Faculty",]$job_cat = "TPT"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Temp",]$job_cat = "TPT"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Visting Scholar",]$job_cat = "TPT"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Visiting Scholar",]$job_cat = "TPT"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Teaching Assistant",]$job_cat = "Grad"
dfs_by_term[dfs_by_term$`Academic Title` == "Teaching Asst Regular",]$job_cat = "Grad" #new after CBA?
dfs_by_term[dfs_by_term$`Academic Title` == "Teaching Asst Special",]$job_cat = "Grad" #new after CBA?
dfs_by_term[dfs_by_term$`Academic Title` == "Research Assistant",]$job_cat = "Grad"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Project Assistant",]$job_cat = "Grad"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Graduate Assistant",]$job_cat = "Grad"
dfs_by_term[dfs_by_term$`Academic Title` == "Graduate Asst Regular",]$job_cat = "Grad" #new after CBA?
dfs_by_term[dfs_by_term$`Academic Title` == "Graduate Asst Special",]$job_cat = "Grad" #new after CBA?
dfs_by_term[dfs_by_term$`Academic Title` %like% "Teaching Associate",]$job_cat = "Grad"
dfs_by_term[dfs_by_term$`Academic Title` %like% "Faculty Working Retiree",]$job_cat = "Professor"

#TODO: postdocs? so many variants! maybe just grep for any postdoc

# capture any rows that don't get mapped to job cat to see what we're not catching above.
message("Academic Titles not mapped to a job category:")
nas <- dfs_by_term[is.na(dfs_by_term$job_cat),]
unique(nas$`Academic Title`) %>% tibble::as_tibble() %>% print(n = nrow(.), width=Inf)

# Extract download date from filename, supplied by MyReports
file_date <- str_extract(file, "[0-9]{4}[0-9]{2}[0-9]{2}")

# add column as_of_date so we know how recent data is
dfs_by_term$as_of_date <- ymd(file_date)


# select relevant fields 
fac_by_term <- dfs_by_term %>% 
    select (term_code, DEPT, `UNM ID`, Name, `Academic Title`, `Job Title`,  job_cat, `Home Organization Desc`, `Appt %`, as_of_date) %>% 
    distinct()


# save for other uses (esp combining with course data)
file_name <- paste0(cedar_data_dir,"processed/fac_by_term.Rda")
message("saving ", file_name,"...")
save(fac_by_term,file=file_name)
message("parse HRreport complete!")
