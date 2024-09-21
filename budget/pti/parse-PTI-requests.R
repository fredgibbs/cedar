pacman::p_load(tidyverse, readxl, fs, data.table, lubridate)

# create a df for my sheet and previous ones
requests <- data.frame()
old_requests <- data.frame()

# load data from my version of PTI spreadsheet
dir_list <- c("Spring 2024 PTI","Fall 2023 PTI")

for (dir in dir_list) {
  file_list <- dir_ls(dir)
  
  for (file in file_list) {
    print(file)
    xl <- read_xlsx(path=file, sheet=2, skip=3, na="", col_names=FALSE, range = cell_cols("A:AF"))
    xl <- xl[-(1:3),]
    #print(xl)
    term <- read_xlsx(path=file, sheet=1, col_names=FALSE, range = "B9:B9")
    #dept <- read_xlsx(path=file, sheet=1, col_names=FALSE, range = "B5:B5")
    #print(term)
    xl$term <- term
    #xl$dept <- dept
    if (nrow(requests) == 0)  {
      print("initializing requests")
      requests <- xl
    } else {
      print("adding to requests")
      requests <- rbind(requests, xl)
    }
  }
}

# name columns
colnames(requests) <- c("last_name","first_name","id","email","emp_home","rank","crse_home","pt","subj","crse","section","crn","title","hrs","mode","cap","xl","aop","buy","tuition","req_salary","comments","approved","fte","index","account","bcg","act_sal","act_fte","act_index","act_account","bcg2","term")



# load data from old versions of PTI spreadsheet
# processing differences for older sheets
# need to skip 41 lines  (lots of instructions)
# collect A:AJ
#dir_list <- c("~/OneDrive - University of New Mexico/Fall 2022 PTI - Brisha Cruz-Garcia's files/5 PTI Funding Approvals with 7.12 raise - 4.10.22")

dir_list <- c("Fall 2022 PTI")

for (dir in dir_list) {
  print(nrow(old_requests))
  
  file_list <- dir_ls(dir)
  
  for (file in file_list) {
    print(file)
    xl <- read_xlsx(path=file, sheet=2, skip=44, na="", col_names=FALSE, range = cell_cols("A:AJ"))
    xl <- xl[-(1:44),]
    if (nrow(old_requests) == 0)  {
      print("initializing requests")
      old_requests <- xl
    } else {
      print("adding to requests")
      old_requests <- rbind(old_requests, xl)
    }
  }
}


colnames(old_requests) <- c("last_name","first_name","id","email","emp_home","emp_cat","rank","antic_cat","antic_rank", "crse_home","term", "pt","subj","crse","section","crn","title","hrs","mode","cap","xl","aop","buy","tuition","req_salary","comments","approved","fte","index","account","bcg","act_sal","act_fte","act_index","act_account","bcg2")

# select only fields that match new sheets
old_requests <- old_requests %>% select("last_name","first_name","id","email","emp_home","rank","crse_home","pt","subj","crse","section","crn","title","hrs","mode","cap","xl","aop","buy","tuition","req_salary","comments","approved","fte","index","account","bcg","act_sal","act_fte","act_index","act_account","bcg2","term")

# merge old and new requests
requests <- rbind(requests,old_requests)



# remove rows that are all NAs
#requests <- requests[rowSums(is.na(requests)) != ncol(requests),]
requests <- drop_na(requests,last_name)
requests <- drop_na(requests,act_sal)

requests$act_sal <- as.numeric(requests$act_sal)



# handle crse numbers with alpha chars:
#  set lab col to be true or false
requests$crse_base <- requests$crse
requests$lab <- ifelse ( grepl("[[:alpha:]]", requests$crse),TRUE,FALSE)
print(requests)

#strip off last char
# TO DO remove all chars, not just one b/c MATH 1215 is lablebed as 1215XYZ
requests$crse_base[requests$lab == TRUE] <- substring(requests$crse_base[requests$lab == TRUE],1, nchar(requests$crse_base[requests$lab == TRUE])-1)

# convert crse_base to INTEGER
requests$crse_base <- as.integer(requests$crse_base)

# summarize multiple sections of same course
sum <- requests %>% group_by(crse) %>% summarize(n=n())


# LEVEL
requests <- requests %>%
  mutate(level = case_when(
    crse_base >= 1000 ~ "lower",
    crse_base >= 500 & crse_base < 700 ~  "grad",
    crse_base >= 300 & crse_base < 500 ~ "upper"
  ))  

by_level <- requests %>% group_by(level) %>% summarize(count=n()) %>% arrange(desc(count))
#print (by_level,n=30)

#requests <- rbindlist(xls, fill=TRUE)

print(requests)
print(sum)


# careful with col position refs!
#courses <- courses %>% select (CAMP,TERM,CRN,SUBJ,CRSE,SECT,MEET_TYPE,STATUS,PRIM_INST_FIRST,PRIM_INST_LAST,SECT_TITLE,ENROLLED,XL_SUBJ,XL_CRSE,XL_CRN,XL_ENRL,XL_CODE)
#courses$XL_ENRL <- as.integer(courses$XL_ENRL)
# courses <- courses %>% unite(CRSE_SECT, c("CRSE", "SECT"), sep="-", remove=FALSE)
#courses[is.na(courses)] <- 0

save(requests,file="requests.Rda")
