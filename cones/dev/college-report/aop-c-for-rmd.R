pacman::p_load(tidyverse, readxl, fs, data.table, ggrepel)

# load external code
source("~/Dropbox/college/r_includes/filter_dept.r")
source("~/Dropbox/college/r_includes/xlist.r")
source("~/Dropbox/college/r_includes/term_codes_labels.r")
source("~/Dropbox/college/r_includes/map_to_subj_code.r")
source("~/Dropbox/college/r_includes/repel_row.r") # code to help create data labels for dept reports

# load courses data
load("~/Dropbox/college/enrollment/courses.Rda")


# try to find AOP courses and shadow sections
# get AOP sections and their CRNs; then match those CRNs to XL_CRN to get non-AOP section
aop_sections <- courses  %>% filter (INST_METHOD=="MOPS") %>% group_by(TERM,DEPT,CRN) 
non_aop_sections <- courses %>% filter (CRN %in% aop_sections$XL_CRN)
non_aop_sections_no_xl <- xlist_filter(non_aop_sections,"compress")

merged <- rbind (aop_sections,non_aop_sections_no_xl) %>% arrange(TERM,SUBJ,XL_CODE,CRSE_TITLE)
merged <- merged %>% select (-XL_CRN) %>% distinct()

#filter out grad sections XLed with undergrad
merged$XL_CRSE <- as.integer(merged$XL_CRSE)
merged <- merged %>%  filter (XL_CRSE <= 400 | XL_CRSE > 1000)


############ merge personnel data with course data
# this allows us to see who is teaching AOP-related sections

# get faculty data to associate title with person in course listings
load("~/Dropbox/college/personnel/fac_by_term.Rda")

# adjust column types for mergins
merged$`PRIM_INST_ID` <- as.double(merged$`PRIM_INST_ID`)
merged$`TERM` <- as.character(merged$`TERM`)

# merge HR data and courses data
merged <- merge(merged,fac_by_term,by.x=c("TERM","PRIM_INST_ID","DEPT"),by.y=c("term_code","UNM ID","SUBJ"),x.all=TRUE)

merged$`Academic Title` <- str_trim(merged$`Academic Title`)

# select fields
merged <- merged %>% select (TERM,CRN,SUBJ,CRSE_TITLE,INST_METHOD,Name, `Academic Title`,ENROLLED,total_enrl,XL_SUBJ,XL_CRSE,XL_ENRL,XL_CODE) %>% arrange(desc(ENROLLED),TERM,SUBJ,CRSE_TITLE, Name, INST_METHOD) %>%  distinct()
#print <- print %>% arrange(`Academic Title`)
merged <- merged %>% arrange(TERM,SUBJ,CRSE_TITLE,INST_METHOD)



ggplot(merged, aes(x=`Academic Title`)) + 
  geom_point(aes(y=ENROLLED,col=INST_METHOD),stat="identity",position="identity",alpha=.5) +
  ggtitle("Course enrollment by instructor type (and method)") +
  theme(legend.position="bottom") +
  xlab("Course") + ylab("instructor type") +
  #labs(caption="This data is derived from stud-fac-ratios.csv", subtitle="Outlined bars show graduate student SFRs") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 



############ load grades to analyze performance
load("~/Dropbox/college/grades/grades.Rda")

grades <- grades %>%  filter (`Academic Period` == "Fall 2022")

aopgrades <- merge(merged,grades,by.x=c("CRN","TERM"),by.y=c("Course Reference Number","term_code"))

# can compare drops from aop and non-aop sections
nrow(aopgrades[is.na(aopgrades$`Final Grade`) & aopgrades$INST_METHOD!="MOPS", ]) / nrow(aopgrades[is.na(aopgrades$`Final Grade`) & aopgrades$INST_METHOD=="MOPS", ])

# select fields
print <- aopgrades %>% select (TERM,CRN,SUBJ,CRSE_TITLE,INST_METHOD,Name, `Academic Title`,ENROLLED,total_enrl,points,XL_SUBJ,XL_CRSE,XL_ENRL,XL_CODE) %>% arrange(desc(ENROLLED),TERM,SUBJ,CRSE_TITLE, Name, INST_METHOD)
print <- distinct(print)
print(print)

print$points[is.na(print$points)] <- 0
summary <- print %>% group_by(CRN) %>% summarize(mean_points=mean(points))

final <- merge(merged,summary)

final <- final %>% select (TERM,CRN,SUBJ,CRSE_TITLE,INST_METHOD,Name, `Academic Title`,ENROLLED,total_enrl,mean_points,XL_SUBJ,XL_CRSE,XL_ENRL,XL_CODE) %>% arrange(TERM,SUBJ,CRSE_TITLE, Name, INST_METHOD)
