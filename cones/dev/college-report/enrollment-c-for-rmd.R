pacman::p_load(tidyverse, readxl, fs, data.table, ggrepel)

# load external code
source("~/Dropbox/college/r_includes/filter_dept.r")
source("~/Dropbox/college/r_includes/xlist.r")
source("~/Dropbox/college/r_includes/term_codes_labels.r")
source("~/Dropbox/college/r_includes/map_to_subj_code.r")

# load courses data
load("~/Dropbox/college/enrollment/courses.Rda")

# set working directory for file outputs
setwd("~/Dropbox/college/outputs")

# map subjs to dept || should now be done in parse-DESRs.
#courses$DEPT <- subj_to_dept_map[courses$SUBJ]
#courses$DEPT <-  ifelse(is.na(courses$DEPT), courses$SUBJ, courses$DEPT)

# handle cross-listing
courses <- xlist_filter(courses,"compress") 

#avoid duplicate CRN/TERM combos (which can happen b/c of other info in courses data)
#courses <- courses_compressed[!duplicated(courses_compressed[,c('CRN','TERM')]),]
courses <- courses %>% distinct(TERM,CRN, .keep_all = TRUE)

#limit to current term as max (no partial enrollments to alter means)
courses <- courses %>% filter(TERM < 202380)

# just using a set min for a semester brings too many rando small sections that are legit courses
# so, we'll compute mean enrolled over data set and then filter
# TODO: need a better way of filtering crosslists besides min enrollment

# for depts, prolly should use ENROLLED but total_enrl for college view to make sure all XLed enrollement gets counted
# this splits up into different entries a course that has had a title change. seems fine.
# small variations in titles make this inexact, but CRNs aren't super reliable either.
courses_mean <- courses %>% group_by (SUBJ,level,CRSE_TITLE,INST_METHOD,PT) %>% summarize(sd=sd(ENROLLED),mean=round(mean(ENROLLED),digits=2), count=n())

# find all tiny courses that shouldn't be counted in calcs
# since 5 is standard XL cap, just try with that, but leave AOP courses alone. not waterproof!
exclude_courses <- courses_mean %>% filter ( mean < 5 & INST_METHOD != "MOPS") %>% select (SUBJ,level,CRSE_TITLE,INST_METHOD,PT,mean,sd,count) %>%  arrange (SUBJ,CRSE_TITLE) %>%  unique()



#subtract courses with v. small means (ind studies, thesis, etc)
# doing this keeps track of one-off small classes that aren't normally small.
courses_mean <- anti_join (courses_mean, exclude_courses) %>% select (SUBJ,level,CRSE_TITLE,INST_METHOD,PT,mean,sd,count) %>%  arrange (SUBJ,CRSE_TITLE) %>%  unique()

# ignore grad classes for now
courses_mean <- courses_mean %>% filter(level=="lower" | level=="upper")

# create summary stats by method and part of term (PT)
college_by_method <- group_by(courses_mean, INST_METHOD,level) %>% 
  summarise(std_dev=sd(mean),mean=mean(mean), TotalCount=sum(count)) %>% arrange(desc(mean))

college_by_pt <- group_by(courses_mean, PT,level) %>% 
  summarise(std_dev=sd(mean),mean=mean(mean), TotalCount=sum(count)) %>% arrange(desc(mean))


#filter by dept
dept_courses <- courses_mean %>% filter(SUBJ=="ECON" & (level=="lower" | level=="upper"))

# NOT WORKING: try to get courses with more than one inst_method for comparison
# this was close, but gets ONLY duplicates
#courses_by_method_pt <- dept_courses[duplicated(dept_courses$CRSE_TITLE),]

# TODO: try dups <-voc.dups[duplicated(voc.dups$id)|duplicated(voc.dups$id, fromLast=TRUE),]
# this should get all rows with any duplicate

write.csv(dept_courses, "dept_course_enrl_summary.csv", row.names=FALSE)


ggplot(dept_courses, aes(x=CRSE_TITLE)) + 
  geom_point(aes(y=mean,col=INST_METHOD,shape=PT),stat="identity",position="identity",alpha=.5) +
  ggtitle("Course enrollment by method and part of term") +
  theme(legend.position="bottom") +
  xlab("Course") + ylab("mean enrollment") +
  #labs(caption="This data is derived from stud-fac-ratios.csv", subtitle="Outlined bars show graduate student SFRs") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  #scale_color_manual(name="",values=c("darkgoldenrod3")) +
  #scale_fill_gradient2(name="",limits=c(-50,100),low="red",mid="gray",high="green",midpoint=pct_long$ug_pct_change[pct_long$DEPT=="COLLEGE"]) +
  #theme(legend.key=element_rect(fill="white")) +
  #coord_flip()


#vars <- group_by(dept_courses, CRSE_TITLE) %>% 
#  summarise(std_dev=sd(rep(mean,count)), TotalCount=sum(count)) %>% arrange(desc(std_dev))

by_method <- group_by(dept_courses, INST_METHOD,level) %>% 
  summarise(std_dev=sd(mean),mean=mean(mean), TotalCount=sum(count)) %>% arrange(desc(mean))

by_pt <- group_by(dept_courses, PT,level) %>% 
  summarise(std_dev=sd(mean),mean=mean(mean), TotalCount=sum(count)) %>% arrange(desc(mean))


#### END BASIC REPORTING




# TODO: need a better way of dealing with split level courses
trouble <- courses_mean %>% filter (mean < 10 & level != "grad")
trouble_count <- trouble %>%  group_by(SUBJ) %>% summarize(n_low = sum(count),n_courses=n())


# find low enrolling courses in latest term
cur_term_courses <- courses %>% filter (level=="upper"  & TERM =="202280") %>% group_by(DEPT) %>% select (SUBJ,CRSE_SECT,CRSE_TITLE,PRIM_INST_LAST,ENROLLED,total_enrl) %>% arrange(DEPT,SUBJ)
low_enrl_courses <- cur_term_courses %>% filter (total_enrl < 10) %>% group_by(DEPT) %>% arrange(DEPT,SUBJ)

low_enrl_courses_sums <- low_enrl_courses %>% group_by(DEPT) %>% summarize (low=n())

total_courses_by_dept <- cur_term_courses %>%  group_by(DEPT) %>% summarize (count=n())

pct_low <- merge (low_enrl_courses_sums,total_courses_by_dept)
pct_low$pct_low <- round(pct_low$low / pct_low$count,digits=2)*100 
pct_low <- arrange(pct_low,-pct_low)

enroll_by_term <- courses %>% group_by(TERM,DEPT) %>% summarize(sum=sum(ENROLLED))


############ merge personnel data with course data

# get faculty data to associate title with person in course listings
load("~/Dropbox/college/personnel/fac_by_term.Rda")

# need to map subj codes to home unit (like FREN to LCL)
merged$`PRIM_INST_ID` <- as.double(merged$`PRIM_INST_ID`)
merged$`TERM` <- as.character(merged$`TERM`)

#courses$dept <- compress_subj_codes
merged <- merge(merged,fac_by_term,by.x=c("TERM","PRIM_INST_ID","DEPT"),by.y=c("term_code","UNM ID","SUBJ"),x.all=TRUE)


# select fields
print <- merged %>% select (TERM,CRN,SUBJ,CRSE_TITLE,INST_METHOD,Name, `Academic Title`,ENROLLED,total_enrl,XL_SUBJ,XL_CRSE,XL_ENRL,XL_CODE) %>% arrange(desc(ENROLLED),TERM,SUBJ,CRSE_TITLE, Name, INST_METHOD)
print <- distinct(print)
print(print)


####
# compare online vs f2f
# compare 8wk vs 16wk
courses_full <- courses %>% filter(PT==1)
sumfun <- courses %>% group_by(DEPT,PT) %>% summarize (mean=mean(ENROLLED))



# left to right bar graph of enrollment by term
ggplot(enroll_by_term, aes(y=sum, x=TERM)) + 
  ggtitle("Enrollment, 2017-2022") +
  theme(legend.position="bottom") +
  geom_line(group="DEPT") +
  #ylim(0, 1500) +
  xlab("Unit") + ylab("Enrollment")



########## MEAN #############

# find mean enrollment in depts and levels across all terms
means <- courses %>% group_by(SUBJ,level) %>% summarise(mean=mean(ENROLLED))

means$level <- factor(means$level, levels = c('grad', 'upper', 'lower'))


# left to right bar graph of headcount
ggplot(means, aes(y=mean, x=reorder(SUBJ,mean,"sum"))) + 
  ggtitle("Mean enrollment, 2017-2022") +
  theme(legend.position="bottom") +
  geom_bar(aes(fill=level),stat="identity") +
  #ylim(0, 1500) +
  xlab("Unit") + ylab("Enrollment") +
  #geom_line(color="grey",lineend="round",arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "open")) +
  #geom_text(size=3,data=pct_changes,aes(x = DEPT, y = unders,  label = paste(ug_pct_change,"%",sep="")), nudge_y = ifelse(pct_changes$ug_pct_change > 0, 40, -40)) +
  #labs(caption=paste("Fall 2022 || College undergrads: ",headcount$unders[headcount$term_code=="202280" & headcount$DEPT=="COLLEGE"], "; grads: ",headcount$grads[headcount$term_code=="202280" & headcount$DEPT=="COLLEGE"],sep="")) +
  facet_wrap("level")+
  coord_flip()
