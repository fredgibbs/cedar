pacman::p_load(tidyverse, readxl,ggplot2,dplyr,rvest,lubridate,fs,stringr, data.table)

# load PTI requests data from spreadsheets (from parse-PTI-requests)
load('requests.Rda') # loads requests DF

# load data from parse-DESRs
courses <- load_courses(opt)


# merge enrollment data with request data
# note that if a course has more than one section, section numbers entered differently can create non-distinct rows
# could try to fix sections as entered on pti spread sheet (from 1 to 001) or remove pti section field and then remove dupes
# this issue adds about 45 rows to the 172 pti rows for spring (not counting math)
merged <- merge(requests,courses,by.x=c("id","subj","crse"),by.y=c("PRIM_INST_ID","SUBJ","CRSE"),all.x=TRUE) %>% distinct()


# NOTES ON MERGING
# generally works fine, but non-exact CRNs on PTI spreadsheet mean we get too many matches with enrollment data.
# need to have pti approvals reflect exact courses when contracts are issued. Have Brisha make sure course listed is exact match.
# math is the largest offender; might need to just filter them out for now. 

merged_reduced <- merged %>% distinct(CRN, .keep_all = TRUE)

final <- merged_reduced %>% select (DEPT, subj, crse, SECT, title, PT, INST_METHOD, ENROLLED, total_enrl, last_name, first_name, rank, fte, act_sal,index,XL_SUBJ,XL_CRSE,level.y)


# general spending analysis

# summarize by subj (not unit)
by_subj <- requests %>% group_by(subj) %>% summarize(total=sum(act_sal), count=n()) %>% arrange(desc(count))
print (by_subj,n=30)

by_subj_rank <- requests %>% group_by(subj,rank) %>% summarize(total=sum(act_sal), count=n()) %>% arrange(desc(total))
print (by_subj_rank,n=40)


by_subj_rank <- by_subj_rank %>% group_by(subj,rank) %>% arrange(subj,rank)
print (by_subj_rank,n=100)



# grad student spending
grad_students <- requests %>% filter(rank == "Graduate Student")
grad_student_cost <- grad_students %>% summarise(total = sum(act_sal))

by_subj <- grad_students %>% group_by(subj) %>% summarize(total=sum(act_sal), count=n()) %>% arrange(desc(count))
print (by_subj,n=30)

#TODO: see if overload rates could be different


# overload analysis
# what are we spending on overloads as part of instructional need?
ranks <- c("Assistant Professor","Professor","Associate Professor","Lecturer III","Senior Lecturer III","Principal Lecturer III","Lecturer III","Lecturer II")

overloads <- requests %>% filter(rank %in% ranks)
overload_cost <- overloads %>% summarise(total = sum(act_sal))
overload_beyond_base <- overloads %>% filter(act_sal > 4957.07)
overload_bb_cost <- overload_beyond_base %>% summarise(total = sum(act_sal))

pti_bonuses <- requests %>% filter(act_sal > 4957.07)



# SUBJECT
by_subj <- final %>% group_by(subj) %>% summarize(total=sum(act_sal), count=n()) %>% arrange(desc(count))
print (by_subj,n=30)

#ggplot(by_subj, aes(y=count, x=reorder(subj,-count))) + 
#  geom_bar(position="dodge",stat="identity") +
#  theme(axis.text.x = element_text(angle = -90)) 

#ggsave("count-by-subj.png", width=2400, units="px")
#system2("open", c("count-by-subj.png"))

#ggplot(by_subj, aes(y=total, x=reorder(subj,-total))) + 
#  geom_bar(position="dodge",stat="identity") +
#  theme(axis.text.x = element_text(angle = -90)) 

#ggsave("total-by-subj.png", width=2400, units="px")
#system2("open", c("total-by-subj.png"))


# INST_METHOD
by_method <- final %>% group_by(INST_METHOD) %>% summarize(count=n()) %>% arrange(desc(count))
print (by_method,n=30)

#ggplot(filtered, aes(y=count, x=reorder(mode,-count))) + 
#  geom_bar(position="dodge",stat="identity") +
#  theme(axis.text.x = element_text(angle = -90)) 

#ggsave("by-mode.png", width=2400, units="px")
#system2("open", c("by-mode.png"))


# RANK
filtered <- final %>% group_by(rank) %>% summarize(count=n()) %>% arrange(desc(count))
print (filtered,n=30)

#ggplot(filtered, aes(y=count, x=reorder(rank,-count))) + 
#  geom_bar(position="dodge",stat="identity") +
#  theme(axis.text.x = element_text(angle = -90)) 

#ggsave("by-rank.png", width=2400, units="px")
#system2("open", c("by-rank.png"))


# SUBJ AND COURSE
by_course <- final %>% group_by(subj, crse) %>% summarize(n=n()) %>% arrange(desc(n))
print(by_course)


# LEVEL
by_level <- final %>% group_by(level.y) %>% summarize(count=n()) %>% arrange(desc(count))
print (by_level,n=30)


