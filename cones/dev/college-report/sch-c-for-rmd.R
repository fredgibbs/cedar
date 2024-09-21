#!/usr/bin/env Rscript

#install.packages("pacman", repos='http://cran.us.r-project.org')
pacman::p_load(tidyverse, readxl,ggplot2,dplyr,rvest,lubridate,fs,stringr, data.table,forcats, optparse)

library(ggrepel)

# code to map term labels to codes
source("~/Dropbox/college/r_includes/term_codes_labels.r")

# code to help create data labels for dept reports
source("~/Dropbox/college/r_includes/repel_row.r")

# code to filter department and handle variations on subject codes
source("~/Dropbox/college/r_includes/filter_dept.r")
source("~/Dropbox/college/r_includes/map_to_subj_code.r")

options("width"=300)
options("scipen" = 999)

# program <- c("Health,Medicine & Human Values")
# program_str <- paste(program,collapse=", ")
# params <- list("dept" = "HMHV",
#                "subj_codes" = c("HMHV"),
#                "crosslist" = "ignore",
#                "plot_summer_degrees" = "TRUE",
#                "program" = program,
#                "program_str" = program_str,
#                "output" = "TRUE")


# load date from parse-DESRs
# courses already filtered for ABQ and EA; colleges filtered for AS and PA
load("~/Dropbox/college/enrollment/courses.Rda")

# select fields
courses <- courses %>% select (TERM,CRN,SUBJ,CRSE,level,SECT,CRSE_TITLE,MEET_TYPE,PRIM_INST_ID,PRIM_INST_LAST,PRIM_INST_FIRST,ENROLLED,TOTAL_HOURS,XL_SUBJ,XL_CRSE,XL_CRN, XL_ENRL,XL_CODE,crse_base) %>% arrange(TERM,crse_base, SECT, PRIM_INST_LAST)

# filter for term
courses <- courses %>% filter(TERM <= 202280)

# remove dupes
courses <- distinct(courses,TERM,CRN,.keep_all = TRUE)

courses <- courses %>% rename(term.code = TERM)


# compress subject codes to dept codes
courses$DEPT <- subj_to_dept_map[courses$SUBJ]

# convert NAs in new DEPT col to whatever is in SUBJ
courses$DEPT <-  ifelse(is.na(courses$DEPT), courses$SUBJ, courses$DEPT)

# save data to CSV for playing in excel
write.csv(courses, "~/Dropbox/college/dept-reports/v2_code/csv-files/all-courses.csv", row.names=FALSE)


# filter out summer and calculate college totals
# make sure filtering is done before lag calculations, or summer m
# even better would be to have separate plots for each semester

# summarize by level, create totals, combine
# sum on DEPT for now, since credit hours are more usually associated with dept than program. But this needs to be more flexible.
depts_summary <- courses %>% group_by(term.code, DEPT, level) %>% summarize (sum = sum(TOTAL_HOURS))
depts_totals <- courses %>% group_by(term.code, DEPT, level="total") %>% summarize (sum = sum(TOTAL_HOURS))
depts_summary <- rbind(depts_summary, depts_totals)

college_summary <- courses %>% group_by(term.code, level, DEPT="COLLEGE") %>% summarize (sum = sum(TOTAL_HOURS))
college_totals <- courses %>% group_by(term.code, DEPT="COLLEGE",level="total") %>% summarize (sum = sum(TOTAL_HOURS))
college_summary <- rbind(college_totals, college_summary)

# note this is grouped by SUBJECT CODE, so will need to compress to aggregate/map them to unit
ch_by_subj <- rbind(depts_summary, college_summary) %>% arrange(term.code,DEPT,level)

#filter out summer for plot and keep for data table
ch_by_subj_w_summer <- ch_by_subj
ch_by_subj <- ch_by_subj %>% filter (!str_detect(as.character(term.code), "60"))

# compute percent change over range: simple calc using first and last values
#totals <- totals %>% add_column(change_total = totals$term_total[which.max(totals$TERM)] / totals$term_total[which.min(totals$TERM)] - 1)
#totals <- totals %>% add_column(change_total_disp = if_else(totals$TERM == max(totals$TERM), format(round(as.numeric(totals$change_total*100),2),nsmall=2), NA_character_))

# filter out totals for stacked graph
ch_by_subj_w_totals <- ch_by_subj
#ch_by_subj <- ch_by_subj %>% filter(level != "total")
#ch_by_subj <- ch_by_subj %>% filter(DEPT != "COLLEGE")


#filter out depts with 0 so they aren't on the plot
ch_by_subj <- ch_by_subj %>% filter (sum > 0)

#filter out military codes
military <- c("MLSL","NVSC","AFAS")
ch_by_subj <- ch_by_subj %>% filter (!(DEPT %in% military))

# save data to CSV for playing in excel
write.csv(ch_by_subj, "~/Dropbox/college/dept-reports/v2_code/csv-files/credit-hours-by-dept.csv", row.names=FALSE)


ch_by_subj_sum_levels <- ch_by_subj %>% group_by(term.code,DEPT) %>% summarise(sum_levels = sum(sum)) %>% arrange(DEPT,term.code)

# computer percent change from first and last terms
ch_by_subj_sum_levels_slices <- ch_by_subj_sum_levels %>% 
  group_by(DEPT) %>% 
  arrange(DEPT, desc(term.code)) %>%
  slice(which.max(term.code), which.min(term.code)) %>% 
  mutate(pct_change = round(-diff(sum_levels)/max(sum_levels) * 100,digits=0)) %>%  arrange(DEPT,term.code)


#get just the latest term data and removing term_code for plotting
#this is not specifically for the dot graph, but uses this data. maybe it doesn't need to.
#this is used for comparison to college, since that plot is 2022 only.
ch_by_subj_slices_recent <- ch_by_subj_sum_levels_slices %>% group_by(DEPT) %>% top_n(1, term.code) %>% select(-term.code)



###### start and end dots
# for this plot, combine all levels
ch_by_subj_sum_levels_slices$term.code <- as.factor(ch_by_subj_sum_levels_slices$term.code)



###### plot DEPT means over timeframe 

# calculate mean
ch_mean_by_subj <- ch_by_subj %>% group_by(DEPT,level) %>% summarize(tf_mean = mean(sum)) %>%  arrange(DEPT,level)

# reorder level grouping
ch_mean_by_subj$level <- factor(ch_mean_by_subj$level, levels = c('grad', 'upper', 'lower'))

ch_mean_by_subj <- ch_mean_by_subj %>% filter(DEPT != "COLLEGE")
ch_mean_by_subj <- ch_mean_by_subj %>% filter(level != "total")


# plot
ggplot(ch_mean_by_subj, aes(x=reorder(DEPT,tf_mean,"sum"), y=tf_mean, fill=level)) + 
  ggtitle("Mean Credit Hours (per semester), Fall 2019 - Fall 2022") +
  theme(legend.position="bottom") +
  #guides(color = guide_legend(title = "")) +
  geom_bar(stat="identity") + 
  scale_color_discrete("") +
  labs(caption="This data is derived from credit-hours-by-dept.csv (from all-courses.csv)") +
  #ylim(0, 35000) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Unit") + ylab("Credit Hours") +
  coord_flip()
  
# plot
ggplot(ch_by_subj_sum_levels_slices, aes(y=sum_levels, x=reorder(DEPT,pct_change))) + 
  ggtitle("Credit Hours (and % change), Fall 2019 and Fall 2022") +
  theme(legend.position="bottom") +
  geom_point(size=2, aes(col=term.code)) +
  scale_color_discrete("") +
  labs(caption="This data is derived from credit-hours-by-dept.csv (from all-courses.csv)") +
  ylim(0, 16000) +
  xlab("Unit") + ylab("Credit Hours") +
  geom_line(color="grey",lineend="round",arrow = arrow(length=unit(0.2,"cm"), ends="last", type = "open")) +
  geom_text(size=3,data=ch_by_subj_slices_recent,aes(x = DEPT, y = sum_levels,  label = paste(pct_change,"%",sep="")), nudge_y = ifelse(ch_by_subj_slices_recent$pct_change > 0, 500, -500)) +
  #labs(caption=paste("College total majors (2022): ",headcount_slices$unders[headcount_slices$term_code=="202280" & headcount_slices$DEPT=="COLLEGE"], "(",headcount_slices$ug_pct_change[headcount_slices$term_code=="202280" & headcount_slices$DEPT=="COLLEGE"], "%)",sep="")) +
  coord_flip()



# plot percent change in majors over timeframe relative to college 
ggplot(ch_by_subj_slices_recent, aes(y=(pct_change - pct_change[ch_by_subj_slices_recent$DEPT=="COLLEGE"]), x=reorder(DEPT,pct_change))) + 
  ggtitle("% change in credit hours compared to College") +
  theme(legend.position="bottom") +
  geom_bar(aes(fill=pct_change),stat="identity") +
  xlab("Unit") + ylab("% change in majors") +
  labs(caption="This data is derived from credit-hours-by-dept.csv (from all-courses.csv)") +
  #ylim(-30, 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_gradient2(name="",limits=c(-60,75),low="red",mid="gray",high="green",midpoint=ch_by_subj_slices_recent$pct_change[ch_by_subj_slices_recent$DEPT=="COLLEGE"])  +
  coord_flip()


    