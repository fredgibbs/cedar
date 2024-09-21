#install.packages("pacman", repos='http://cran.us.r-project.org')
pacman::p_load(tidyverse, readxl,ggplot2,dplyr,rvest,lubridate,fs,stringr, data.table,forcats, optparse)

library(ggrepel)

# code to map term labels to codes
source("~/Dropbox/college/r_includes/term_codes_labels.r")

# code to help create data labels for dept reports
source("~/Dropbox/college/r_includes/repel_row.r")

# code to filter department and handle variations on subject codes
source("~/Dropbox/college/r_includes/filter_dept.r")

options("width"=300)
options("scipen" = 999)

# 
# program <- c("History")
# program_str <- paste(program,collapse=", ")
# params <- list("dept" = "HIST",
#                  "subj_codes" = c("HIST"),
#                  "crosslist" = "ignore",
#                  "plot_summer_degrees" = "FALSE",
#                  "program" = program,
#                  "program_str" = program_str,
#                  "output" = "TRUE")
# 

# load table with number of faculty per term for each unit
# this file is produced by parse-personnel-2
load("~/Dropbox/college/personnel/fac_by_term_counts.Rda")

### attempt at completing with 0s
fac_by_term_counts <- ungroup(fac_by_term_counts)
fac_by_term_counts <- fac_by_term_counts %>% complete(term_code,SUBJ,job_cat)
fac_by_term_counts <- fac_by_term_counts[!is.na(fac_by_term_counts$job_cat),]
fac_by_term_counts$count[is.na(fac_by_term_counts$count)] <- 0


# # for fun, plot all depts (what a mess!)
# all_depts_plot <- ggplot(fac_by_term_counts) + 
#   #theme(legend.position="bottom") +
#   #guides(color = guide_legend(title = "")) +
#   scale_x_discrete(breaks=num.labs,labels=term.labs) +
#   #geom_line(aes(x=term_code,y=count,group=SUBJ,color=SUBJ)) +
#   geom_line(aes(x=term_code,y=count,group=SUBJ,col=SUBJ), stat="smooth", method=lm, se=FALSE) + 
#   #geom_bar(aes(x=TERM.c,y=per_dept_change, fill="department"),stat="identity", position="identity",alpha=.3) +
#   #geom_line(aes(x=TERM.c,y=diff_per_change, group=2, color="dept net diff"), linetype = "solid") +
#   #scale_colour_manual(values = c("dept net difference" = "black")) +
#   facet_wrap(~job_cat) +
#   labs(color="Job Category") +
#   xlab("Semester") + ylab("FTE (sum of % appointment in unit)") 
# 
# all_depts_plot

# save data file for other uses
write.csv(fac_by_term_counts, "~/Dropbox/college/dept-reports/v2_code/csv-files/all-fac-counts-by-dept.csv", row.names=FALSE)

# filter to only this term
faculty_this_term_counts <- fac_by_term_counts %>% filter(term_code == "202280") %>% select (-term_code)

faculty_this_term_counts$job_cat <- 
  factor(faculty_this_term_counts$job_cat, levels = c('Grad','TPT','Term Teacher','Lecturer','Assistant Professor','Associate Professor','Professor'))

library(RColorBrewer)


# try plotting stacked bars of faculty types for each dept
ggplot(faculty_this_term_counts, aes(x=reorder(SUBJ,count,"sum"), y=count, fill=job_cat)) + 
  ggtitle("Instructor types in each unit, Fall 2022") +
  theme(legend.position="bottom") +
  scale_fill_discrete("") +
  #guides(color = guide_legend(title = "")) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Unit") + ylab("Number of instructors") +
  labs(caption="This data is available in all-fac-counts-by-dept.csv") +
  #scale_fill_brewer(palette = "Pastel1",direction=-1) +
  #scale_fill_grey() +
  coord_flip()


# facet faculty counts by dept
ggplot(faculty_this_term_counts, aes(x=job_cat, y=count, fill=job_cat)) + 
  ggtitle("Instructor types faceted by unit, Fall 2022") +
  theme(legend.position="bottom") +
  scale_fill_discrete("") +
  #guides(color = guide_legend(title = "")) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Instructor type") + ylab("Number of instructors") +
  labs(caption="This data is available in all-fac-counts-by-dept.csv") +
  facet_wrap("SUBJ",ncol=6)
  #scale_fill_brewer(palette = "Pastel1",direction=-1) +
  #scale_fill_grey() +
  #coord_flip()


# facet faculty counts by job type
ggplot(faculty_this_term_counts, aes(x=reorder(SUBJ,count,"sum"), y=count, fill=job_cat)) + 
  ggtitle("Instructor types facted by type, Fall 2022") +
  theme(legend.position="bottom") +
  scale_fill_discrete("") +
  geom_bar(stat="identity") + 
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Unit") + ylab("Number of instructors") +
  theme(axis.text.y = element_text(size = "8")) + 
  labs(caption="This data is available in all-fac-counts-by-dept.csv") +
  facet_wrap("job_cat") +
  coord_flip()


# select just lecturer and professor titles
fac_by_term_counts <- fac_by_term_counts %>% filter (job_cat == "Assistant Professor" | job_cat == "Associate Professor" | job_cat == "Professor" | job_cat == "Lecturer")
fac_by_term_counts$count <- as.double(fac_by_term_counts$count)

# combine both cats for each term
fl_counts <- fac_by_term_counts %>% group_by(term_code,SUBJ) %>% summarize (fl_count = sum(count))


# create row for College total
col_sum <- fl_counts %>% group_by(term_code,SUBJ="COLLEGE") %>%  summarize (fl_count = sum(fl_count))
fl_counts <- rbind(fl_counts, col_sum)

#write.csv(fl_counts, "~/Dropbox/college/dept-reports/v2_code/csv-files/fac-lect-counts-by-dept.csv", row.names=FALSE)

#filter out summer
fl_counts <- fl_counts %>% filter (!str_detect(as.character(term_code), "60"))


#create df for just labels; get the fl_count as of latest term (for y value) and keep SUBJ for x value
fl_counts_w_pos <- fl_counts %>% group_by(SUBJ) %>% mutate (max=max(fl_count)) %>% mutate (min=min(fl_count)) 


# slicing by min and max of fl_count is great at getting min and max values for subj 
fl_counts_slices <- fl_counts_w_pos %>% 
  group_by(SUBJ) %>% 
  arrange(SUBJ, desc(term_code)) %>%
  slice(which.max(term_code), which.min(term_code)) %>% 
  mutate(pct_change = round(-diff(fl_count)/max(fl_count) * 100,digits=0)) 

fl_counts_top_slice <- fl_counts_slices %>% group_by(SUBJ) %>% top_n(1, term_code) %>% select(-term_code)

temp <- distinct(fl_counts_slices %>% select (SUBJ,pct_change))

merged <- merge (fl_counts_w_pos,temp)
# compute mean for each dept over time period
#fl_means <- fac_by_term_counts %>% group_by(SUBJ) %>% summarize (fl_mean = mean(fl_count))


# Classic palette BuPu, with 4 colors
coul <- brewer.pal(4, "PuOr") 

# Add more colors to this palette :
coul <- colorRampPalette(coul)(25)

mycol <- c(coul)

# gradient dots experiment (all terms)
ggplot(merged, aes(y=fl_count, x=reorder(SUBJ,pct_change))) + 
  ggtitle("Faculty (all ranks of professors + lecturers) headcount and % change, Fall 2017, Fall 2022") +
  xlab("Unit") + ylab("# of faculty (prof. and lect.)") +
  labs(caption=paste("This data is derived from all-fac-counts-by-dept.csv.\n College total faculty (2022): ",merged$fl_count[merged$term_code=="202280" & merged$SUBJ=="COLLEGE"], "(",merged$pct_change[merged$term_code=="202280" & merged$SUBJ=="COLLEGE"], "%)",sep="")) +
  theme(legend.position="bottom") +
  geom_point(size=1.8, aes(col=term_code,group=SUBJ)) +
  scale_color_grey(name="",start=.8,end=.2) +
  geom_text(aes(x = SUBJ, y = ifelse(pct_change > 0, max, min),  label = paste(pct_change,"%",sep="")), nudge_y = ifelse(merged$pct_change > 0, 1.7, -1.9)) +
  ylim(0, 50) +
  coord_flip()


# dots showing 2017 and 2022
ggplot(fl_counts_slices, aes(y=fl_count, x=reorder(SUBJ,pct_change))) + 
  ggtitle("Faculty (all ranks of professors + lecturers) headcount, Fall 2017 - Fall 2022") +
  xlab("Unit") + ylab("# of faculty (prof. and lect.)") +
  labs(caption=paste("College total faculty (2022): ",merged$fl_count[merged$term_code=="202280" & merged$SUBJ=="COLLEGE"], "(",merged$pct_change[merged$term_code=="202280" & merged$SUBJ=="COLLEGE"], "%)",sep="")) +
  theme(legend.position="bottom") +
  scale_color_discrete("") +
  labs(caption="Faculty = all ranks of professors + lecturers.\nThis data is derived from all-fac-counts-by-dept.csv") +
  geom_point(size=1.8, aes(col=term_code)) +
  geom_line(arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed")) +
  geom_text(data=fl_counts_top_slice,aes(x = SUBJ, y = fl_count,  label = paste(pct_change,"%",sep="")), nudge_y = ifelse(fl_counts_top_slice$pct_change > 0, 2, -2)) +
  ylim(0, 50) +
  coord_flip()


# plot % change compared to college
ggplot(fl_counts_top_slice, aes(y=pct_change - pct_change[fl_counts_top_slice$SUBJ=="COLLEGE"], x=reorder(SUBJ,pct_change))) + 
  ggtitle("% change in faculty headcount compared to College, Fall 2017 to Fall 2022") +
  labs(caption="Grey bars are simply extremes in the range, excluded from the gradient to make it more useful.\n These are usually because of small department size.\n This data is derived from all-fac-counts-by-dept.csv") +
  xlab("Unit") + ylab("% difference from College") +
  theme(legend.position="bottom") +
  geom_bar(aes(fill=pct_change),stat="identity") +
  scale_fill_gradient2(name="",limits=c(-30,5),low="red",mid="gray",high="green",midpoint=fl_counts_top_slice$pct_change[fl_counts_top_slice$SUBJ=="COLLEGE"])  +
  coord_flip()
