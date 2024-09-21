pacman::p_load(tidyverse, readxl,ggplot2,dplyr,rvest,lubridate,fs,stringr, data.table,forcats, optparse)
library(ggrepel)

# program <- c("French","French Studies","German","German Studies","Russian","Languages","Classical Studies","East Asian Studies","Comp Lit & Cultural Studies")
# program_str <- paste(program,collapse=", ")
# params <- list("dept" = "LCL",
#                  "subj_codes" = c('LCL','MLNG','COMP','SWAH','JAPN','CHIN','GREK','LATN','ARAB','FREN','GRMN','RUSS','ITAL','CLST'),
#                  "crosslist" = "ignore",
#                  "plot_summer_degrees" = "FALSE",
#                  "program" = program,
#                  "program_str" = program_str,
#                  "output" = "TRUE")



# load external code
source("~/Dropbox/college/r_includes/filter_dept.r")
source("~/Dropbox/college/r_includes/term_codes_labels.r")
source("~/Dropbox/college/r_includes/repel_row.r") # code to help create data labels for dept reports

# this loads sfr_w_counts from create-sfr-df.R
load("~/Dropbox/college/dept-reports/v2_code/sfr_w_counts.Rda")

# might want to get from majors instead so PRGM information is intact.

headcount <- sfr_w_counts


#complete on term and dept so 0s get added
comp <- complete(headcount,term_code,DEPT)

headcount <- comp %>% filter (unders > 1)

#TODO: make a level field instead of sep cols for grad and undergrad
hc_long <- headcount %>% select (term_code,DEPT,unders,grads)
hc_long <- hc_long %>% rename(under="unders",grad='grads')
hc_long <- gather (hc_long, level, students, under, grad)

# filter out college for stacked graph
hc_long <- hc_long %>% filter(term_code == "202280")
hc_long <- hc_long %>% filter(DEPT != "COLLEGE")


# compute % change over terms
# TODO: add ability to compute by PRGM (available from aggreagate-majors); maybe need to load majors instead of sfr_w_counts
headcount_slices <- headcount %>% 
  group_by(DEPT) %>% 
  arrange(DEPT, desc(term_code)) %>%
  slice(which.max(term_code), which.min(term_code)) %>% 
  mutate(ug_pct_change = round(-diff(unders)/max(unders) * 100,digits=0)) 


pct_changes <- headcount_slices %>% group_by(DEPT) %>% top_n(1, term_code)



# left to right bar graph of headcount
ggplot(hc_long, aes(y=students, x=reorder(DEPT,students))) + 
  ggtitle("Number of majors, Fall 2022") +
  theme(legend.position="bottom") +
  geom_bar(aes(fill=level),stat="identity") +
  #ylim(0, 1500) +
  scale_fill_discrete("") +
  xlab("Unit") + ylab("Majors (incl. pre and second majors)") +
  #geom_line(color="grey",lineend="round",arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "open")) +
  #geom_text(size=3,data=pct_changes,aes(x = DEPT, y = unders,  label = paste(ug_pct_change,"%",sep="")), nudge_y = ifelse(pct_changes$ug_pct_change > 0, 40, -40)) +
  labs(caption=paste("Fall 2022 College undergrads: ",headcount$unders[headcount$term_code=="202280" & headcount$DEPT=="COLLEGE"], "; grads: ",headcount$grads[headcount$term_code=="202280" & headcount$DEPT=="COLLEGE"],"\nThis data is derived from stud-fac-ratios.csv",sep="")) +
  coord_flip()


# working with annotations
# ylim removes college from blowing out plot
ggplot(headcount_slices, aes(y=unders, x=reorder(DEPT,ug_pct_change))) + 
  ggtitle("Majors (and % change), Fall 2017 and Fall 2022") +
  theme(legend.position="bottom") +
  geom_point(size=2, aes(col=term_code)) +
  ylim(0, 1500) +
  scale_color_discrete("") +
  xlab("Unit") + ylab("Majors (incl. pre and second majors)") +
  geom_line(color="grey",lineend="round",arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "open")) +
  geom_text(size=3,data=pct_changes,aes(x = DEPT, y = unders,  label = paste(ug_pct_change,"%",sep="")), nudge_y = ifelse(pct_changes$ug_pct_change > 0, 40, -40)) +
  labs(caption=paste("College total majors (2022): ",headcount_slices$unders[headcount_slices$term_code=="202280" & headcount_slices$DEPT=="COLLEGE"], " (",headcount_slices$ug_pct_change[headcount_slices$term_code=="202280" & headcount_slices$DEPT=="COLLEGE"], "%)","\nThis data is derived from stud-fac-ratios.csv",sep="")) +
  coord_flip()


pct_changes <-  pct_changes %>% select(-term_code)

# plot percent change in majors over timeframe relative to college 
ggplot(pct_changes, aes(y=(ug_pct_change - ug_pct_change[pct_changes$DEPT=="COLLEGE"]), x=reorder(DEPT,ug_pct_change))) + 
  ggtitle("% change in majors compared to College, 2017-2022") +
  theme(legend.position="bottom") +
  geom_bar(aes(fill=ug_pct_change),stat="identity") +
  xlab("Unit") + ylab("% change in majors") +
  #ylim(-30, 30) +
  labs(caption="This data is derived from stud-fac-ratios.csv") + 
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_gradient2(name="",limits=c(-60,75),low="red",mid="gray",high="green",midpoint=pct_changes$ug_pct_change[pct_changes$DEPT=="COLLEGE"])  +
  coord_flip()

