pacman::p_load(tidyverse, readxl,ggplot2,dplyr,rvest,lubridate,fs,stringr, data.table,forcats, optparse)
library(ggrepel)

# load external code
source("~/Dropbox/college/r_includes/filter_dept.r")
source("~/Dropbox/college/r_includes/term_codes_labels.r")
source("~/Dropbox/college/r_includes/repel_row.r") # code to help create data labels for dept reports

# this loads sfr_w_counts from create-sfr-df.R
# sfr_college has DEPT code, but not program, which is fine until we have faculty/program data.
load("~/Dropbox/college/dept-reports/v2_code/sfr_w_counts.Rda")
sfr_college <- sfr_w_counts

#save csv file for other uses
write.csv(sfr_college, "~/Dropbox/college/dept-reports/v2_code/csv-files/stud-fac-ratios.csv", row.names=FALSE)


# filter out super high values, usually from tiny depts
sfr_college <- filter (sfr_college, ufr < 60)

# create basic stats
# sd(sfr_college$ufr[sfr_college$DEPT != "COLLEGE"])
# mean(sfr_college$ufr[sfr_college$DEPT != "COLLEGE"])



# compute % change over terms
sfr_changes <- sfr_college %>% 
  group_by(DEPT) %>% 
  arrange(DEPT, desc(term_code)) %>%
  slice(which.max(term_code), which.min(term_code)) %>% 
  mutate(ug_pct_change = round(-diff(ufr)/max(ufr) * 100,digits=0)) %>% 
  mutate(grad_pct_change = round(-diff(gfr)/max(gfr) * 100,digits=0))

sfr_changes <- sfr_changes %>% rename(FTF=count)

#create df for just labels; get the fl_count as of latest term (for y value) and keep SUBJ for x value
pct_changes <- sfr_changes %>% group_by(DEPT) %>% top_n(1, term_code) %>% select(-term_code)

#convert pct_change to long to graph under and grad ratios as stacked bars
pct_long <- gather(pct_changes,level,students,unders,grads) %>% arrange(DEPT)

# plot ufr and gfr for all depts as of lastest semester
ggplot(filter(pct_long,DEPT != "COLLEGE"), aes(y=students, x=reorder(DEPT,students,"sum"))) + 
  geom_bar(aes(fill=level),stat="identity") +
  ggtitle("Student / Faculty Ratios, Fall 2022") +
  xlab("Unit") + ylab("Students per faculty member") +
  theme(legend.position="bottom") +
  scale_fill_discrete("") +
  labs(caption="This data is derived from stud-fac-ratios.csv") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  #scale_fill_gradient2(limits=c(0,40),low="green",mid="gray",high="red",midpoint=pct_changes$ufr[pct_changes$DEPT=="COLLEGE"]) +
  #geom_bar(data=cas_pct_changes,aes(x=reorder(SUBJ,pct_change),y=pct_change),stat="identity",col="red") 
  coord_flip()


# plot percent change compared to college over timeframe
ggplot(pct_long, aes(x=reorder(DEPT,ug_pct_change))) + 
  geom_bar(aes(y=ug_pct_change,fill=ug_pct_change),stat="identity",position="identity",alpha=.5) +
  geom_bar(aes(y=grad_pct_change,col="grad",fill=grad_pct_change),stat="identity",position="identity",alpha=.5) +
  ggtitle("% change in SFRs, ordered by decreasing % change in UG ratio, Fall 2017 - Fall 2022") +
  
  theme(legend.position="bottom") +
  xlab("Unit") + ylab("% change in SFR") +
  labs(caption="This data is derived from stud-fac-ratios.csv", subtitle="Outlined bars show graduate student SFRs") +
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_color_manual(name="",values=c("darkgoldenrod3")) +
  scale_fill_gradient2(name="",limits=c(-50,100),low="red",mid="gray",high="green",midpoint=pct_long$ug_pct_change[pct_long$DEPT=="COLLEGE"]) +
  theme(legend.key=element_rect(fill="white")) +
  coord_flip()


pct_longer <- pct_long %>% select (-level,-students) %>% gather(pct_level,pct_change,ug_pct_change,grad_pct_change) %>% arrange(DEPT)


# plot percent change compared to college over timeframe
ggplot(pct_longer, aes(y=pct_change, x=reorder(DEPT,DEPT))) + 
  geom_bar(aes(fill=(pct_change)),stat="identity") +
  ggtitle("Student / Faculty Ratios") +
  theme(legend.position="bottom") +
  xlab("Unit") + ylab("% change in SFR") +
  labs(caption="This data is derived from stud-fac-ratios.csv") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_gradient2(name="",limits=c(-100,100),low="red",mid="gray",high="green",midpoint=pct_longer$pct_change[pct_longer$DEPT=="COLLEGE"]) +
  facet_wrap("pct_level",ncol=1)
  
