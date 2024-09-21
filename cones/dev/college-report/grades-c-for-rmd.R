pacman::p_load(tidyverse, readxl, fs, data.table, ggrepel)


load("~/Dropbox/college/grades/grades.Rda")


# filter by campus, including online
filtered <- grades %>% filter (`Course Campus`=="Online & ITV" | `Course Campus`=="Albuquerque/Main") 

filtered <- filtered %>% filter (`Subject Code`=="MATH" & `Course Number`=="1215X" & `Primary Instructor Last Name`=="Adamson") 

filtered <- filtered %>% select (`Academic Period`,`Primary Instructor Last Name`,`Student ID`,`Final Grade`)

filtered <- filtered %>% replace_na(list(`Final Grade`="Drop"))
unique(filtered$`Final Grade`)

sum_students <- filtered %>%  group_by(`Academic Period`) %>%  summarize( `total` = n())
sum_students

sum_grades <- filtered %>% group_by(`Academic Period`,`Final Grade`, .drop=FALSE) %>% summarize( `students` = n()) 
print(sum_grades, n=Inf)

ggplot(sum_grades, aes(x=`Final Grade`, y=`students`)) +
  geom_bar(stat="identity", position="stack") + theme(axis.text.x = element_text(angle = 0)) +
  scale_x_discrete(limits = c("A+","A","A-","B+","B","B-","C+","C","CR","C-","D+","D","D-","F","NC","NR","W","Drop"))


passing <- c("A+","A","A-","B+","B","B-","C+","C","CR")
failing <- c("C-","D+","D","D-","F","cC","NR","W","Drop")

passes <- sum_grades %>%  filter (`Final Grade` %in% passing)   %>% summarize( passes = sum(students)) 
passes

fails <- sum_grades %>%  filter (`Final Grade` %in% failing)   %>% summarize( fails = sum(students)) 
fails

df_list <- list(sum_students,passes,fails)
totals <- df_list %>%  reduce (full_join, by="Academic Period" )
totals

totals$passrate <- totals$passes / totals$total
totals$failrate <- totals$fails / totals$total

term.labs <- c("Fall 2019", "Spring 2020", "Summer 2020", "Fall 2020", "Spring 2021", "Summer 2021", "Fall 2021", "Spring 2022", "Summer 2022", "Fall 2022") 

ggplot(totals, aes(x=`Academic Period`, y=value, color=variable)) + 
  scale_x_discrete(labels=term.labs) +
  geom_line(aes(y=passrate, col="Passes", group=1)) +
  geom_line(aes(y=failrate, col="Fails", group=1))
