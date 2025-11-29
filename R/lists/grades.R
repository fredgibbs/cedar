# define list of passing grades, used for computing earned credit hours
# passing_grades <- c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","CR")
passing_grades <- c("A+","A","A-","B+","B","B-","C+","C","CR")

# assign point values to letter grades
grades_to_points <- data.frame(grade=c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","CR","F","NC","NR","W","Drop","I"),
                               points=c(4.3,4,3.7,3.3,3,2.7,2.3,2,1.7,1.3,1,.7,0,0,0,0,0,0,0))
