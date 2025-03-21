# map term labels to codes
# TODO: create script to create between specified params 
term.labs <- c("F17", "Sp18", "Su18","F18", "Sp19", "Su19","F19", "Sp20", "Su20", "F20", "Sp21", "Su21", "F21", "Sp22", "Su22", "F22","Sp23","Su23","F23","Sp24","Su24","F24","Sp25","Su25","F25") 
num.labs <- c("201780", "201810", "201860","201880", "201910", "201960","201980", "202010", "202060", "202080", "202110", "202160", "202180", "202210", "202260", "202280","202310","202360","202380","202410","202460","202480","202510","202560","202580")
term_text <- c("Fall 2017","Spring 2018","Summer 2018","Fall 2018","Spring 2019","Summer 2019","Fall 2019","Spring 2020","Summer 2020","Fall 2020","Spring 2021","Summer 2021","Fall 2021","Spring 2022","Summer 2022","Fall 2022","Spring 2023","Summer 2023","Fall 2023","Spring 2024","Summer 2024","Fall 2024","Spring 2025","Summer 2025","Fall 2025")

# define list of passing grades, used for computing earned credit hours
passing_grades <- c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","CR")

# assign point values to letter grades
grades_to_points <- data.frame(grade=c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","CR","F","NC","NR","W","Drop","I"),
                               points=c(4.3,4,3.7,3.3,3,2.7,2.3,2,1.7,1.3,1,.7,0,0,0,0,0,0,0))


# define course_list_names for critical courses we monitor in the fall
# use prefix cl_ for course lists
cl_engl <- list("ENGL 1110","ENGL 1120")
cl_chem <- list("CHEM 1120C","CHEM 1215","CHEM 1225")
cl_biol <- list("BIOL 1110","BIOL 1140","BIOL 2305")
cl_fyex <- list("FYEX 1010","FYEX 1030")

cl_math_1 <- list("MATH 1130", "MATH 1215", "MATH 1215X", "MATH 1220")
cl_math_2 <- list( "MATH 1230", "MATH 1240", "MATH 1250")
cl_math_3 <- list("MATH 1350", "MATH 1430", "MATH 1440")

cl_comm1000 <- list("COMM 1115", "COMM 1130", "COMM 1140", "COMM 1145", "COMM 1150")
cl_comm2000 <- list("COMM 2120", "COMM 2121", "COMM 2130")

cl_crits <- as.list(c(cl_engl,cl_chem,cl_biol,cl_fyex,cl_math_1,cl_math_2,cl_math_3,cl_comm1000,cl_comm2000))


# define term_list_names 
tl_falls <- c("202280", "202380","202480")
tl_springs <- c("202310","202410","202510")
tl_summers <- c("202460","202560")

tl_recents <- c(tl_falls,tl_springs,tl_summers)


# use prefix dl_ for department lists
dl_humanities <- list("AFST", "AMST", "CCS", "ENGL", "LCL", "HIST", "NATV", "PHIL", "SPAN", "RELG", "WGSS")
dl_soc_sci <- list("ANTH", "CJ", "ECON", "LING", "PADM", "POLS", "PSYC", "SHS", "SOCI")
dl_stem <- list("BIOL", "CHEM","EPS", "MATH", "PHYS")
dl_ind <- list("ISI","LTAM", "MSST", "GES") # sometimes stats tend to be blown out b/c of faculty counts
dl_all <- as.list(c(dl_humanities,dl_soc_sci,dl_stem,dl_ind))