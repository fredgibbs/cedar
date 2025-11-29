# use prefix dl_ for department lists
dl_humanities <- list("AFST", "AMST", "CCS", "ENGL", "LCL", "HIST", "NATV", "PHIL", "SPAN", "RELG", "WGSS")
dl_soc_sci <- list("ANTH", "CJ", "ECON", "LING", "PADM", "POLS", "PSYC", "SHS", "SOCI")
dl_stem <- list("BIOL", "CHEM","EPS", "MATH", "PHYS")
dl_ind <- list("ISI","LTAM", "MSST", "GES") # sometimes stats tend to be blown out b/c of faculty counts
dl_all <- as.list(c(dl_humanities,dl_soc_sci,dl_stem,dl_ind))


# map between SUBJ codes and DEPT codes 
# even if a unit has only one SUBJ code, it is still listed to ensure lookup success
subj_to_dept_map <- c(
  "AFST" = "AFST",
  "AMST" = "AMST",
  "ANTH" = "ANTH",
  "ASTR" = "PHYS",
  "ARAB" = "LCL", # for pre 2020 data
  "ARBC" = "LCL",
  "BIOL" = "BIOL",
  "CCST" = "CCS",
  "CCS" = "CCS",
  "CHEM" = "CHEM",
  "CHIN" = "LCL",
  "CJ" = "CJ",
  "CLST" = "LCL",
  "COMM" = "CJ",
  "COMP" = "LCL",
  "CRIM" = "SOCI",
  "EAS" = "LCL",
  "ECON" = "ECON",
  "ENGL" = "ENGL",
  "EPS" = "EPS",
  "ENVS" = "EPS",
  "FREN" = "LCL",
  "GEOL" = "EPS",
  "GEOG" = "GES",
  "GNDR" = "WGSS",
  "GREK" = "LCL",
  "GRMN" = "LCL",
  "HIST" = "HIST",
  "HMHV" = "HMHV",
  "INTS" = "ISI",
  "ITAL" = "LCL",
  "JAPN" = "LCL",
  "LANG" = "LCL",
  "LATN" = "LCL",
  "LCL" = "LCL", # not sure there is an LCL subj code, but just in case
  "LING" = "LING",
  "LTAM" = "LTAM",
  "MATH" = "MATH",
  "MDVL" = "ENGL",
  "MLNG" = "LCL",
  "MSST" = "MSST",
  "NAVA" = "LING",
  "NATV" = "NATV",
  "NVJO" = "LING",
  "PADM" = "PADM",
  "PHIL" = "PHIL",
  "PHYC" = "PHYS",
  "PHYS" = "PHYS",
  "POLS" = "POLS",
  "PSY" = "PSYC",
  "PSYC" = "PSYC",
  "PORT" = "SPAN",
  "RELG" = "RELG",
  "RUSS" = "LCL",
  "SHS" = "SHS",
  "SIGN" = "LING",
  "STAT" = "MATH",
  "SOC" = "SOCI",
  "SOCI" = "SOCI",
  "SPAN" = "SPAN",
  "STAT" = "STAT",
  "SUST" = "GES",
  "SWAH" = "LCL",
  "WMST" = "WGSS",
  "WGSS" = "WGSS")

# for easy printing / debugging                            
# subj_dept_map <- data.frame(subj_to_dept_map)

old_code_to_new_code <- c(
"PSY" = "PSYC",
"SOC" = "SOCI",
"GNDR" = "WGSS"
)


# map every informal program_code to a dept_code
prgm_to_dept_map <- c(
  "AFST" = "AFST",
  "AMST" = "AMST",
  "ANTH" = "ANTH",
  "ARBC" = "LCL",
  "ASL" = "LING",
  "ASPH" = "PHYS",
  "ASTR" = "PHYS",
  "ASIN" = "LCL",
  "BIOL" = "BIOL",
  "CCS" = "CCS",
  "CHEM" = "CHEM",
  "CHIN" = "LCL",
  "CJ" = "CJ",
  "CLST" = "LCL",
  "CLCS"="LCL", # comp lit and cult studies
  "COM" = "CJ",
  "COMM" = "CJ",
  "COMP" = "LCL",
  "CRIM" = "SOCI",
  "CRWR"="ENGL",
  "EAS" = "LCL",
  "EAST" = "LCL",
  "ECON" = "ECON",
  "ENGL" = "ENGL",
  "ENGP"="ENGL", # who gets credit for this? half each? all each?
  "ENGS"="ENGL", 
  "ENSC" = "EPS",
  "EPS" = "EPS",
  "FREN" = "LCL",
  "FRNS"="LCL",
  "GEOG" = "GES",
  "GNDR" = "WGSS",
  "GREK" = "LCL",
  "GRMN" = "LCL",
  "GRMS"="LCL",
  "HIST" = "HIST",
  "HMHV" = "HMHV",
  "INTS" = "ISI",
  "ISI" = "ISI",
  "ITAL" = "LCL",
  "JAPN" = "LCL",
  "JOUR"="CJ",
  "JRMC" = "CJ",
  "LANG" = "LCL",
  "LATN" = "LCL",
  "LCL" = "LCL", # not sure there is a LCL program code
  "LING" = "LING",
  "LTAM" = "LTAM",
  "MATH" = "MATH",
  "MCOM" = "CJ",
  "MDVL" = "ENGL",
  "MLNG" = "LCL",
  "MSST" = "MSST",
  "NAVA" = "LING",
  "NATV" = "NATV",
  "NVJO" = "LING",
  "PADM" = "PADM",
  "PAP" = "PHYS",
  "PHIL" = "PHIL",
  "PHYC" = "PHYS",
  "PHYS" = "PHYS",
  "POLS" = "POLS",
  "PSY" = "PSYC",
  "PSYC" = "PSYC",
  "PORT" = "SPAN",
  "RELG" = "RELG",
  "RLST" = "RELG",
  #"RSJ" = "" Institue for Race and Social Justice Certificate
  "RUSS" = "LCL",
  "RUSL" = "LCL",
  "SHS" = "SHS",
  "SIGN" = "LING",
  "STAT" = "MATH",
  "SOC" = "SOCI",
  "SOCI" = "SOCI",
  "SPAN" = "SPAN",
  "STAT"="MATH",
  "SUST" = "GES",
  "TCOM" = "ENGL",
  "WMST" = "WGSS",
  "WGSS" = "WGSS"
  )


# the maps the Major field, Second Major, First Minor, Second Minor to an unofficial PROGRAM CODE
# applies to Graduate and Pending Graduates, Academic Study Guided Adhoc
# minors are included below as "major" since the Major field in many MyReport is the Program Name

# NOTE: to map majors/programs to departments, use this, then prgm_to_dept_map, above)

major_to_program_map <- c(
  "American Studies"="AMST", 
  "Africana Studies"="AFST",
  "African American Studies"="AFST",
  "Anthropology"="ANTH",
  "American Sign Language"="ASL",
  "Arabic" = "ARBC",
  "Asian Studies"="EAS",
  "Astrophyics"="ASTR",
  "Biology"="BIOL",
  "Biology/Biological Sciences"="BIOL",
  "Biochemistry"="BIOC",
  "Chemistry"="CHEM",
  "Chicana and Chicano Studies"="CCS",
  "Chicana Chicano Studies"="CCS",
  "Chicana/o Studies"="CCS",
  "Chinese"="CHIN",
  "Classical Studies"="CLST",
  "Communication Journalism"="CJ",
  "Communication & Journalism"="CJ",
  "Communication"="COMM",
  "Comp Lit & Cultural Stds"="CLCS",
  "Comp Lit & Cultural Studies"="CLCS",
  "Comparative Literature"="CLCS",
  "Criminology"="CRIM",
  "Creative Writing"="CRWR",
  "Earth & Planetary Sciences"="EPS",
  "East Asian Studies"="EAS",
  "Economics"="ECON",
  "English"="ENGL",
  "English Studies"="ENGS",
  "English-Philosophy"="ENGP",
  "Environmental Science"="ENSC",
  "French"="FREN",
  "French Studies"="FRNS",
  "Foreign Languages Literatures"="LCL",
  "Forensic Anthropology"="ANTH",
  "Forensic Science"="ANTH",
  "Geography"="GEOG",
  "German"="GRMN",
  "German Studies"="GRMS",
  "Greek"="GREK",
  "Health,Medicine & Human Values"="HMHV",
  "History"="HIST",
  "International Studies"="ISI",
  "Italian"="ITAL",
  "Japanese"="JAPN",
  "Journal & Mass Comm"="JOUR",
  "Journalism"="JOUR",
  "Journalism & Mass Comm"="JOUR",
  "Languages Cultures & Literatures"="LCL",
  "Languages"="LANG",
  "Latin"="LATN",
  "Latin American Studies"="LTAM",
  "Linguistics"="LING",
  "Mass Communication"="COMM",
  "Mathematics"="MATH",
  "Mathematics Statistics"="MATH",
  "Museum Studies"="MSST",
  "Native American Studies"="NATV",
  "Navajo Language & Linguistics" = "NVJO",
  "Philosophy"="PHIL",
  "Physics"="PHYS",
  "Physics & Astrophysics"="PHYS",
  "Physics Astronomy"="PHYS",
  "Astrophysics"="ASTR",
  "Political Science"="POLS",
  "Political Science Gen Admin"="POLS",
  "Portuguese"="PORT",
  "School of Public Admin"="PADM",
  "School of Public Administration"="PADM",
  "Public Administration"="PADM",
  "Psychology"="PSYC",
  "Religious Studies Prgm"="RELG",
  "Religious Studies"="RELG",
  "Russian"="RUSS",
  "Russian Studies"="RUSS",
  "Signed Language Interpreting"="SIGN",  
  "Signed Language Interpret"="SIGN",  
  "Sociology"="SOCI",
  "Spanish" = "SPAN",
  "Spanish Portuguese" = "SPAN",
  "Spanish & Portuguese" = "SPAN",
  "Speech & Hearing Sciences" = "SHS",
  "Speech and Hearing Sciences" = "SHS",
  "Speech-Language Pathology" = "SHS",
  "Statistics" = "STAT",
  "Sustainability Studies" = "SUST", # SUST has an undergraduate minor
  "Sustainability Studies Program" = "SUST", # not sure if this ever appears
  "Technical & Professional Comm" = "TCOM",
  "Women Studies"="WGSS",
  "Womens Studies"="WGSS",
  "Wmn, Gndr, Sexlty Studies"="WGSS",
  "Women, Gndr, Sexuality Studies"="WGSS")

# These remain unmapped, but aren't part of A&S
# Dramatic Writing (Fine Arts)
# General Science
# General Studies
# Health Education
# Integrative Studies (not sure!)
# Interdisc Film & Digital Media (Fine Arts)
# Interdisc Liberal Arts (Honors College)
# Interdisciplinary Arts (College of Fine Arts)
# Liberal Art
# Liberal Arts (University College)
# Non-Degree


# these appear in the HR employee report Employees with Date Range
# "Department" has already been removed in parse-personnel.R by the time we need the map 
hr_org_desc_to_dept_map <- c("American Studies"="AMST", 
              "Africana Studies"="AFST",
              "African American Studies"="AFST",
              "African American Studies Gen Admin"="AFST",
              "AS Anthropology"="ANTH", 
              "Anthropology"="ANTH",
              "AS Biology"="BIOL",
              "AS Biology General Administrative"="BIOL",
              "AS BA/MD Program"="HMHV",
              "Biology"="BIOL",
              "Biology/Biological Sciences"="BIOL",
              "Biochemistry"="BIOC",
              "Chemistry"="CHEM",
              "Chicana / Chicano Studies"="CCS",
              "Communication Journalism"="CJ",
              "Earth & Planetary Sciences Dept"="EPS",
              "Earth and Planetary Sciences Dept"="EPS",
              "Earth and Planetary Sciences E PS"="EPS",
              "AS Economics"="ECON",                  
              "Economics"="ECON",
              "English"="ENGL",
              "English Studies"="ENGL",
              "English General Administrative"="ENGL",
              "Environmental Science"="EPS",
              "Geography"="GES",
              "Health,Medicine & Human Values"="HMHV",
              "History"="HIST",
              "International Studies Institute ISI"="ISI",
              "Mass Communication"="CJ",
              "Foreign Language Lit Gen Admin"="LCL",
              "Languages Cultures & Literatures"="LCL",
              "Languages Cultures & Lit Gen Admin"="LCL",
              "Foreign Languages Literatures"="LCL",
              "Latin American Studies"="LTAM",
              "Linguistics"="LING",
              "Mathematics Statistics"="MATH",
              "Museum Studies"="MSST",
              "Native American Studies"="NATV",
              "Philosophy"="PHIL",
              "Physics"="PHYS",
              "Physics Astronomy"="PHYS",
              "CAS/CQuIC"="PHYS",
              "Physics Astronomy Gen Admin"="PHYS",
              "Political Science"="POLS",
              "Political Science Gen Admin"="POLS",
              "School of Public Administration"="PADM",
              "Psychology"="PSYC",
              "Religious Studies Prgm"="RELG",
              "Sociology"="SOCI",
              "Spanish Portuguese" = "SPAN",
              "Speech and Hearing Sciences" = "SHS",
              "Speech Hearing Sciences Gen Admin" = "SHS",
              "Sustainability Studies Program" = "SUST",
              "Women Gender and Sexuality Studies" = "WGSS",
              "Womens Studies"="WGSS")


# UNMAPPED from HR report
#Institute for Social Research
#Maxwell Museum Department
#Maxwell Museum
#Earth Data Analysis Center
#Peace Studies
# Arts Sciences Admn Support
# Arts Sciences Advisement
# Arts Sciences Development
# Institute of Meteoritics
#Language Learning Center
#Psychology Department AGORA
#Psychology Clinic
#Center for Stable Isotopes
#Cradle to Career Policy Institute
#History NM Historical Review
# AS Departmental Administration
# AS General Administrative
# Center for Health Policy
# Ctr for Education Policy Research
# Feminist Rsch Institute Gen Admin


# provides the full dept name from standard code
dept_code_to_name <- c(
  "AFST"="Africana Studies",
  "AMST"="American Studies", 
  "ANTH"="Anthropology", 
  "BIOL"="Biology",
  "BIOC"="Biochemistry",
  "CHEM"="Chemistry",
  "CCS"="Chicana and Chicano Studies",
  "CJ"="Communication and Journalism",
  "ECON"="Economics",
  "ENGL"="English",
  "EPS"="Earth & Planetary Sciences",
  "GES"="Geography and Environmental Studies",
  "HIST"="History",
  "HMHV"="Health, Medicine, & Human Values",
  "IMS"="Institute for Medieval Studies", # no programs or curriculum
  "ISI"="International Studies",
  "LCL"="Languages, Cultures & Literatures",
  "LTAM"="Latin American Studies",
  "LING"="Linguistics",
  "MATH"="Mathematics and Statistics",
  "MPP" = "Master of Public Policy",
  "MSST"="Museum Studies",
  "NATV"="Native American Studies",
  "PADM"="School of Public Administration",
  "PHIL"="Philosophy",
  "PHYS"="Physics and Astronomy",
  "POLS"="Political Science",
  "PSYC"="Psychology",
  "RELG"="Religious Studies",
  "SOCI"="Sociology",
  "SPAN"="Spanish & Portuguese",
  "SHS"="Speech and Hearing Sciences",
  "SUST"="Sustainability Studies",
  "WGSS"="Women, Gender, Sexuality Studies"
)

