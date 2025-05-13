---
title: CLI guide
parent: Guides 
layout: home
---

# CEDAR CLI


### A note about examples and terminology
Everything after a single or double dash is a flag. There are two kinds of flags:  
- boolean (specifying it turns it on, otherwise it uses the default value (usually off)) 
- option (you must specify a parameter) 

For example, consider:  `Rscript cedar.R -f enrl -c 'MATH 1130' --uel`
- the 'f' and 'c' option flags set their parameters to 'enrl' and 'MATH 1130' respectively.
- the --uel flag is a boolean flag, so it doesn't have a parameter; it just gets turned on 

{: .important }
> Flags names that are just a single character like "f" need one dash in front of them, like -f.  
> Flags names that are more than a single character, require two dashes, like --uel. 
   

For more information about what different cones do, use the --guide flag for more info: 
`Rscript -f enrl --guide` 


## ENROLLMENT 

### How do I see past enrollments for a course? 
Use the course filter: `Rscript cedar.R -f enrl -c 'HIST 491'` 

  
### How do I see past enrollments for an entire unit for a given semester? 
Use the dept and term filters: `Rscript cedar.R -f enrl -d ECON -t 202410` 


### How can I ignore the Dissertation and Independent Study sections? 
Use the "use exclude list" flag (--uel). Courses to be excluded are in the excluded_courses.R file.
`Rscript cedar.R -f enrl -d ECON -t 202410 --uel` 

  
### What have Physics enrollments been like in recent summers? 
The term filter understands term types (fall, spring, summer)
`Rscript cedar.R -f enrl  --uel -d PHYS -t summer` 


How can I quickly see total enrollments for courses with many sections (like ENGL 1120)? 

Use the -a (aggregate) flag, with the 'course' option 

Rscript cedar.R -f enrl --uel -c 'ENGL 1120' -a course 

 

Other aggregating examples: 

Rscript cedar.R -f enrl -c 'ENGL 1110' -a course 

 

Rscript cedar.R -f enrl -t 202480 -c 'ENGL 1110' -a course_type 

 

Rscript cedar.R -f enrl -c 'ENGL 1110' -a course_term 

 

 

Rscript cedar.R -f enrl -c 'ENGL 1110'  -t 202480 -a dept_level 

 

 

Rscript cedar.R -f enrl -t 202480 -a college_level 

 

 

What do 2H enrollments look like for a given term? 

Use the --pt (part of term) filter. 

Rscript cedar.R -f enrl --college AS --pt 2H --uel -t 202410 

 

 

Why are courses listed multiple times?  

Often because they are crosslisted with different values in the XL_SUBJ field. 

For an example, run Rscript cedar.R -f enrl -d AFST -t 202310  

You can compress crosslisted courses into a single row by setting the -x flag to 'compress'. 

Rscript cedar.R -f enrl -d AFST -t 202310 -x compress 

  

  

AOP section enrollments rows are distracting. How can I filter out AOP enrollments? 

It almost always makes more sense to just combine the AOP and non-AOP sections 

If you want to see the AOP sections stand out in an enrollment list: 

Rscript cedar.R -f enrl -c 'SPAN 1110'  -t 202380 --aop compress 

If you want the AOP sections to be totally merged into twin section: 

Rscript cedar.R -f enrl -c 'SPAN 1110'  -t 202380 -x compress 

  

  

What do AOP enrollments look like for a dept? 

Use the --im filter (instructional method); look for the legacy label of MOPS: 

- cedar.R -f enrl -d CCS --im MOPS -t 202410 

 

  

How can I compare available courses across semesters? 

For the most flexibilty, you can run two reports and manually compare: 

Rscript cedar.R -f enrl -d AMST --pt 2H -t 202310  

Rscript cedar.R -f enrl -d AMST --pt 2H -t 202410  

 

  

You can also use the SEATFINDER-REPORT 

Rscript cedar.R -f seatfinder-report -d AMST --pt 2H -t 202310,202410  

This saves an ugly report to CEDAR_OUTPUT_DIR/seatfinder-reports 

  

  

How can I get unit enrollment totals across the College? 

Use the aggregate flag! 

Rscript cedar.R -f enrl --college AS -a college-dept 

 

 

How can I see total waitlist numbers for courses? 

Use the ENRL function with filtering (here, for lower-division A&S courses) 

Rscript cedar.R -f enrl -t 202510 --uel --arrange waiting --college AS -l lower -a course 

 

### COURSE-REPORT 

Use the course report generator to see trends in a particular course. The report will be saved to CEDAR_OUTPUT_DIR/course-reports.
`Rscript cedar.R -f course-report -c 'BIOL 2305' -t 202510` 



### CREDIT-HOURS 

#### How can I see the credit hours for a dept? 
`Rscript cedar.R -f credit-hours -d EPS` 



### DATA-STATUS 
This reports when CEDAR data was updated with MyReports data. 
`Rscript cedar.R -f data-status` 


### DEPT-REPORT 

How can I see general department trends? 

Use the department report generator that saves a report to CEDAR_OUTPUT_DIR/dept-reports 

If sharing the report via OneDrive, use the --output-format flag set to aspx; otherwise use html (default). 

Rscript cedar.R -f dept-report -d ECON 

  

## FORECASTING 
See the separate forecasting guide 


## GRADEBOOK 

### How can I see what grades students are getting in a course? 
`Rscript cedar.R -f gradebook -c 'MATH 1220'` 
  

## HEADCOUNT 
Because there is not much filtering available, probably most useful for creating CSV files.

#### How do I track numbers of students historically in a given program? 
`Rscript cedar.R -f headcount -d ECON` 


How do I get just grad and undergrad headcount totals for a department? 

Rscript cedar.R -f headcount -d ECON -a level 

   

How can I see headcounts across the College? 

Rscript cedar.R -f headcount -a level –t 202480 

  

## LOOKOUT 
Lookout reports what courses students are taking along with a given course, as well as courses taken beforehand and courses they go into afterwards. 

`Rscript cedar.R -f lookout -c 'MATH 1130'`

   

## ROLLCALL 

How can I tell what kinds of students are in a course? 

Rscript cedar.R -f rollcall -c 'MATH 1130' 

  
Use the -a (aggregate) flag to aggregate data by some combintation of course, major, and classification. Options:   

Rscript cedar.R -f rollcall -c 'HIST 434' -a course_classification 
 

Rscript cedar.R -f rollcall -c 'HIST 434' -a course_classification_avg 


Rscript cedar.R -f rollcall -c 'HIST 434' -a major_wide 

Rscript cedar.R -f rollcall -c 'HIST 434' -a classification_wide 

  

## SEATFINDER-REPORT 

### What are the differences between Gen Ed offerings between this year and last? 
`Rscript cedar.R -f seatfinder-report -t '202380,202480'` (be sure to have start date and end date in chronological order) 

### How do I see if we might need more 1H courses next term (based on last year)? 
`Rscript cedar.R -f seatfinder-report --pt 1H -t '202410,202510'` 

### What 2H Gen Ed courses have seats available? 
`Rscript cedar.R -f seatfinder-report --pt 2H -t '202380,202480'` 

  

## WAITLIST 

### How can I tell who is on a waitlist BUT NOT already REGISTERED for that course? 
For example: who is on a waitlist for 2H sections of Spanish 1110 for Fall 2024? 
`Rscript cedar.R -f waitlist --pt 2H -c 'SPAN 1110' -t 202480` 

 

 

 