---
title: CLI guide
parent: Advanced Guides 
layout: home
---

# CEDAR CLI

This page explains how to use CEDAR from the Command Line. 


CEDAR stands for Curriculuar (and)  Enrollment Data Reporting and Analysis

It provides a suite of tools for gathering data and doing common analysis and reporting tasks.

It can be run as CLI tool, Rstudio environment, or as a Shiny web app

---

## CONFIGURING CEDAR

Using Finder (Mac) or the Windows File Manager, navigate to the "includes" folder in your cedar directory. You don't need to do this on the command line.

Find the cedar/includes/config_template.R, and rename it to config.R
Open the newly renamed config.R file in a text editor, like Notepad++ (Windows) or TextEdit (Mac).  
*DO NOT USE Notepad or Word!* 

You will see lines for "cedar_base_dir" and "cedar_data_archive_dir". For each, in between the quotes: 
- cedar_base_dir: specify the full path to your cedar folder. 
- cedar_data_archive_dir: If you want to save downloaded Excel file from MyReports after you've ingested them into CEDAR, specify the full path to the folder where you want to store them. If you don't want to archive old MyReports, leave it blank.

In both cases: Make sure the path BEGINS and END with a regular slash "/".
On Windows, use either single forward slashes, or double backslashes anywhere they appear. 

- cedar_pandoc_path:
If you want to make reports, you need to have pandoc installed on your machine. See https://pandoc.org/installing.html. On a Mac, it will install to /usr/local/bin/, so set cedar_pandoc_path to "/usr/local/bin/"

Make sure filename extension stays as .R  when you save your file.



## Testing Installation

Now, on the command line, change directories to the cedar folder.

From the commannd line, type  "Rscript cedar.R" (without quotes) and press Enter
If you get an error that Rscript is not found, there is a problem with your system path.

Otherwise, you should see a long list of options and a message that no function was specified. This means EVERYTHING IS WORKING!

For fun, copy and paste the following onto the command line: 
Rscript cedar.R -f enrl -c 'ENGL 1120' -t 202410 --group_cols SUBJ_CRSE,TERM

See the cookbook for examples of what you can do.





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




--- 

## COURSE-REPORT 
Use the course report generator to see trends in a particular course. The report will be saved to CEDAR_OUTPUT_DIR/course-reports.
`Rscript cedar.R -f course-report -c 'BIOL 2305'`


---

## CREDIT-HOURS 

### How can I see the credit hours for a dept? 
`Rscript cedar.R -f credit-hours -d EPS` 

### How can I see the credit hours for a college?
 `Rscript cedar.R -f credit-hours --college UC`

### How can I see the credit hours for a college for a specific term?
 `Rscript cedar.R -f credit-hours --college UC -t 202580`

## DATA-STATUS 
This reports when CEDAR data was updated with MyReports data. 
`Rscript cedar.R -f data-status` 

---

## DEPT-REPORT 

### How can I see general department trends? 
Use the department report generator that saves a report to CEDAR_OUTPUT_DIR/dept-reports 
`Rscript cedar.R -f dept-report -d ECON` 

[See an example report](../ANTH.html)

If sharing the report via OneDrive, use the --output-format flag set to aspx; otherwise use html (default). 

---  

## FORECASTING 
See the separate forecasting guide 


---

## GRADEBOOK 

### How can I see what grades students are getting in a course? 
`Rscript cedar.R -f gradebook -c 'MATH 1220'` 

---


## HEADCOUNT 
Because there is not much filtering available, probably most useful for creating CSV files.

### How do I track numbers of students in a program? 
`Rscript cedar.R -f headcount -d ECON` 

Rscript cedar.R -f headcount -d 

How do I get headcount totals for a department based on first major, second major, etc? 
Rscript cedar.R -f headcount -d ECON --group_cols level 

---


## LOOKOUT 
Lookout reports what courses students are taking along with a given course, as well as courses taken beforehand and courses they go into afterwards. 

`Rscript cedar.R -f lookout -c 'MATH 1130'`

---   

## ROLLCALL 

### How can I tell what kinds of students are in a course? 
`Rscript cedar.R -f rollcall -c 'MATH 1130'` 

  
Use the --group_cols flag to aggregate data by some combintation of course, major, and classification. Options:   

Rscript cedar.R -f rollcall -c 'HIST 434' --group_cols course_classification 
 
Rscript cedar.R -f rollcall -c 'HIST 434' -a course_classification_avg 

---

## SEATFINDER-REPORT 

### What are the differences between Gen Ed offerings between this year and last? 
`Rscript cedar.R -f seatfinder-report -t '202380,202480'` (be sure to have start date and end date in chronological order) 

### How do I see if we might need more 1H courses next term (based on last year)? 
`Rscript cedar.R -f seatfinder-report --pt 1H -t '202410,202510'` 

### What 2H Gen Ed courses have seats available? 
`Rscript cedar.R -f seatfinder-report --pt 2H -t '202380,202480'` 

---

## WAITLIST 

### How can I tell who is on a waitlist BUT NOT already REGISTERED for that course? 
For example: who is on a waitlist for 2H sections of Spanish 1110 for Fall 2024? 
`Rscript cedar.R -f waitlist --pt 2H -c 'SPAN 1110' -t 202480` 

 

 

 