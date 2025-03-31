CEDAR is a command line tool written in R.

To use it, you need to have R installed, and to able to run Rscript from the command line.

Ideally, you can clone it from GitHub, but you can also just download the zip file of the code.
---

## CONFIGURING CEDAR

Using Finder (Mac) or the Windows File Manager, navigate to the "includes" folder in your cedar directory. You don't need to do this on the command line.

Find the cedar/includes/config_template.R, and rename it to config.R
Open the newly renamed config.R file in a text editor, like Notepad++ (Windows) or TextEdit (Mac).  
DO NOT USE Notepad or Word! 

You will see lines for "cedar_base_dir" and "cedar_data_archive_dir". For each, in between the quotes: 
- cedar_base_dir: put the full path to your cedar directory. 
- cedar_data_archive_dir: put the full path to where you will store the raw Excel files from my reports after you've ingested them into CEDAR. If you don't want to archive old MyReports, leave it blank

In both cases: Make sure the path BEGINS and END with a regular slash "/".
On Windows, use either single forward slashes, or double backslashes anywhere they appear. 

If you want to make reports, you need to make sure you have pandoc installed on your machine. See https://pandoc.org/installing.html. On a Mac, it will install to /usr/local/bin/. Set cedar_pandoc_path to "/usr/local/bin/"

Make sure filename extension stays as .R  when you save your file.

---

Now, on the command line, change directories to the cedar folder.

From the commannd line, type  "Rscript cedar.R" (without quotes) and press Enter
If you get an error that Rscript is not found, there is a problem with your system path.

Otherwise, you should see a long list of options and a message that no function was specified. This means EVERYTHING IS WORKING!

For fun, copy and paste the following onto the command line: 
Rscript cedar.R -f enrl -c 'ENGL 1120' -t 202410 -a course

See the cookbook for examples of what you can do.


## Updating Data

GENERAL RULE: When getting current data, always download ALL available fields from MyReports.

Enrollment data from DESRs and class lists are updated more or less frequently depending on the need for enrollment monitoring. 

DESR DATA (used for general enrollment reporting)
Log into MyReports
Click on the Student tab at the top
Expand the Student Reports folder
Double Click Department Enrollment Status
Wait for the Academic Period Code to not be blank
Select appropriate term and wait 5-10 seconds for the fields to refresh
For the Sort Fields and the Select Fields boxes, clikc on the two double arrows pointing to the right to select all fields for download (everything else can be left alone)
Make sure Excel file is output format
Click Run Report
Save download (the xls file) to cedar/data/downloads/DESRs
On the command line, go to cedar/data-parsers
Rscript parse-DESR.R


CLASS LIST DATA (used for some enrollment reporting and anything that needs student data (lookout, gradebook, credit hours, forecast)
Log into MyReports
Click on the Student tab at the top
Expand the Student Reports folder
Double-click on Class_List_Guided_Adhoc
Select appropriate term
For the Course Sort and Student Detail fields, click the double arrorws to the right to select all fields for download
For Registration Status, select All
Make sure Excel file is output format
Click the Run Report button
Save download (the xls file) to cedar/data/downloads/class-lists
On command line, go to cedar/data-parsers
Rscript parse-class-list.R



Other data needs to be updated after each semester:

ACADEMIC STUDY
Log into MyReports
Click on the Student tab at the top
Expand the Student_Reports folder
Double click Academic_Study_Detail_Guided_Adhoc Report 
Select appropriate term
Use the double arrorws to select all fields for download
Make sure Excel file is output format
Click the Run Report button
Save download (the .xls file) to cedar/data/downloads/academic-study
On command line, go to cedar/data-parsers
Rscript parse-academic-study.R

In all three cases, the downloaded xls file is automatically moved from latest folder to the cedar_data_archive_dir specified in cedar/includes/config.R



DEGREE DATA
Degree data is a bit different than other reports because we can download multiple semesters in one file. You can either get the most recent semester and add it to existing data, or all semesters from Fall 209 onward at once. 

Before downloading new files, remove or archive any old files from cedar/data/downloads/degrees, so that the directory is empty. 

Log into MyReports
Click on the Student tab at the top
Expand the Student Reports folder
Double click Graduates and Pending Graduates Guided Ad_hoc 
Select all semesters between Fall 2019 and current term
Save to cedar/data/downloads/degrees
On command line, go to cedar/data-parsers
Rscript parse-degrees.R


HR DATA (for job titles and categories)
Raw data comes from HR Reports (from My UNM), Employee Reports.
To get new data, Log into HR Reports, and use hamburger menu in upper left to select "Employees by Date Range"
Click Select Criteria text box, then "Select By Level 3 Org"
Move A&S to the right-side box with the arrow button
Move all the Orgs to the right-side box with arrow button
start date: 1 sep 2019 (could be anything, but i generally haven't been going beyond 2019 for SFR reporting)
end date: current date
Run Report
[page takes a long time?...]
Click Actions button to "Select Columns"
ADD cols: Home Org, Home Org Desc; click Apply button
Once refreshed, click Actions button to Download. 
Save file as CSV, save to cedar/data/HRreports/ 
(keep original filename)
On command line, go to CEDAR_HOME_DIR/data-parsers
Rscript parse-HRreport.R
