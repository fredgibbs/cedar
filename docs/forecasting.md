---
title: Forecasting
layout: home
nav_order: 20
---

# CEDAR Forecasting


## THIS PAGE UNDER CONSTRUCTION!

## Data updates 
Before forecasting, it may be helpful to update DESR and Class List data from MyReports. See the [data page](data) for details on how to do this.
 

## Accuracy and demand 
When looking at forecast output, the accuracy columns report the extent to which forecasts matched prior enrollments (compared to all registrations, enrollment at census, and end-of-term enrollment). Despite the "accuracy" label, this doesn't necessarily indicate how accurate the forecasts were because enrollments are throttled by many variables, including cap sizes, instructor availability, historical precedent, funding, etc. 

Obviously, forecasts can be off for many reasons. BUT, seeing consistently higher predictions than enrollments isn't necessarily an overestimate, but perhaps a suggestion that there is more demand than being met.  

 


## TL;DR SUMMER SCHOOL QUICK START GUIDE 

Update data as needed DESRs (and class_lists) 

Forecasting can only really estimate enrollments for courses we've offered in previous term types. Forecasting a list of relevant course from the previous term type is a good start to see if demand has increased. Run these commands to generate a list of courses from last summer and then forecast for them 

`Rscript cedar.R -f enrl -t 202460 --uel -l lower --college AS -a course --output csv` 

`Rscript cedar.R -f forecast -c enrollments.csv -t 202560` 

 

IMPORTANT REMINDER: The regstats reports are good indicators of NEW courses that might enroll well, even for summer, and likely better show impacts of new scholarship rules. 

 

Create a regstats report to see what courses are being flagged (based on threshold filtering for enrollment/drop anomolies) in the term(s) PRIOR to the term you want to forecast for: 

Rscript cedar.R -f regstats -t 202510 --output csv 

This creates a group of CSV files in the output/csv folder, in case you want to handle them separately, but... 

All "flagged" courses (meaning they seem to deviate from their normal enrollments or drops, etc) are combined in all_flagged_courses.csv 

You can also specify--output html or --output aspx if you want non-CSV files to share 

 

To forecast for flagged courses (via the CSV file created above): 

Rscript cedar.R -f forecast -c all_flagged_courses.csv -t 202560  

This creates two CSV files: output/csv/forecasts-short.csv 
 

Use the forecast-report function with filtering to display forecasts of interest (like all summer forecasts), for determining needed sections 

Rscript cedar.R -f forecast-report -t summer --output csv 

Rscript cedar.R -f forecast-report -c ENGL 1110 --output html 
 

Regstats also creates a csv file called high_fall_sophs.csv that lists courses that sophomores take in the fall and therefore may be useful to offer in summer 

 

## Augment regstats with course reports 
Because COURSE REPORTS provide valuable insight into course dynamics over the last few years, they help determine if the algorithmic forecasts/recommendations make sense and identify new courses to offer 

Especially useful for investigating courses that haven't been offered in summer but maybe should be! 

Course reports can be created individually: 
- `Rscript cedar.R -f course-report -c 'BIOL 1110'` 

Easier to generate a set of course reports from some of the CSV files created by regstats (in the output/csv folder): 

- `Rscript cedar.R -f course-report -c drops.csv` 

You can generate course reports for courses you've already forecasted for: 
- `Rscript cedar.R -f course-report -c forecasts`  

WARNING: This generates probably way more reports than you really need!  



## Forecasting Introduction
The forecasting function uses previous enrollment data (one term or one semester depending on the method) to forecast enrollment for specified courses in future semesters. You can do individual course forecasts by course and “target” term to forecast for. You can also use set course lists and term lists (defined in lists.R). You can use the regstats function to automatically flag courses to forecast for given recent enrollment shifts.   
 

METHODS 

For a given "target" course and term: 

Conduit 

Get student IDs for students in previous (1 year ago) target course 

Identify what courses students are coming from before taking the given course. These are the "conduit" courses. 

Identify how enrollments in the conduit courses changed from a year prior to target course. 

Apply that pct diff (weighted by the amount of their mean contribution) to mean target course enrollments 

 

Major 

Instead of just looking at course enrollments as the CONDUIT method does, this MAJOR method looks at how MAJORS changed from prev conduit term to conduit term (the conduit term is just the term before the target term; the prev conduit term is one year prior to maintain term type) 

Apply the change in majors percentage to prev_target_term enrollments 

Compute how much that will contribute to target term enrollment based composition of majors in target course 

Requires a course to have been offered a year previously 

 

FORECAST RECIPES 

Forecasting always requires a course or course list (as defined in lists.R). A term is not required, but forecast will default to tl_recents (defined in lists.R), which is all terms from Fall 2021. Going back several years helps evaluate the accuracy of forecasting for each course. 

 

 

How do I forecast for a certain course and term? 

Rscript cedar.R -f forecast -t 202560 -c 'ENGL 1120' 

 

How do I forecast for just summers (or falls or springs)? 

Rscript cedar.R -f forecast -c 'ENGL 1120' -t tl_summers (or tl_falls or tl_springs) 

 

 

How do I forecast for a small set of courses? 

Rscript cedar.R -f forecast -t 202560 -c 'ENGL 1110,ENGL 1120' 

 

How do I forecast for an arbitrary  set of courses? 

use basic course filtering and reporting functionality (via enrl) to create a CSV file, which is a list of courses (these go in your output/csv folder. 

 forecast with -f forecast -c NAME_OF_CSV_FILE.csv (with an optional term parameter) 
 

Rscript cedar.R -f enrl -d ECON --uel -l lower --output csv 

Rscript cedar.R -f forecast -c enrollments.csv -t 202510 

 

 

How can I forecast for other terms for courses already in the forecast table? 

Set the course parameter to "forecasts". Optionally specify whatever term (or term list) you want to forecast for. 

Rscript cedar.R -f forecast -c forecasts -t tl_falls 

 

 

How do I forecast with a conduit term that isn’t the term immediately before the target term?  

This is useful, for instance, if forecasting for summer in October before spring registration starts). Indicate the desired conduit term and the target term it should be used with as a comma-separated pair. For instance, if you want to forecast summer 2025 based on Fall 2024 data (rather than Spring 2025 data as is default behavior): 

Rscript cedar.R -f forecast -c 'MATH 1220' --forecast_conduit_term 202480,202560 

 

 

 

FORECAST REPORT RECIPES 

How do I view all forecasting results? 

This is often too much information, but if you want to see all current forecast data, for ALL courses and terms ever forecasted: 

Rscript cedar.R -f forecast-report 

The --output csv flag will create two CSV files in the output/csv folder:  

forecast-short.csv provides an executive summary view of forecasting stats 

forecast-long.csv shows all the accuracy and evaluation calculations  

 

How can I see forecast data for specific terms and/or courses? 

Use the standard filtering flags by course AND/OR term OR term type: 

Rscript cedar.R -f forecast-report -c 'MATH 1215' 

Rscript cedar.R -f forecast-report -t 202510 

Rscript cedar.R -f forecast-report -t fall 

Rscript cedar.R -f forecast-report -c ‘ENGL 1120’ -t summer 

 

How can I get an HTML file of forecast data to share? 

Rscript cedar.R -f forecast-report -c 'MATH 1215' -output html  

 

How can I get an ASPX file of forecast data to share via OneDrive? 

Rscript cedar.R -f forecast-report -t summer --output aspx 

 

How can I get a CSV file of forecast data? 

Rscript cedar.R -f forecast-report -c 'MATH 1215' --output csv 

Rscript cedar.R -f forecast-report -t 202560 --output csv 

 

What does a zero mean for a forecast? 

It usually means that course wasn't offered in the previous term_type 

 

Why are there so many NAs for recommendations? 

Usually because there isn't enough data for the term_type. For instance, if you're trying to forecast for 202560, but the accuracy data and recommendations are all NAs, try forecasting for earlier summers with: 

Rscript cedar.R -f forecast -c forecasts -t tl_summers 

Then, rerun forecast report with the term flag set to summer to see all summer data: 

Rscript cedar.R -f forecast-report -t summer 

 

What are all the forecast-report columns? 

enrolled: MyReports final enrollment for that semester (all registered students) 

cl_total: total number of students who registered for the course, including all drops (theory: the sheer number of registrations tell us something about demand apart from final or even census enrollments)  

de_mean: mean number of drops before census date (de = drops_early) 

dl_mean: mean number of drops after census date (de = drops_late) 

conduit: forecasted enrollment from the “conduit” method 

major: forecasted enrollment from the “major” method 

_accr columns: how accurate the forecast was for terms with enrollment (with respect to enrl or cl enrollment totals) 

avg_ columns: the average accuracy (with respect to enrl or cl enrollment totals) 

pref_enrl_method: the method that seems to be most accurate with respect to final enrollment numbers 

pref_cl_method: the method that seems to be most accurate with respect to the total number of registrations 

rec_sections: the computed number of sections needed based on the number of forecasted students indicated by pref_method, and what seems to be the section cap size from last term type  

diff_fr_prev:  difference in number of sections compared to a year prior. 

 

 

 

REGSTATS 

One challenge in forecasting is knowing what courses to forecast. One approach is to forecast courses that have been offered in the previously for that term type. But that won’t catch courses that have accumulated an unmet demand, or courses that normally follow a course that has unusually high enrollment. 

Regstats reports registration statistics for a course. It is particularly useful (and primarily used) during forecasting for identifying courses with bumps or dips in enrollment, and/or student drops that are outside their standard deviation. It also looks at “squeezes”, where the number of seats at the end of the term is significantly less than what we’d expect from regular attrition. Recognizing when courses are outside their normal behavior helps identify courses to forecast (and inspect via course reports). 

Forecasting uses the regstats tool to identify courses that have higher than expected drops or enrollment bumps or dips, or courses with high waitlists. These courses, or courses students take afterward, may need extra scrutiny.  

Use regstats to create a list of courses, specifying a term or two PRIOR to what you will forecast for (to forecast for summer, run regstats or 202510 (and/or 202480): 

Rscript cedar.R -f regstats –t 202480,202510 

Note that this defaults to lower-division A& courses, unless otherwise specified 

 

This creates a series of files in the output/csv folder: 

bumps.csv: Courses with enrollment bumps  

after_bumps.csv: the top 10 courses students take after "bump" courses; useful for deciding whether some might be offered in summer, for instance. 

dips.csv: Enrollment dips and the courses students take afterward (we might not need as many) 

drops.csv: Courses with an unusually high number of drops, since we might need more of those courses 

waits.csv: Courses with long wait lists, where a minimum number of students is defined in config.R 

all_flagged_courses: all courses gathered into a master list for convenience 

Then, use the forecast function and set the -c flag to all_flagged_courses.csv. For instance, to forecast for summer 2025: 

Rscript cedar.R -f forecast -c all_flagged_courses.csv -t 202560 

AGAIN: just forecasting for summer won't tell us about NEW courses that might be needed (but regstats and forecast-report will give you ideas!) 

 

Thresholds 

Regstat’s primary function is to identify courses of interest. To do so, it needs thresholds of what constitutes a significant deviation from the norm. Here are the thresholds set in config.R and can be adjusted as need be. 

sd_buffer: % of standard deviation before flagging as concern. Default: 1 (meaning use exactly the calculated SD) 

min_count: min number of students enrolled before we flag as a concern. Default: 20 

min_impacted: min difference b/w enrollment and mean (= number of students affected). Default: 6 

min_pct_sd: percent of students outside the mean compared to standard deviation. Z = (mean – count) / sd. Default: 1.2 

min_squeeze: squeeze is the ratio of avail seats to drops after census. Default: .5 (determined by general inspection).  

min_wait: minimum number of students on a wait list before course is flagged. Default: 20 

section_proximity: how close to integer before rounding up/down for recommended sections? closer to .5 reduces -100s: Default: .3 (set closer to .5 for more rounding; set closer to 0 for no rounding. 

 

 

What do the regstats columns mean? 

count: total enrollment for the course 

pct_sd: percent of students outside the mean affected compared to standard deviation. Computed as (mean – count) / sd 

impacted: a rough count of how many students outside the mean are in the bump/dip/etc. This is to filter out courses in which not many students are being affected.  

 

How can I share regstats output: 

As with all reporting tools, to generate an HTML or ASPX report, use --output [csv/html/aspx]. If you don’t specify a format, the default is html. Files are saved to output/regstats-reports 

Specify csv to create the series of CSV files for each category (dips/bumps/drops/etc) in the output/csv folder. Files are saved to output/csv 

Rscript cedar.R -f regstats -t 202510 --output aspx 

 

How can I automatically generate course reports for courses identified by regstats? 

If you want to generate course reports based on the courses identified by regstats, you can use –f course-report -c forecasts -t TERM or TERM_TYPE. This will generate course reports for all courses listed in the forecast_data table that have forecasts for the specified TERM. 

 

