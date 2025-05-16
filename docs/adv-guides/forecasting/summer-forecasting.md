---
title: Summer Forecasts
layout: home
parent: Forecasting
nav_order: 15
---

# Summer Forecasting

## Data updates 
Before forecasting, it may be helpful to update DESR and Class List data from MyReports. See the [data page](data) for details on how to do this.

Forecasting can only really estimate enrollments for courses we've offered in previous term types. Forecasting a list of relevant course from the previous term type is a good start to see if demand has increased. Run these commands to generate a list of courses from last summer and then forecast for them 

`Rscript cedar.R -f enrl -t 202460 --uel -l lower --college AS -a course --output csv` 

`Rscript cedar.R -f forecast -c enrollments.csv -t 202560` 

 

IMPORTANT REMINDER: The regstats reports are good indicators of NEW courses that might enroll well, even for summer, and likely better show impacts of new scholarship rules. 


Create a regstats report to see what courses are being flagged (based on threshold filtering for enrollment/drop anomolies) in the term(s) PRIOR to the term you want to forecast for: 

`Rscript cedar.R -f regstats -t 202510 --output csv` 

This creates a group of CSV files in the output/csv folder, in case you want to handle them separately, but... 

All "flagged" courses (meaning they seem to deviate from their normal enrollments or drops, etc) are combined in all_flagged_courses.csv 

You can also specify--output html or --output aspx if you want non-CSV files to share 

 

To forecast for flagged courses (via the CSV file created above): 

`Rscript cedar.R -f forecast -c all_flagged_courses.csv -t 202560`  

This creates two CSV files: output/csv/forecasts-short.csv 
 

Use the forecast-report function with filtering to display forecasts of interest (like all summer forecasts), for determining needed sections 

- `Rscript cedar.R -f forecast-report -t summer --output csv` 
- `Rscript cedar.R -f forecast-report -c ENGL 1110 --output html` 
 
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
